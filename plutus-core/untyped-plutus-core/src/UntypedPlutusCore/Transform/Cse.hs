{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TupleSections        #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

module UntypedPlutusCore.Transform.Cse (cse) where

import PlutusCore (MonadQuote, Name, freshName)
import PlutusCore.MkPlc
import Universe
import UntypedPlutusCore.Core
import UntypedPlutusCore.Purity (isWorkFree)
import UntypedPlutusCore.Size (termSize)

import Control.Lens
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State.Strict
import Data.Foldable
import Data.Hashable
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as Map
import Data.List.Extra
import Data.Traversable
import GHC.Generics

{-

\x y -> (1+(2+x))
        +
        (case y [ (1+(2+x)) + (3+x)
                , (2+x) + (3+x)
                , 4+x
                ]
        )

In the first pass, we assign a unique ID to each LamAbs, Delay and Case branch.
Then, we annotate each subterm with a path, consisting of IDs encountered from the root
to that subterm.

In the above example, the ID of `\x` is 0, the ID of `\y` is 1, and the IDs of the
three case branches are 2, 3, 4 (the actual numbers don't matter, as long as they are unique).
The path for the first `1+(2+x)` and the first `2+x` is "0.1"; the path for the second
`1+(2+x)` and the second `2+x` is "0.1.2"; and the path for `4+x` is "0.1.4".

In the second pass, we calculate a count for each `(term, path)` pair, where `term` is a
non-workfree term, and `path` is its path. If the same term has two paths, and one is an
ancestor (i.e., prefix) of the other, we increment the count for the ancestor path in both
instances.

In the above example, there are three occurrences of `2+x`,
whose paths are "0.1", "0.1.2" and "0.1.3". The first path is an ancestor of the latter two.
Therefore the count for `(2+x, "0.1")` is 3, while the count for `(2+x, "0.1.2")` and
`(2+x, "0.1.3")` is 0. The following all have a count of 1: `(3+x, "0.1.2")`,
`(3+x, "0.1.3")` and `(4+x, "0.1.4")`.

Now, each `(term, path)` pair whose count is greater than 1 is a CSE candidate.
In the above example, the CSE candidates are `(2+x, "0.1")` and `(1+(2+x), "0.1")`.
Note that `3+x` is not a CSE candidate.

The CSE candidates are processed in descending order of their `termSize`s. For each CSE
candidate, we generate a fresh variable, create a LamAbs for it under its path, and substitute
it for all occurrences in the original term whose paths are descendents (or self) of
the candidate's path.

In the above example, we first process CSE candidate `(1+(2+x), "0.1")`. We create a fresh
variable `cse1` for it. After substitution, the original term becomes

\x y -> (\cse1 -> cse1
                  +
                  (case y [ cse1 + (3+x)
                  , (2+x) + (3+x)
                  , 4+x
                  ]
        ) (1+(2+x))

The second CSE candidate is processed similarly, and the final result is

\x y -> (\cse2 -> (\cse1 -> cse1
                            +
                            (case y [ cse1 + (3+x)
                            , cse2 + (3+x)
                            , 4+x
                            ]
                  ) (1+cse2)
        ) (2+x)

-}

-- | In reverse order, e.g., "1.2.3" is `[3, 2, 1]`.
type Path = [Int]

isAncestorOrSelf :: Path -> Path -> Bool
isAncestorOrSelf = isSuffixOf


newtype AnnotatedTerm uni fun ann = AnnotatedTerm
  { unAnnotatedTerm :: Term Name uni fun (Path, ann) }
  deriving stock Generic

instance Eq (Term Name uni fun ann) => Eq (AnnotatedTerm uni fun ann) where
  AnnotatedTerm t == AnnotatedTerm t' = fmap snd t == fmap snd t'

instance Eq (Term Name uni fun ann) => Hashable (AnnotatedTerm uni fun ann) where
  hashWithSalt = undefined
  hash = undefined

termPath :: AnnotatedTerm uni fun ann -> Path
termPath = fst . termAnn . unAnnotatedTerm

data CseCandidate uni fun ann = CseCandidate
  { ccFreshName :: Name
  , ccTerm      :: AnnotatedTerm uni fun ann
  }

cse ::
  (MonadQuote m, Eq (Term name uni fun ann)) =>
  Term name uni fun ann ->
  m (Term name uni fun ann)
cse t = undefined

-- | The first pass. See Note
annotate :: forall uni fun ann. Term Name uni fun ann -> AnnotatedTerm uni fun ann
annotate = AnnotatedTerm . flip evalState 0 . flip runReaderT [] . go
  where
    -- The integer state is the highest ID assigned so far.
    -- The reader context is the current path.
    go :: forall name. Term name uni fun ann
      -> ReaderT Path (State Int) (Term name uni fun (Path, ann))
    go t = do
      path <- ask
      case t of
        Apply ann fun arg -> Apply (path, ann) <$> go fun <*> go arg
        Force ann body -> Force (path, ann) <$> go body
        Constr ann i args -> Constr (path, ann) i <$> traverse go args
        Constant ann val -> pure $ Constant (path, ann) val
        Error ann -> pure $ Error (path, ann)
        Builtin ann fun -> pure $ Builtin (path, ann) fun
        Var ann name -> pure $ Var (path, ann) name
        LamAbs ann n body -> do
          freshId <- (+ 1) <$> lift get
          lift $ put freshId
          LamAbs (path, ann) n <$> local (freshId :) (go body)
        Delay ann body -> do
          freshId <- (+ 1) <$> lift get
          lift $ put freshId
          Delay (path, ann) <$> local (freshId :) (go body)
        Case ann scrut branches ->
          Case (path, ann)
            <$> go scrut
            <*> ( for branches $ \br -> do
                    freshId <- (+ 1) <$> lift get
                    lift $ put freshId
                    local (freshId :) (go br)
                )

-- | The second pass. See Note
collectEvaluations ::
  AnnotatedTerm uni fun ann ->
  HashMap (AnnotatedTerm uni fun ann) (HashMap Path Int)
collectEvaluations =
  foldrOf termSubtermsDeep (addToMap . AnnotatedTerm) Map.empty . unAnnotatedTerm

addToMap ::
  AnnotatedTerm uni fun ann ->
  HashMap (AnnotatedTerm uni fun ann) (HashMap Path Int) ->
  HashMap (AnnotatedTerm uni fun ann) (HashMap Path Int)
addToMap t hm
  -- We don't consider work-free terms for CSE, because doing so may or may not
  -- have a size benefit, but certainly doesn't have any cost benefit (the cost
  -- will in fact be slightly higher due to the additional application).
  | isWorkFree (unAnnotatedTerm t) = hm
  | otherwise = case Map.lookup t hm of
      Nothing    -> Map.insert t (Map.singleton (termPath t) 1) hm
      Just paths -> undefined
      -- Map.alter
      --   ( \case
      --       Nothing -> Just $ Map.singleton (termPath t) 1
      --       Just paths -> Just $ combinePaths (termPath t) paths
      --   )
      --   t

-- | Combine a new path with a number of existing (path, count) pairs.
combinePaths :: Path -> HashMap Path Int -> HashMap Path Int
combinePaths path = Map.fromListWith (+) . go . Map.toList
  where
    go :: [(Path, Int)] -> [(Path, Int)]
    -- The new path is not a descendent-or-self of any existing path.
    go [] = [(path, 1)]
    go ((path', cnt) : paths)
      -- The new path is an ancestor-or-self of an existing path.
      -- Take over all counts of the existing path, remove the existing path,
      -- and continue.
      | path `isAncestorOrSelf` path' = (path, cnt) : go paths
      -- The new path is a descendent-or-self of an existing path.
      -- Increment the count for the existing path. There can only be one such
      -- existing path, so we can stop here.
      | path' `isSuffixOf` path = (path', cnt + 1) : paths
      | otherwise = (path', cnt) : go paths

mkCseTerm ::
  forall uni fun ann m.
  (MonadQuote m, Eq (Term Name uni fun ann)) =>
  -- value: all paths of the term that need cse
  -- the paths should not contain each other
  HashMap (AnnotatedTerm uni fun ann) [Path] ->
  Term Name uni fun (Path, ann) ->
  m (Term Name uni fun ann)
mkCseTerm hm t = do
  cs <-
    traverse (uncurry mkCseCandidate)
      . concatMap (uncurry $ fmap . (,))
      . sortOn (negate . termSize . fst)
      $ Map.toList hm
  pure . fmap snd $ foldl' (flip applyCse) t cs

applyCse :: CseCandidate uni fun ann -> Term Name uni fun (Path, ann)
  ->  Term Name uni fun (Path, ann)
applyCse c = transformOf termSubterms subst
  where
    subst t0 = if t == ccTerm c && ccPath c `isAncestorOrSelf` path
      then undefined
      else undefined
      where
        t = snd <$> t0
        path = fst (termAnn t)

mkCseCandidate ::
  forall uni fun ann m.
  (MonadQuote m, Eq (Term Name uni fun ann)) =>
  AnnotatedTerm uni fun ann ->
  Path ->
  m (CseCandidate uni fun ann)
mkCseCandidate term path = CseCandidate <$> freshName "cse" <*> pure term <*> pure path
