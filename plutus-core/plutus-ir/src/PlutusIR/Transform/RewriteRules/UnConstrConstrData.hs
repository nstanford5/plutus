-- editorconfig-checker-disable-file
{-# LANGUAGE GADTs         #-}
{-# LANGUAGE TypeOperators #-}
module PlutusIR.Transform.RewriteRules.UnConstrConstrData
    ( unConstrConstrData
    ) where

import PlutusCore.Default
import PlutusCore.Quote
import PlutusIR
import PlutusIR.Analysis.Builtins
import PlutusIR.Analysis.VarInfo
import PlutusIR.Transform.RewriteRules.Common

{- | This rule rewrites terms of form `BUILTIN(unConstrData(constrData(x,y)))`
, where builtin stands for `FstPair` or `SndPair`, to "x" or "y" respectively.

This rewrite-rule was originally meant to rewrite `unConstrData(constrData(x,y)) => (x,y)`,
however we do not have a (polymorphic or monomorphic) builtin constructor to create a `BuiltinPair`
"(x,y)". See note [Representable built-in functions over polymorphic built-in types].

So we adapted the original rewrite rule to try to achieve a similar goal.
Unfortunately, the adapted rule is less applicable and will most likely never fire
(at least for PIR code generated by plutus-tx).
The reason is that the TH code in plutus-tx does not create such "tight" code, but uses
way more lets that get in the way preventing the rule from firing.

Possible solutions: Some more aggressive PIR inlining, rewriting the PlutusTx TH code, or
introducing specialised pattern-matching builtins as last resort.
Plutus Tx TH code responsible:
<https://github.com/IntersectMBO/plutus/blob/9364099b38e3aa27fb311af3299d2210e7e33e45/plutus-tx/src/PlutusTx/IsData/TH.hs#L174-L192>
-}
unConstrConstrData :: (MonadQuote m, t ~ Term tyname Name DefaultUni DefaultFun a, Monoid a)
                   => BuiltinsInfo DefaultUni DefaultFun
                   -> VarsInfo tyname Name DefaultUni a
                   -> t
                   -> m t
unConstrConstrData binfo vinfo t = case t of
    -- This rule might as well have been split into two separate rules, but kept as one
    -- so as to reuse most of the matching pattern.

    -- builtin({t1}, {t2}, unConstr(constrData(i, data)))
    (A (I (I (B builtin) tyFst) tySnd)
     (A (B UnConstrData) (A (A (B ConstrData) arg1) arg2))) ->
        case builtin of
            -- sndPair({t1}, {t2}, unConstr(constrData(i, data))) = i `seq` data
            SndPair -> (tyFst,arg1) `seQ` arg2

            -- fstPair({t1}, {t2}, unConstr(constrData(i, data))) = let !gen = i in data `seq` gen
            FstPair ->  do
                  (genVar, genLetIn) <- mkFreshTermLet tyFst arg1
                  genLetIn <$>
                      (tySnd, arg2) `seQ` genVar
            _ -> pure t
    _ -> pure t

  where
      infixr 5 `seQ` -- 5 so it has more precedence than <$>
      seQ = seqP binfo vinfo
