{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies     #-}
{-# LANGUAGE TypeOperators    #-}

module PlutusTx.Evaluation
    ( evaluateCek
    , unsafeEvaluateCek
    , evaluateCekTrace
    , ErrorWithCause(..)
    , EvaluationError(..)
    , CekExTally
    , TallyingSt(..)
    , CekEvaluationException
    )
where

import PlutusCore qualified as PLC
import PlutusCore.Default
import PlutusCore.Name

import UntypedPlutusCore
import UntypedPlutusCore.Evaluation.Machine.Cek hiding (evaluateCek, unsafeEvaluateCek)
import UntypedPlutusCore.Evaluation.Machine.Cek qualified as Cek

import Control.Monad.Except
import Data.Text (Text)

-- We do not use qualified import because the whole module contains off-chain code
import Prelude as Haskell

-- TODO: remove most of these functions

-- | Evaluate a program in the CEK machine with the usual text dynamic builtins.
evaluateCek
    :: (uni ~ DefaultUni, fun ~ DefaultFun)
    => Program Name uni fun () -> Either (CekEvaluationException uni fun) (Term Name uni fun ())
evaluateCek (Program _ _ t) = Cek.evaluateCekNoEmit PLC.defaultCekParameters t

-- | Evaluate a program in the CEK machine with the usual text dynamic builtins. May throw.
unsafeEvaluateCek
    :: (uni ~ DefaultUni, fun ~ DefaultFun)
    => Program Name uni fun () -> EvaluationResult (Term Name uni fun ())
unsafeEvaluateCek (Program _ _ t) = Cek.unsafeEvaluateCekNoEmit PLC.defaultCekParameters t

-- | Evaluate a program in the CEK machine with the usual text dynamic builtins and tracing, additionally
-- returning the trace output.
evaluateCekTrace
    :: (uni ~ DefaultUni, fun ~ DefaultFun)
    => Program Name uni fun ()
    -> ([Text], TallyingSt fun, Either (CekEvaluationException uni fun) (Term Name uni fun ()))
evaluateCekTrace (Program _ _ t) =
    case runExcept @FreeVariableError $ deBruijnTerm t of
        Left _ -> (mempty, mempty, error "freevarT")
        Right dbt ->
            -- Don't use 'let': https://github.com/input-output-hk/plutus/issues/3876
            case runCek PLC.defaultCekParameters Cek.tallying Cek.logEmitter dbt of
                -- translating back the output
                (res, st, logs) -> (logs, st, unDeBruijnResult res)
