-- This is a simple state modelling library for use with Haskell
-- QuickCheck. For documentation, see the associated slides.

{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

module Language.Plutus.Contract.Test.StateModel(
    StateModel(..)
  , Any(..)
  , Step(..)
  , LookUp, Var(..) -- we export the constructors so that users can construct test cases
  , Script(..)
  , stateAfter
  , runScript
  , runScriptInState
) where

import           Data.Typeable

import           Test.QuickCheck         as QC
import           Test.QuickCheck.Monadic

class (forall a. Show (Action state a),
       Monad (ActionMonad state),
       Show state,
       Typeable state) =>
        StateModel state where
  data Action state a
  type ActionMonad state :: * -> *
  actionName      :: Action state a -> String
  actionName = head . words . show
  arbitraryAction :: state -> Gen (Any (Action state))
  shrinkAction    :: (Typeable a, Show a) => state -> Action state a -> [Any (Action state)]
  shrinkAction _ _ = []
  initialState    :: state
  nextState       :: state -> Action state a -> Var a -> state
  nextState s _ _ = s
  precondition    :: state -> Action state a -> Bool
  precondition _ _ = True
  perform         :: state -> Action state a -> LookUp -> ActionMonad state a
  perform _ _ _ = return undefined
  postcondition   :: state -> Action state a -> LookUp -> a -> Bool
  postcondition _ _ _ _ = True
  monitoring      :: (state,state) -> Action state a -> LookUp -> a -> Property -> Property
  monitoring _ _ _ _ = id

type LookUp = forall a. Typeable a => Var a -> a

type Env = [EnvEntry]

data EnvEntry where
  (:==) :: (Show a, Typeable a) => Var a -> a -> EnvEntry

infix 5 :==

deriving instance Show EnvEntry

lookUpVar :: Typeable a => Env -> Var a -> a
lookUpVar [] v = error $ "Variable "++show v++" is not bound!"
lookUpVar ((v' :== a) : env) v =
  case cast (v',a) of
    Just (v'',a') | v==v'' -> a'
    _                      -> lookUpVar env v

data Any f where
  Some :: (Show a, Typeable a, Eq (f a)) => f a -> Any f

deriving instance (forall a. Show (Action state a)) => Show (Any (Action state))

instance Eq (Any f) where
  Some (a :: f a) == Some (b :: f b) =
    case eqT @a @b of
      Just Refl -> a == b
      Nothing   -> False

data Step state where
  (:=) :: (Show a, Typeable a, Eq (Action state a), Typeable (Action state a), Show (Action state a)) =>
            Var a -> Action state a -> Step state

infix 5 :=

deriving instance (forall a. Show (Action state a)) => Show (Step state)

newtype Var a = Var Int
  deriving (Eq, Ord, Show, Typeable)

instance Eq (Step state) where
  (Var i := act) == (Var j := act') =
    (i==j) && Some act == Some act'

newtype Script state = Script [Step state]
  deriving Eq

instance (forall a. Show (Action state a)) => Show (Script state) where
  showsPrec d (Script as)
    | d>10      = ("("++).showsPrec 0 (Script as).(")"++)
    | null as   = ("Script []"++)
    | otherwise = (("Script \n [")++) .
                  foldr (.) (showsPrec 0 (last as) . ("]"++))
                    [showsPrec 0 a . (",\n  "++) | a <- init as]


instance (Typeable state, StateModel state) => Arbitrary (Script state) where
  arbitrary = Script <$> arbActions initialState 1
    where
      arbActions :: state -> Int -> Gen [Step state]
      arbActions s step = sized $ \n ->
        let w = n `div` 2 + 1 in
          frequency [(1, return []),
                     (w, do mact <- arbitraryAction s `suchThatMaybe`
                                      \(Some act) -> precondition s act
                            case mact of
                              Just (Some act) ->
                                ((Var step := act):) <$> arbActions (nextState s act (Var step)) (step+1)
                              Nothing ->
                                return [])]

  shrink (Script as) =
    map (Script . prune . map fst) (shrinkList shrinker (withStates as))
    where shrinker ((Var i := act),s) = [((Var i := act'),s) | Some act' <- shrinkAction s act]

prune :: StateModel state => [Step state] -> [Step state]
prune = loop initialState
  where loop _s [] = []
        loop s ((var := act):as)
          | precondition s act
            = (var := act):loop (nextState s act var) as
          | otherwise
            = loop s as


withStates :: StateModel state => [Step state] -> [(Step state,state)]
withStates = loop initialState
  where
    loop _s [] = []
    loop s ((var := act):as) =
      ((var := act),s):loop (nextState s act var) as

stateAfter :: StateModel state => Script state -> state
stateAfter (Script script) = loop initialState script
  where
    loop s []                  = s
    loop s ((var := act) : as) = loop (nextState s act var) as

runScript :: StateModel state =>
                Script state -> PropertyM (ActionMonad state) (state,Env)
runScript = runScriptInState initialState

runScriptInState :: StateModel state =>
                    state -> Script state -> PropertyM (ActionMonad state) (state,Env)
runScriptInState state (Script script) = loop state [] script
  where
    loop _s env [] = return (_s,reverse env)
    loop s env ((Var n := act):as) = do
      pre $ precondition s act
      ret <- run (perform s act (lookUpVar env))
      let name = actionName act
      monitor (tabulate "Actions" [name] . classify True ("contains "++name))
      monitor (counterexample ("Var "++show n++" := "++show act++" --> "++show ret))
      let s'   = nextState s act (Var n)
          env' = (Var n :== ret):env
      monitor (monitoring (s,s') act (lookUpVar env') ret)
      assert $ postcondition s act (lookUpVar env) ret
      loop s' env' as
