module AppM (AppM, runAppM) where

-- | our monad is just Aff with Navigation attached to it


import Prelude
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect, liftEffect)
import Control.Monad.Reader.Trans (class MonadAsk, ReaderT, asks, runReaderT)
import Routing.Duplex (print)
import Routing.Hash (setHash)
import TuneBank.Navigation.Route (routeCodec)
import TuneBank.Navigation.Navigate (class Navigate)
import TuneBank.Data.Types (Env)
import Type.Equality (class TypeEquals, from)

newtype AppM a = AppM (ReaderT Env Aff a)

derive newtype instance functorAppM :: Functor AppM
derive newtype instance applyAppM :: Apply AppM
derive newtype instance applicativeAppM :: Applicative AppM
derive newtype instance bindAppM :: Bind AppM
derive newtype instance monadAppM :: Monad AppM
derive newtype instance monadEffectAppM :: MonadEffect AppM
derive newtype instance monadAffAppM :: MonadAff AppM

-- | The first instance we'll implement is a little funky. We can't write instances for type
-- | synonyms, and we defined our environment (`Env`) as a type synonym for convenience. To get
-- | around this, we can use `TypeEquals` to assert that types `a` and `b` are in fact the same.
-- |
-- | In our case, we'll write a `MonadAsk` (an alternate name for `Reader`) instance for the type
-- | `e`, and assert it is our `Env` type. This is how we can write a type class instance for a
-- | type synonym, which is otherwise disallowed.
-- |
-- | With this instance, any monad `m` with the `MonadAsk Env m` constraint can read from the
-- | environment we defined. This is done with the `ask` function. For example:
-- |
-- | ```purescript
-- | toggleLogLevel :: forall m. MonadAsk Env m => m LogLevel
-- | toggleLogLevel = do
-- |   env <- ask
-- |  if env.logLevel == Dev then Prod else Dev
-- | ```
instance monadAskAppM :: TypeEquals e Env => MonadAsk e AppM where
  ask = AppM $ asks from


instance navigateAppM :: Navigate AppM where
  navigate =
    liftEffect <<< setHash <<< print routeCodec

runAppM :: Env -> AppM ~> Aff
runAppM env (AppM m) = runReaderT m env
