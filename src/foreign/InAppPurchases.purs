module InAppPurchases where
import Prelude
import Control.Promise as Promise
import Control.Promise (Promise)
import Effect.Class (liftEffect)
import Effect.Aff (Aff)
import Effect (Effect)
import Data.Nullable (toMaybe, Nullable)
import Data.Maybe (Maybe(..), fromMaybe)
import Effect.Uncurried (runEffectFn1, EffectFn1)

type Purchase = {transactionReceipt :: Nullable String, transactionId :: String}
type Product = {productId :: String}
foreign import _getSubscriptions :: Array String -> Effect (Promise (Nullable (Array Product)))
foreign import _requestSubscription :: String -> Boolean -> Effect (Promise Unit)
foreign import _purchaseUpdatedListener :: EffectFn1 (Purchase -> Effect (Promise Unit)) Unit
foreign import _finishTransactionIOS :: String -> Effect (Promise Unit)

getSubscriptions :: Array String -> Aff (Array Product)
getSubscriptions x = fromMaybe [] <$> toMaybe <$> (liftEffect (_getSubscriptions x) >>= Promise.toAff)

requestSubscription :: String -> Boolean -> Aff Unit
requestSubscription x y = (liftEffect (_requestSubscription x y) >>= Promise.toAff)

purchaseUpdatedListener :: (Purchase -> Aff Unit) -> Effect Unit
purchaseUpdatedListener fn = runEffectFn1 _purchaseUpdatedListener affFn
  where affFn purchase = Promise.fromAff $ fn purchase

finishTransactionIOS :: String -> Aff Unit
finishTransactionIOS x = (liftEffect (_finishTransactionIOS x) >>= Promise.toAff)
