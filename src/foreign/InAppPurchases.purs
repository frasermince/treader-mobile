module InAppPurchases where
import Prelude
import Control.Promise as Promise
import Control.Promise (Promise)
import Effect.Class (liftEffect)
import Effect.Aff (Aff)
import Effect (Effect)
import Data.Nullable (toMaybe, Nullable)
import Data.Maybe (Maybe(..), fromMaybe)
import Debug.Trace (spy)
import Effect.Uncurried (runEffectFn1, EffectFn1, mkEffectFn1)

type Purchase = {transactionReceipt :: Nullable String, transactionId :: String}
type Product = {productId :: String}
foreign import _getSubscriptions :: Array String -> Effect (Promise (Nullable (Array Product)))
foreign import _requestSubscription :: String -> Boolean -> Effect (Promise Unit)
foreign import _purchaseUpdatedListener :: (EffectFn1 Purchase Unit) -> Effect Unit
foreign import _purchaseErrorListener :: (EffectFn1 {} Unit) -> Effect Unit
foreign import _finishTransaction :: Purchase -> Effect (Promise Unit)

getSubscriptions :: Array String -> Aff (Array Product)
getSubscriptions x = fromMaybe [] <$> toMaybe <$> (liftEffect (_getSubscriptions x) >>= Promise.toAff)

requestSubscription :: String -> Boolean -> Aff Unit
requestSubscription x y = (liftEffect (_requestSubscription x y) >>= Promise.toAff)

purchaseUpdatedListener :: (Purchase -> Effect Unit) -> Effect Unit
purchaseUpdatedListener fn = _purchaseUpdatedListener $ mkEffectFn1 fn

purchaseErrorListener :: ({} -> Effect Unit) -> Effect Unit
purchaseErrorListener fn = _purchaseErrorListener $ mkEffectFn1 fn

finishTransaction :: Purchase -> Aff Unit
finishTransaction x = (liftEffect (_finishTransaction x) >>= Promise.toAff)
