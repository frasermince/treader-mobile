var module = require('react-native-iap');

exports._getSubscriptions = function(skus) {
  return function () {
    return module.getSubscriptions(skus);
  }
}

exports._requestSubscription = function(sku) {
  return function (dangerouslyFinish) {
    return function () {
      return module.requestSubscription(sku, dangerouslyFinish);
    }
  }
}

exports._purchaseUpdatedListener = function(fn) {
  console.log("P FN", module.purchaseUpdatedListener);
  return module.purchaseUpdatedListener(fn);
}

exports._purchaseErrorListener = function(fn) {
  console.log("ERROR FN", module.purchaseErrorListener);
  return module.purchaseErrorListener(fn);
}


exports._finishTransactionIOS = function(id) {
  return function () {
    return module.finishTransactionIOS(id);
  }
}

