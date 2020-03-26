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
  return module.purchaseUpdatedListener(fn);
}

exports._purchaseErrorListener = function(fn) {
  return module.purchaseErrorListener(fn);
}


exports._finishTransactionIOS = function(id) {
  return function () {
    return module.finishTransactionIOS(id);
  }
}

