var module = require('react-native-iap');

exports._getSubscriptions = function(skus) {
  return function () {
    debugger
    return module.getSubscriptions(skus);
  }
}

exports._requestSubscription = function(sku) {
  return function (dangerouslyFinish) {
    return function () {
      debugger
      return module.requestSubscription(sku, dangerouslyFinish);
    }
  }
}

exports._purchaseUpdatedListener = function(fn) {
  return function () {
    debugger
    return module.purchaseUpdatedListener(fn);
  }
}

exports._purchaseErrorListener = function(fn) {
  return function () {
    debugger
    return module.purchaseErrorListener(fn);
  }
}


exports._finishTransactionIOS = function(id) {
  return function () {
    debugger
    return module.finishTransactionIOS(id);
  }
}

