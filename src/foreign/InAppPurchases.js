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
  return function () {
    return module.purchaseUpdatedListener(fn);
  }
}

exports._purchaseErrorListener = function(fn) {
  return function () {
    return module.purchaseErrorListener(fn);
  }
}


exports._finishTransaction = function(id) {
  return function () {
    return module.finishTransaction(id, false);
  }
}

