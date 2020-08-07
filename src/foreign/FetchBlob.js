var RNFetchBlob = require('rn-fetch-blob').default

exports._fetch = function(config) {
  return function (method) {
    return function (url) {
      return function (options) {
        return function () {
          return RNFetchBlob
            .config(config)
            .fetch(method, url, options);
        }
      }
    }
  }
}

exports._progress = function(promise) {
  return function(callback) {
    return function() {
      return promise.progress(callback);
    }
  }
}
