var RNFetchBlob = require('rn-fetch-blob').default

exports._fetch = function(method) {
  return function (url) {
    return function (options) {
      return function () {
        return RNFetchBlob
          .config({fileCache : true})
          .fetch(method, url, options);
      }
    }
  }
}
