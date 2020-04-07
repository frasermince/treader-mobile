var imageSearch = require('react-native-google-image-search');

exports._imageSearch = function(keyword) {
  return function (low) {
    return function (high) {
      return function () {
        return imageSearch.default(keyword, low, high);
      }
    }
  }
}
