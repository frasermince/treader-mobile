var rn = require('react-native')
exports._clear = function() {
  return rn.AsyncStorage.clear();
}
exports._setItem = function(key) {
  return function (value) {
    return function () {
      return rn.AsyncStorage.setItem(key, value);
    }
  }
}
exports._getItem = function(key) {
  return function () {
    return rn.AsyncStorage.getItem(key);
  }
}

exports._removeItem = function(key) {
  return function () {
    return rn.AsyncStorage.removeItem(key);
  }
}
