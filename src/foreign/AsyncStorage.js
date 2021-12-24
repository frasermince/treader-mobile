var AsyncStorage = require('@react-native-async-storage/async-storage');
exports._clear = function() {
  return AsyncStorage.default.clear();
}
exports._setItem = function(key) {
  return function (value) {
    return function () {
      console.log("***ASYNCSTORAGE", AsyncStorage)
      return AsyncStorage.default.setItem(key, value);
    }
  }
}
exports._getItem = function(key) {
  return function () {
    return AsyncStorage.default.getItem(key);
  }
}

exports._removeItem = function(key) {
  return function () {
    return AsyncStorage.default.removeItem(key);
  }
}
