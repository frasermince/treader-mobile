var RNFS = require('react-native-fs');
exports.bookDir = RNFS.DocumentDirectoryPath + "/www";
exports._exists = function(path) {
  return function () {
    return RNFS.exists(path);
  }
}
exports._readDirectory = function(path) {
  return function () {
    console.log("FOREIGN DIRECTORY", path);
    return RNFS.readDir(path);
  }
}
