var RNFS = require('react-native-fs');
var RNFetchBlob = require('rn-fetch-blob').default
var absinthe = require('apollo-absinthe-upload-link')
let dirs = RNFetchBlob.fs.dirs
exports.bookDir = dirs.DocumentDir + "/www";
exports.audioDir = dirs.DocumentDir;
exports.audioBookDir = dirs.DocumentDir + "/audiobooks";
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

exports._mkdir = function(path) {
  return function(options) {
    return function() {
      return RNFS.mkdir(path, options);
    }
  }
}

exports._unlink = function(path) {
  return function() {
    return RNFS.unlink(path);
  }
}

exports._writeFile = function(path) {
  return function(data) {
    return function(encoding) {
      return function() {
        return RNFS.writeFile(path, data, encoding);
      }
    }
  }
}

exports._readFile = function(path) {
  return function(encoding) {
    return function() {
      return RNFS.readFile(path, encoding);
    }
  }
}

exports.absintheFile = function(a) {
  return new absinthe.ReactNativeFile(a);
}
