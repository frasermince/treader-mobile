var epubRn = require("epubjs-rn");
var epubjs = require("epubjs");

exports.toCfi = function(cfi) {
  return new epubjs.EpubCFI(cfi)
}

exports._compare = function(first) {
  return function(second) {
    return first.compare(first, second);
  }
}

exports._createStreamer = function() {
  return new epubRn.Streamer();
}
exports.epub = epubRn.Epub;
exports._startStream = function(streamer) {
  return function() {
    return streamer.start();
  }
}
exports._streamGet = function(streamer) {
  return function (string) {
    return function() {
      console.log("Here", string);
      return streamer.get(string);
    }
  }
}
exports._killStream = function(streamer) {
  return streamer.kill;
}
