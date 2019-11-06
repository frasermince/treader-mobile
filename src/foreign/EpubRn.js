var epubRn = require("epubjs-rn");

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
