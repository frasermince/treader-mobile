var epubRn = require "epubjs-rn";

var streamer = new epubRn.Streamer();
exports.epub = epubRn.Epub;
exports._startStream = streamer.start;
exports._streamGet = streamer.get;
exports.killStream = streamer.kill;
