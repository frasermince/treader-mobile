const fs = require('fs');
const path = require('path');

process.env.CUSTOM_BRIDGE = fs.readFileSync(path.resolve(__dirname, "src/bridge.js"), "utf8");
process.env.EPUBJS = fs.readFileSync(path.resolve(__dirname, "node_modules/epubjs/dist/epub.js"), "utf8");
console.log("env", process.env);

module.exports = {
  "presets": ["module:metro-react-native-babel-preset"],
  "plugins": [
    ["module-resolver", {
      "alias": {
        "stream": "stream-browserify",
        "path": "path-webpack"
      }
    }],
    "transform-inline-environment-variables"
  ]
}
