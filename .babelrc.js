const fs = require('fs');
const path = require('path');
const Bundler = require('parcel-bundler');

const html = fs.readFileSync(path.resolve(__dirname, "dist/index.html"), "utf8");
process.env.CUSTOM_BRIDGE = html

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
