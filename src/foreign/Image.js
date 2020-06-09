var fastImage = require('react-native-fast-image');

exports._image = fastImage.default;

exports.contain = fastImage.default.resizeMode.contain

exports.cover = fastImage.default.resizeMode.cover
