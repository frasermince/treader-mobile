var native = require('react-native');

exports._openUrl = function(s) {
  return native.Linking.openURL(s);
}
