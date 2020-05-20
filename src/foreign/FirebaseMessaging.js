var messaging = require('@react-native-firebase/messaging');

exports._requestPermission = function() {
  return messaging.default().requestPermission();
}
