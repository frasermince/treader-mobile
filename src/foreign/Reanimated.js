var reanimate = require('react-native-reanimated');

exports.interpolate = function(union) {
  return function(animation) {
    return function(options) {
      return reanimate.interpolate(options);
    }
  }
}

exports.useSharedValue = function(a) {
  console.log("REANIMAE", reanimate)
	var value = reanimate.useSharedValue(a);
  var update = function(newValue) {
    value = newValue;
  }
  return [value, update]
}

exports.useAnimatedStyle = reanimate.useAnimatedStyle;