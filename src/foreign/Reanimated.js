var reanimate = require('react-native-reanimated');
const React = require("react");

const useEqCache = (eq, a) => {
  const memoRef = React.useRef(a);
  if (memoRef.current !== a && !eq(memoRef.current, a)) {
    memoRef.current = a;
  }
  return memoRef.current;
};

exports.interpolate = function(x) {
  console.log("***X", x)
  return function(input) {
    console.log("INPUT", input)
    return function(output) {
      console.log("OUTPUT", output)
      return function(type) {
        console.log("SHARED INPUT", type)
        return reanimate.interpolate(x, input, output, type);
      }
    }
  }
}

exports._useSharedValue = function(tuple, a) {
  console.log("REANIMAE", reanimate)
  console.log("***STARTING VALUE", a);
	var value = reanimate.useSharedValue(a);
  var update = function(newValue) {
    return function() {
      console.log("***NEW VALUE", newValue);
      value = newValue;
    }
  }
  return tuple(value, update);
}

exports._useAnimatedStyle = function (eq, deps, styles) {
  console.log("***LEVEL UP", eq, deps);
  console.log("***DEPS", eq, deps, styles);
  const memoizedKey = useEqCache(eq, deps);
  return reanimate.useAnimatedStyle(styles, [memoizedKey]);
}

exports._view = reanimate.default.View