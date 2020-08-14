var native = require('react-native');
exports.value = function (x) {
  return new native.Animated.Value(x);
}
exports._timing = function(animation) {
  return function (options) {
    return function (onError, onSuccess) {
      var req = native.Animated.timing(animation, options);
      req.start(function(result) {
        if (result.finished) {
          onSuccess(result);
        } else {
          console.log(result);
          //onError("Animation was halted");
        }
      });
      return function (cancelError, cancelerError, cancelerSuccess) {
        req.stop();
        cancelerSuccess();
      }
    }
  }
}
exports._view = native.Animated.View
exports._scrollView = native.Animated.ScrollView

exports.interpolate = function(union) {
  return function(animation) {
    return function(options) {
      animation.addListener((value) => {
        console.log(value);
      })
      return animation.interpolate(options);
    }
  }
}

exports.getNode = function (component) {
  return component.getNode();
}

exports.scrollTo = function (node, position) {
  return function (position) {
    return function () {
      return node.scrollTo({x: position});
    }
  }
}
