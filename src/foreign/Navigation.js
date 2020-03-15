var navigation = require('@react-navigation/native');
var React = require("react");
var basicHooks = require("../../output/React.Basic.Hooks/foreign.js");

exports.useNavigation = navigation.useNavigation;
exports.useFocusEffect_ = function(eq, key, effect) {
  var memoizedKey = basicHooks.useEqCache_(eq, key);
  console.log("EFFECT FN", navigation);
  navigation.useFocusEffect(React.useCallback(effect, [memoizedKey]));
};
