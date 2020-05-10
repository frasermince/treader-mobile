var c = require("react-native-copilot");
var wrap = require('../../src/WrapCopilot')

exports._copilot = function(opts) {
  return function(component) {
    return c.copilot(opts) (component);
  }
}
exports._copilotStep = c.CopilotStep
exports._walkthroughable = function (component) {
  debugger
  return wrap.default(component);
}
