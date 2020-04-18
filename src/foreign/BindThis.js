
exports.bindThis = function(fn) {
  return function(self) {
    return fn.bind(self);
  }
}
