var withPerformanceMonitor = require('react-native-performance-monitor/provider');
exports.withPerformanceMonitor = function(name) {
  return function(component) {
    return withPerformanceMonitor.default(component, name);
  }
}