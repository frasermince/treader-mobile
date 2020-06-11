var analytics = require('@segment/analytics-react-native');

exports.identify = function(email){
  return function(data) {
    return function() {
      analytics.default.identify(email, data);
    }
  }
}

exports.track = function(event){
  return function(data) {
    return function() {
      analytics.default.track(event, data);
    }
  }
}

exports.screen = function(screen){
  return function(data) {
    return function() {
      analytics.default.screen(screen, data);
    }
  }
}
