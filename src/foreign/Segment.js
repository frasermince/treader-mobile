var analytics = require('@segment/analytics-react-native');

exports._identify = function(email){
  return function(data) {
    return function() {
      analytics.default.identify(email, data);
    }
  }
}

exports._track = function(event){
  return function(data) {
    return function() {
      analytics.default.track(event, data);
    }
  }
}

exports._screen = function(screen){
  return function(data) {
    return function() {
      analytics.default.screen(screen, data);
    }
  }
}
