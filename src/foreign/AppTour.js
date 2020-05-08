var tour = require('react-native-app-tour');

exports.tourViewFor = function(node) {
  return function(options) {
    return tour.AppTourView.for(node, options);
  }
}

exports.addTarget = function(sequence) {
  return function(target) {
    sequence.add(target);
    return sequence;
  }
}

exports.emptySequence = function() {
  return new tour.AppTourSequence();
}

exports.showSequence = function (sequence) {
  return function() {
    tour.AppTour.ShowSequence(sequence);
  }
}
