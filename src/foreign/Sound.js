const Sound = require('react-native-sound')

exports._createSound = function (music) { // accepts a request
  return function (onError, onSuccess) { // and callbacks
    Sound.setCategory("Playback");
    let sound = new Sound(music, '', (error) => {
      if (error) {
        console.warn('failed to load the sound', error);
        onError(error.message);
      } else {
        onSuccess(sound)
      }
    });

    return function (cancelError, cancelerError, cancelerSuccess) {
      sound.stop(); // cancel the request
      cancelerSuccess(); // invoke the success callback for the canceler
    };
  };
};

exports._play = function (sound) {
  return function (onError, onSuccess) { // and callbacks
    return sound.play(() => {
      onSuccess(sound);
    });

    return function (cancelError, cancelerError, cancelerSuccess) {
      cancelerSuccess(); // invoke the success callback for the canceler
    };
  }
}

exports._stop = function (sound) {
  return function (onError, onSuccess) { // and callbacks
    return sound.stop(() => {
      onSuccess(sound);
    });

    return function (cancelError, cancelerError, cancelerSuccess) {
      cancelerSuccess(); // invoke the success callback for the canceler
    };
  }
}

exports.release = function (sound) {
  return function () {
    sound.release();
  }
}

