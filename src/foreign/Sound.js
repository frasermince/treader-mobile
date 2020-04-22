const Sound = require('react-native-sound')

exports.createSound = function (music) {
  Sound.setCategory("Playback");
  return new Sound(music, '', (error) => {
    if (error) {
      console.warn('failed to load the sound', error)
      return null
    }
  });
}

exports.play = function (sound) {
  return function () {
    sound.play((success) => {
      if (!success) {
        console.warn('playback failed due to audio decoding errors')
      }
    })
  }
}

exports.stop = function (sound) {
  return function (callback) {
    return function () {
      return sound.stop(callback);
    }
  }
}

exports.release = function (sound) {
  return function () {
    sound.release();
  }
}

