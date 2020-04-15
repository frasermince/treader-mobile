const Sound = require('react-native-sound')

exports.play = function (music) {
  return function () {
    const speech = new Sound(music, '', (error) => {
      if (error) {
        console.warn('failed to load the sound', error)
        return null
      }
      speech.play((success) => {
        if (!success) {
          console.warn('playback failed due to audio decoding errors')
        }
      })
      return null
    })
  }
}
