var wr = require('wordreference-api');

exports._wordReference = function(word) {
      console.log("HARR");
  return function(targetLanguage) {
      console.log("HARR");
    return function(hostLanguage) {
      console.log("HARR");
      return function() {
        console.log("***WORD", word);
        return wr(word, targetLanguage, hostLanguage);
      }
    }
  }
}
