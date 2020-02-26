var wiktionary = require('wiktionary');

exports._getDefinition = function(word) {
  return function (locale) {
    return function (language) {
      return function () {
        return wiktionary(word, locale, language);
      }
    }
  }
}
