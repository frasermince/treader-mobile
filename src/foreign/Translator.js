const translate = require('react-native-power-translator');


exports._translate = function(apiKey) {
  return function(language) {
    return function(snippet) {
      return function() {
        translate.TranslatorConfiguration.setConfig(translate.ProviderTypes.Google, apiKey, 'en', language);
        const translator = translate.TranslatorFactory.createTranslator();
        debugger
        return translator.translate(snippet);
      }
    }
  }
}
