var paper = require('react-native-paper');

exports._textInput = paper.TextInput;
exports._surface = paper.Surface;
exports._button = paper.Button;
exports._listSection = paper.List.Section
exports._listItem = paper.List.Item
exports._title = paper.Title
exports._menu = paper.Menu
exports._menuItem = paper.Menu.Item
exports._switch = paper.Switch
exports.listIcon = paper.List.Icon
exports.navigationOptions = function(component) {
  return function (options) {
    component.navigationOptions = () => options;
    return component;
  }
}
