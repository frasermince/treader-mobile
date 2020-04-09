var paper = require('react-native-paper');

exports._textInput = paper.TextInput;
exports._surface = paper.Surface;
exports._button = paper.Button;
exports._listSection = paper.List.Section
exports._listItem = paper.List.Item
exports._title = paper.Title
exports._menu = paper.Menu
exports._menuItem = paper.Menu.Item
exports.listIcon = paper.List.Icon
exports._iconButton = paper.IconButton
exports._divider = paper.Divider
exports._modal = paper.Modal
exports._portal = paper.Portal
exports._subheading = paper.Subheading
exports._headline = paper.Headline
exports._paragraph = paper.Paragraph
exports._badge = paper.Badge
exports.navigationOptions = function(component) {
  return function (options) {
    component.navigationOptions = () => options;
    return component;
  }
}
