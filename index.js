/**
 * @format
 */

import {AppRegistry} from 'react-native';
import {reactComponent as App} from './output/App';
import {name as appName} from './app.json';

AppRegistry.registerComponent(appName, () => App);
