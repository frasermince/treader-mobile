import { NavigationContainer } from '@react-navigation/native';
import React from 'react';
import { reactComponent as BookScreen} from "../output/BookView";
import { reactComponent as IndexScreen} from "../output/BookIndex";
import { reactComponent as AccountScreen} from "../output/Account";
import { reactComponent as DailySelectionList} from "../output/DailySelectionList";
import { reactComponent as FlashcardBuilderScreen} from "../output/FlashcardBuilder";
import Icon from 'react-native-vector-icons/MaterialIcons';
import CommunityIcon from 'react-native-vector-icons/MaterialCommunityIcons';
import { createMaterialBottomTabNavigator } from '@react-navigation/material-bottom-tabs';
import { createStackNavigator } from '@react-navigation/stack';

FlashcardNavigator = () => {
  const Stack = createStackNavigator();
  return (
      <Stack.Navigator>
        <Stack.Screen name="WordList" component={DailySelectionList}/>
        <Stack.Screen name="Build Sentence Flashcards" component={FlashcardBuilderScreen}/>
      </Stack.Navigator>
  );
}
export default TabNavigator = () => {
  const Tab = createMaterialBottomTabNavigator();
  let homeIcon = ({focused, color}) => <Icon style={[{color: color}]} name={"home"} size={25} />;
  let bookIcon = ({focused, color}) => <Icon style={[{color: color}]} name={"book"} size={25} />
  let accountIcon = ({focused, color}) => <Icon style={[{color: color}]} name={"account-box"} size={25} />
  let flashcardIcon = ({focused, color}) => <CommunityIcon style={[{color: color}]} name={"card-plus-icon"} size={25} />
  return (
    <NavigationContainer>
      <Tab.Navigator barStyle={{backgroundColor: "black"}}>
        <Tab.Screen name="Home" component={IndexScreen} options={{tabBarIcon:  homeIcon}} />
        <Tab.Screen name="Flashcards" component={FlashcardNavigator} options={{tabBarIcon: flashcardIcon}} />
        <Tab.Screen name="Read" component={BookScreen} options={{tabBarIcon:  bookIcon}} />
        <Tab.Screen name="Account" component={AccountScreen} options={{tabBarIcon:  accountIcon}} />
      </Tab.Navigator>
    </NavigationContainer>
  );
}

