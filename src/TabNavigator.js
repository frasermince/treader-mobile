import { NavigationContainer } from '@react-navigation/native';
import React from 'react';
import { reactComponent as BookScreen} from "../output/BookView";
import { reactComponent as IndexScreen} from "../output/BookIndex";
import Icon from 'react-native-vector-icons/MaterialIcons';
import { createMaterialBottomTabNavigator } from '@react-navigation/material-bottom-tabs';

export default TabNavigator = () => {
  const Tab = createMaterialBottomTabNavigator();
  let homeIcon = ({focused, color}) => <Icon style={[{color: color}]} name={"home"} size={25} />;
  let bookIcon = ({focused, color}) => <Icon style={[{color: color}]} name={"book"} size={25} />
  return (
    <NavigationContainer>
      <Tab.Navigator barStyle={{backgroundColor: "black"}}>
          <Tab.Screen name="Home" component={IndexScreen} options={{tabBarIcon:  homeIcon}} />
          <Tab.Screen name="Read" component={BookScreen} options={{tabBarIcon:  bookIcon}} />
      </Tab.Navigator>
    </NavigationContainer>
  );
}