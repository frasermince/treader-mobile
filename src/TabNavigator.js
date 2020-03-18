import { NavigationContainer } from '@react-navigation/native';
import React from 'react';
import { reactComponent as BookScreen} from "../output/BookView";
import { reactComponent as IndexScreen} from "../output/BookIndex";
import { reactComponent as Subscribe} from "../output/Subscribe";
import Icon from 'react-native-vector-icons/MaterialIcons';
import { createMaterialBottomTabNavigator } from '@react-navigation/material-bottom-tabs';
import { createStackNavigator } from '@react-navigation/stack';
TabNavigator = () => {
  const Tab = createMaterialBottomTabNavigator();
  let homeIcon = ({focused, color}) => <Icon style={[{color: color}]} name={"home"} size={25} />;
  let bookIcon = ({focused, color}) => <Icon style={[{color: color}]} name={"book"} size={25} />
  return (
    <Tab.Navigator barStyle={{backgroundColor: "black"}}>
      <Tab.Screen name="Home" component={IndexScreen} options={{tabBarIcon:  homeIcon}} />
      <Tab.Screen name="Read" component={BookScreen} options={{tabBarIcon:  bookIcon}} />
    </Tab.Navigator>
  );
}

export default Navigator = () => {
  const Stack = createStackNavigator();
  return (
    <NavigationContainer>
      <Stack.Navigator>
        <Stack.Screen name="Main" component={TabNavigator} options={{ headerShown: false }}/>
        <Stack.Screen name="Subscribe" component={Subscribe} options={{ headerShown: false }}/>
      </Stack.Navigator>
    </NavigationContainer>
  );
}
