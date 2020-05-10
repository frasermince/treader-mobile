import { NavigationContainer } from '@react-navigation/native';
import React from 'react';
import { reactComponent as HomeScreen} from "../output/Home";
import { reactComponent as BookScreen} from "../output/BookView";
import { reactComponent as BookIndexScreen} from "../output/BookIndex";
import { reactComponent as AccountScreen} from "../output/Account";
import { reactComponent as DailySelections} from "../output/FlashcardBuilder.DailySelections";
import { reactComponent as SentenceChoice} from "../output/FlashcardBuilder.SentenceChoice";
import { reactComponent as ImageChoice} from "../output/FlashcardBuilder.ImageChoice";
import { reactComponent as ReviewScreen} from "../output/FlashcardReview.Main";
import { reactComponent as ReviewEntryScreen} from "../output/FlashcardReview.Index";
import { reactComponent as ReviewCompleteScreen} from "../output/FlashcardReview.Complete";
//import Review from "./Tinder/Review"
import { reactComponent as WordSelection} from "../output/FlashcardBuilder.WordSelection";
import Icon from 'react-native-vector-icons/MaterialIcons';
import CommunityIcon from 'react-native-vector-icons/MaterialCommunityIcons';
import { createMaterialBottomTabNavigator } from '@react-navigation/material-bottom-tabs';
import { createStackNavigator } from '@react-navigation/stack';

ReviewNavigator = () => {
  const Stack = createStackNavigator();
  return (
      <Stack.Navigator headerMode="none">
        <Stack.Screen name="ReviewEntry" component={ReviewEntryScreen} />
        <Stack.Screen name="Review" component={ReviewScreen} options={{gestureEnabled: false}}/>
        <Stack.Screen name="ReviewComplete" component={ReviewCompleteScreen} options={{gestureEnabled: false}}/>
      </Stack.Navigator>
  )

}
BookNavigator = () => {
  const Stack = createStackNavigator();
  return (
      <Stack.Navigator headerMode="none">
        <Stack.Screen name="BookIndex" component={BookIndexScreen} />
        <Stack.Screen name="Read" component={BookScreen} options={{gestureEnabled: false}} />
      </Stack.Navigator>
  )
}
FlashcardNavigator = () => {
  const Stack = createStackNavigator();
  return (
      <Stack.Navigator>
        <Stack.Screen name="WordList" component={DailySelections} options={{headerTitle: "Flashcard Creation"}} />
        <Stack.Screen name="SentenceChoice" component={SentenceChoice} options={{headerTitle: "Choose Sentence"}} />
        <Stack.Screen name="ImageChoice" component={ImageChoice} options={{headerTitle: "Choose Images"}} />
        <Stack.Screen name="WordSelection" component={WordSelection} options={{headerTitle: "Choose More Words", headerLeft: null}} />
      </Stack.Navigator>
  );
}
export default TabNavigator = () => {
  const Tab = createMaterialBottomTabNavigator();
  let homeIcon = ({focused, color}) => <Icon style={[{color: color}]} name={"home"} size={25} />;
  let bookIcon = ({focused, color}) => <Icon style={[{color: color}]} name={"book"} size={25} />
  let accountIcon = ({focused, color}) => <Icon style={[{color: color}]} name={"account-box"} size={25} />
  let flashcardIcon = ({focused, color}) => <CommunityIcon style={[{color: color}]} name={"card-bulleted-outline"} size={25} />
  let reviewIcon = ({focused, color}) => <Icon style={[{color: color}]} name={"layers"} size={25} />
  return (
    <NavigationContainer>
      <Tab.Navigator barStyle={{backgroundColor: "black"}}>
        <Tab.Screen name="Home" component={HomeScreen} options={{tabBarIcon:  homeIcon}} />
        <Tab.Screen name="Read" component={BookNavigator} options={{tabBarIcon:  bookIcon}} />
        <Tab.Screen name="Review" component={ReviewNavigator} options={{tabBarIcon: reviewIcon}} />
        <Tab.Screen name="Create" component={FlashcardNavigator} options={{tabBarIcon: flashcardIcon}} />
        <Tab.Screen name="Account" component={AccountScreen} options={{tabBarIcon:  accountIcon}} />
      </Tab.Navigator>
    </NavigationContainer>
  );
}

