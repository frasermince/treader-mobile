import { NavigationContainer } from '@react-navigation/native';
import useAppState from "react-native-appstate-hook";
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
import { reactComponent as LanguageSettings } from "../output/LanguageSettings";
import Icon from 'react-native-vector-icons/MaterialIcons';
import CommunityIcon from 'react-native-vector-icons/MaterialCommunityIcons';
import { createMaterialBottomTabNavigator } from '@react-navigation/material-bottom-tabs';
import { createStackNavigator } from '@react-navigation/stack';
import { gql } from 'apollo-boost';
import { useQuery } from '@apollo/react-hooks';

ReviewNavigator = () => {
  const Stack = createStackNavigator();
  return (
      <Stack.Navigator headerMode="none">
        <Stack.Screen name="ReviewEntry" component={ReviewEntryScreen} initialParams={{complete: false}}/>
        <Stack.Screen name="Review" component={ReviewScreen} options={{gestureEnabled: false}}/>
        <Stack.Screen name="ReviewComplete" component={ReviewCompleteScreen} options={{gestureEnabled: false}}/>
      </Stack.Navigator>
  )

}

AccountNavigator = () => {
  const Stack = createStackNavigator();
  return (
      <Stack.Navigator>
        <Stack.Screen name="Settings" component={AccountScreen} options={{headerShown: false}}/>
        <Stack.Screen name="LanguageSettings" component={LanguageSettings} options={{headerShown: false}}/>
      </Stack.Navigator>
  );
}

BookNavigator = () => {
  const Stack = createStackNavigator();
  return (
      <Stack.Navigator>
        <Stack.Screen name="BookIndex" component={BookIndexScreen} options={{headerTitle: "Books"}}/>
        <Stack.Screen name="Read" component={BookScreen} options={{gestureEnabled: false, headerShown: false}} />
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
const query = gql `
  query getSelections($language: String) {
    dailySelections {
      id
      word
      sentence
      phrase
      phraseOffset
      sentenceOffset
      wordLength
      book {
        id
        language
      }
    }
    flashcards(language: $language) {
      id
      imageUrl
      a
      b
      t
      startOffset
      word
      hoursPassed
      sentence {
        id
        audioUrl
        text
        translation
      }
    }
  }
`

export default TabNavigator = () => {
  const Tab = createMaterialBottomTabNavigator();
  let homeIcon = ({focused, color}) => <Icon style={[{color: color}]} name={"home"} size={25} />;
  let bookIcon = ({focused, color}) => <Icon style={[{color: color}]} name={"book"} size={25} />
  let accountIcon = ({focused, color}) => <Icon style={[{color: color}]} name={"account-box"} size={25} />
  let flashcardIcon = ({focused, color}) => <CommunityIcon style={[{color: color}]} name={"card-bulleted-outline"} size={25} />
  let reviewIcon = ({focused, color}) => <Icon style={[{color: color}]} name={"layers"} size={25} />
  const {loading, error, data, refetch} = useQuery(query);
  useAppState({onForeground: () => refetch()});
  console.log("selections", {loading, error, data, refetch});
  let selectionCount = data && data.dailySelections.length != 0  ? data.dailySelections.length : false

  return (
    <NavigationContainer>
      <Tab.Navigator barStyle={{backgroundColor: "black"}}>
        <Tab.Screen name="Home" component={HomeScreen} options={{tabBarIcon:  homeIcon}} />
        <Tab.Screen name="Read" component={BookNavigator} options={{tabBarIcon:  bookIcon}} />
        <Tab.Screen name="Review" component={ReviewNavigator} options={{tabBarIcon: reviewIcon}} />
        <Tab.Screen name="Create" component={FlashcardNavigator} options={{tabBarIcon: flashcardIcon, tabBarBadge: selectionCount}} />
        <Tab.Screen name="Account" component={AccountNavigator} options={{tabBarIcon: accountIcon}} />
      </Tab.Navigator>
    </NavigationContainer>
  );
}

