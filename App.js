import { createAppContainer, createSwitchNavigator } from 'react-navigation';
import { reactComponent as HomeScreen} from "./output/App";
import { reactComponent as AuthLoadingScreen} from "./output/AuthLoading";
import { reactComponent as SignInScreen} from "./output/SignIn";
import { createStackNavigator } from 'react-navigation-stack';
import { Provider as PaperProvider } from 'react-native-paper';
import React from 'react';
import { ApolloProvider } from '@apollo/react-hooks';
import client from './src/ApolloClient';

// Implementation of HomeScreen, OtherScreen, SignInScreen, AuthLoadingScreen
// goes here.

const AppStack = createStackNavigator({ Home: HomeScreen});
const AuthStack = createStackNavigator({ SignIn: SignInScreen });

const Routing = createAppContainer(
  createSwitchNavigator(
    {
      AuthLoading: AuthLoadingScreen,
      App: AppStack,
      Auth: AuthStack,
    },
    {
      initialRouteName: 'AuthLoading',
    }
  )
);

export default App = () => {
  return (

    <ApolloProvider client={client}>
      <PaperProvider>
        <Routing/>
      </PaperProvider>
    </ApolloProvider>
  );
}
