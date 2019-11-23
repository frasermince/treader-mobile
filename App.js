import { createAppContainer, createSwitchNavigator } from 'react-navigation';
import { reactComponent as BookScreen} from "./output/App";
import { reactComponent as IndexScreen} from "./output/BookIndex";
import { reactComponent as AuthLoadingScreen} from "./output/AuthLoading";
import { reactComponent as SignInScreen} from "./output/SignIn";
import { provider as DataStateContext} from "./output/Context";
import { createStackNavigator } from 'react-navigation-stack';
import { Provider as PaperProvider } from 'react-native-paper';
import React, {useState} from 'react';
import { ApolloProvider } from '@apollo/react-hooks';
import client from './src/ApolloClient';
import { Button, Snackbar } from 'react-native-paper';

// Implementation of HomeScreen, OtherScreen, SignInScreen, AuthLoadingScreen
// goes here.

const AppStack = createStackNavigator({ Index: IndexScreen, Book: BookScreen }, {headerMode: 'screen'});
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
  const [error, setError] = useState(null);
  const [loading, setLoading] = useState(false);
  const [errorVisible, setErrorVisible] = useState(false);
  const setSnackbar = (error) => {
    if (error === "") {
      setVisible(false)
    } else {
      setError(error);
      setVisible(true);
    }
  }
  const contextValue = { setLoading: setLoading, setError: setSnackbar}
  return (
    <ApolloProvider client={client}>
      <PaperProvider>
        <DataStateContext value={contextValue}>
          <Routing/>
        </DataStateContext>
        <Snackbar
          visible={errorVisible}
          onDismiss={() => setErrorVisible(false)}
          action={{
            label: 'Close',
            onPress: () => {
              // Do something
            },
          }}
        >
          {error}
        </Snackbar>
      </PaperProvider>
    </ApolloProvider>
  );
}
