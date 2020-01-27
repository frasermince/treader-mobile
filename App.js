import { createAppContainer, createSwitchNavigator } from 'react-navigation';
import { reactComponent as BookScreen} from "./output/BookView";
import { reactComponent as IndexScreen} from "./output/BookIndex";
import { reactComponent as AuthLoadingScreen} from "./output/AuthLoading";
import { reactComponent as SignInScreen} from "./output/SignIn";
import { provider as DataStateContext} from "./output/Context";
import { createStackNavigator } from 'react-navigation-stack';
import { Provider as PaperProvider } from 'react-native-paper';
import { View } from 'react-native';
import React, {useState, useEffect} from 'react';
import { ApolloProvider } from '@apollo/react-hooks';
import apolloClient from './src/ApolloClient';
import { Button, Snackbar } from 'react-native-paper';
import { createMaterialBottomTabNavigator } from 'react-navigation-material-bottom-tabs';
import Icon from 'react-native-vector-icons/MaterialIcons';

// Implementation of HomeScreen, OtherScreen, SignInScreen, AuthLoadingScreen
// goes here.

const AppStack = createMaterialBottomTabNavigator(
  { Home: {
      screen: IndexScreen,
      navigationOptions: {
        tabBarIcon: ({ tintColor }) => (
          <View>
            <Icon style={[{color: tintColor}]} name={"home"} size={25} />
          </View>
        )
      }
    },
    Read: {
      screen: BookScreen,
      navigationOptions: {
        tabBarIcon: ({ tintColor }) => (
          <View>
            <Icon style={[{color: tintColor}]} name={"book"} size={25} />
          </View>
        )
      }
    }
  }, {barStyle: {backgroundColor: "black"}});
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
  const [client, setClient] = useState(null);
  useEffect(async function() {
    const c = await apolloClient();
    console.log("***C", c);
    setClient(c);
  }, [])
  const setSnackbar = (error) => {
    if (error === "") {
      setVisible(false)
    } else {
      setError(error);
      setVisible(true);
    }
  }
  const contextValue = { setLoading: setLoading, setError: setSnackbar}
  if (client) {
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
  } else {
    return (<View></View>);
  }
}
