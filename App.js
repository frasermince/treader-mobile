import 'react-native-gesture-handler';
import { reactComponent as Main} from "./output/Main";
import { reactComponent as SignInScreen} from "./output/SignIn";
import { provider as DataStateContext} from "./output/Context";
import { Provider as PaperProvider } from 'react-native-paper';
import { View } from 'react-native';
import React, {useState, useEffect} from 'react';
import { ApolloProvider } from '@apollo/react-hooks';
import apolloClient from './src/ApolloClient';
import { Button, Snackbar, DefaultTheme } from 'react-native-paper';

// Implementation of HomeScreen, OtherScreen, SignInScreen, AuthLoadingScreen
// goes here.
//
if(__DEV__) {
  import('./ReactotronConfig').then(() => console.log('Reactotron Configured'))
}
export default App = () => {
  const [error, setError] = useState(null);
  const [loading, setLoading] = useState(false);
  const [errorVisible, setErrorVisible] = useState(false);
  const [client, setClient] = useState(null);
  useEffect(() => {
    async function awaitClient() {
      const c = await apolloClient();
      setClient(c);
    }
    awaitClient();
  }, []);
  const setSnackbar = (error) => {
    if (error === "") {
      setErrorVisible(false)
    } else {
      setError(error);
      setErrorVisible(true);
    }
  }
  const contextValue = { setLoading: setLoading, setError: setSnackbar}
  if (client) {
    return (
      <ApolloProvider client={client}>
        <PaperProvider theme={{...DefaultTheme, roundness: 3}}>
          <DataStateContext value={contextValue}>
            <Main/>
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
