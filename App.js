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
import Config from "react-native-config"
import StoryBook from './storybook';
import { ActivityIndicator, Colors } from 'react-native-paper';



// Implementation of HomeScreen, OtherScreen, SignInScreen, AuthLoadingScreen
// goes here.
//
if(__DEV__) {
  import('./ReactotronConfig').then(() => console.log('Reactotron Configured'))
}

let loadingStyle = {
  zIndex: 3,
  position: 'absolute',
  alignItems: 'center',
  left: "43%",
  top: "43%",
  justifyContent: 'center'
}

export default App = () => {
  if (Config.IS_STORYBOOK == "true") {
    return (<StoryBook />);
  } else {

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
          <DataStateContext value={contextValue}>
            <PaperProvider theme={{...DefaultTheme, roundness: 3, colors: {...DefaultTheme.colors, primary: "#66aab1" }}}>
              <ActivityIndicator animating={loading} size={"large"} style={loadingStyle}/>
              <Main/>
              <Snackbar
                style={{zIndex: 25}}
                visible={errorVisible}
                onDismiss={() => setErrorVisible(false)}
                action={{
                  label: 'Close',
                    onPress: () => {
                      // Do something
                    },
              }}>
                {error}
              </Snackbar>
            </PaperProvider>
          </DataStateContext>
        </ApolloProvider>
      );
    } else {
      return (<View></View>);
    }
  }
}
