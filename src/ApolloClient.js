const { ApolloClient } = require('apollo-client')
import { setContext } from 'apollo-link-context';
const { InMemoryCache } = require('apollo-cache-inmemory')
import { createLink } from "apollo-absinthe-upload-link";
import { AsyncStorage } from 'react-native'
import { persistCache } from 'apollo-cache-persist';
import env from '../env'

console.log("HOST", env);
const authLink = setContext(async function(_, { headers }) {
  const token = await AsyncStorage.getItem('treader-session');
  // return the headers to the context so httpLink can read them
  return {
    headers: {
      ...headers,
      authorization: token ,
    }
  }
});

export default client = async function() {
  const cache = new InMemoryCache();
  await persistCache({
    cache,
    storage: AsyncStorage,
  });
  return new ApolloClient({
    cache: cache,
    link: authLink.concat(createLink(
      {
        uri: env.graphqlHost,
      }
    )),
  });
}

