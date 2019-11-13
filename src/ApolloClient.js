const { ApolloClient } = require('apollo-client')
import { setContext } from 'apollo-link-context';
const { InMemoryCache } = require('apollo-cache-inmemory')
import { createLink } from "apollo-absinthe-upload-link";
import { AsyncStorage } from 'react-native'
import env from '../env'

console.log("HOST", env);
const authLink = setContext(async function(_, { headers }) {
  // get the authentication token from local storage if it exists
  
  const token = await AsyncStorage.getItem('treader-session');
  // return the headers to the context so httpLink can read them
  console.log("token", token);
  return {
    headers: {
      ...headers,
      authorization: token ,
    }
  }
});
export default client = new ApolloClient({
  cache: new InMemoryCache(),
  link: authLink.concat(createLink(
    {
      uri: env.graphqlHost,
    }
  )),
})

