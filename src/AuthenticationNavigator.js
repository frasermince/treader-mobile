import React from 'react';
import { NavigationContainer } from '@react-navigation/native';
import { createStackNavigator } from '@react-navigation/stack';
import { reactComponent as SignIn} from "../output/SignIn";
import { reactComponent as SignUp} from "../output/SignUp";
import { reactComponent as Reset} from "../output/ResetPassword";
const Stack = createStackNavigator();

function AuthenticationNavigator() {
  return (
    <NavigationContainer>
      <Stack.Navigator initialRouteName={"Signup"}>
        <Stack.Screen name="Login" component={SignIn} />
        <Stack.Screen name="Signup" component={SignUp} />
        <Stack.Screen name="ResetPassword" component={Reset} options={{headerTitle: "Reset Password"}}/>
      </Stack.Navigator>
    </NavigationContainer>
  );
}
export default AuthenticationNavigator;
