import React from 'react';
import { View } from 'react-native';
export default Wrap = WrappedComponent => { 
  return ({copilot, ...props}) => {

    return (
      <View {...copilot}>
        <WrappedComponent {...props}></WrappedComponent>
      </View>
    );
  }
}
