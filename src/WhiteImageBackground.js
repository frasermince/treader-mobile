import React from 'react';
import { View, ImageBackground } from 'react-native';

const WhiteImageBackground = (props) => {
    return (<ImageBackground
      source={require('../images/bg.png')}
      style={props.style}
    >{props.children}</ImageBackground>);

}

export default WhiteImageBackground;
