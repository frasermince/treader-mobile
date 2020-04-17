import React from 'react';
import { View, ImageBackground } from 'react-native';
import CardStack, { Card } from 'react-native-card-stack-swiper';
import CardItem from './CardItem';
import styles from './Styles';
import Demo from './Demo.js';

const Home = () => {
  return (
    <ImageBackground
      source={require('./images/bg.png')}
      style={styles.bg}
    >
      <View style={styles.containerHome}>
        <View style={styles.top}>
        </View>

        <CardStack
          loop={true}
          verticalSwipe={false}
          renderNoMoreCards={() => null}
          ref={swiper => (this.swiper = swiper)}
        >
          {Demo.map((item, index) => (
            <Card key={index}>
              <CardItem
                image={item.image}
                name={item.name}
                description={item.description}
                matches={item.match}
                actions
                onPressLeft={() => this.swiper.swipeLeft()}
                onPressRight={() => this.swiper.swipeRight()}
              />
            </Card>
          ))}
        </CardStack>
      </View>
    </ImageBackground>
  );
};

export default Home;
