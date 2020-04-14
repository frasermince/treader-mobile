import React from 'react';
import { Text } from 'react-native';

import { storiesOf } from '@storybook/react-native';
import { action } from '@storybook/addon-actions';
import { linkTo } from '@storybook/addon-links';

import Button from './Button';
import CenterView from './CenterView';
import Welcome from './Welcome';
import { reactComponent as WordGrid} from "../../output/FlashcardBuilder.WordGrid";

let redirect = function(a) {
  return function(b) {
    return function(c){
      return function(){}
    }
  }
}
storiesOf('Welcome', module).add('to Storybook', () => <Welcome showApp={linkTo('Button')} />);

storiesOf('Button', module)
  .addDecorator(getStory => <CenterView>{getStory()}</CenterView>)
  .add('word grid', () => (
    <WordGrid redirect={redirect} words={[{word: "This", offset: 0}, {word: "is", offset: 4}, {word: "a", offset: 8}, {word: "sentence", offset: 12}]}/>
  ))
  .add('with text', () => (
    <Button onPress={action('clicked-text')}>
      <Text>Hello Button</Text>
    </Button>
  ))
  .add('with some emoji', () => (
    <Button onPress={action('clicked-emoji')}>
      <Text>😀 😎 👍 💯</Text>
    </Button>
  ));
