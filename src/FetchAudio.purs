module FetchAudio where
import Prelude
import ApolloHooks (useMutation, gql)
import Effect.Aff (Aff, launchAff_, try)
import Effect (Effect)
import Effect.Uncurried (EffectFn1, mkEffectFn1, runEffectFn1, mkEffectFn2, runEffectFn2, EffectFn2)
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Sound (play, Sound, release, stop, createSound, stopAndPlay)
import Data.Traversable (traverse_, traverse)
import FetchBlob (fetch)
import Data.Map (Map)
import Data.Map (fromFoldable, lookup, empty, insert, values) as Map
import Data.Either (Either(..))
import Effect.Class (liftEffect)
import Data.String (stripPrefix, Pattern(..))
import QueryHooks (useData, UseData, stripGraphqlError)
import Effect.Exception (message)
import FS (audioDir, writeFile, exists, unlink, absintheFile)
import React.Basic.Hooks as React
import React.Basic.Hooks (JSX, ReactComponent, component, element, useState, useEffect, (/\), useLayoutEffect, UseEffect, UseState, Hook, coerceHook, useContext, UseContext)
import Data.Foldable (foldl)
import Data.Newtype (class Newtype)
import Context (dataStateContext, Context)
import Data.Interpolate (i)

type AudioMutate = {variables :: {input :: {text :: String, language :: String}}} -> Aff {textToSpeech :: {encodedUrl :: String}}

defaultAudioFile = audioDir <> "/speech.mp3"

audioMutation =
  gql
    """
mutation audioMutation($input: TextToSpeechInput!) {
  textToSpeech(input: $input) {
    encodedUrl
  }
}
  """

fileExists :: Maybe String -> Aff Boolean
fileExists (Just filePath) = exists filePath
fileExists Nothing = pure false

type DefaultSound = Maybe {url :: String, text :: String}
newtype UseAudio h
  = UseAudio (UseEffect DefaultSound (UseState (Maybe String) (UseState (Maybe Sound) (UseState (Map String String) (UseState Int (UseEffect Unit (UseContext Context h)))))))

derive instance ntUseAudio :: Newtype (UseAudio h) _

useAudio :: String -> DefaultSound -> Hook UseAudio {play :: String -> String -> Effect Unit, fetch :: String -> String -> Aff (Maybe {path :: String, sound :: Sound})}
useAudio tag audio = coerceHook $ React.do
  { setLoading, setError } <- useContext dataStateContext
  getAudio /\ d <- useMutation audioMutation { errorPolicy: "all" }
  numberOfAudio /\ setNumberOfAudio <- useState 0
  audioPaths /\ setAudioPaths <- useState (mempty :: Map String String)
  sound /\ setSound <- useState (Nothing :: Maybe Sound)
  soundText /\ setSoundText <- useState (Nothing :: Maybe String)
  let setAudioInformation :: String -> String -> Aff {sound :: Sound, path :: String}
      setAudioInformation path text = do
        s <- createSound $ path
        liftEffect $ setAudioPaths \m -> Map.insert text path m
        liftEffect $ setSoundText \_ -> Just text
        liftEffect $ setSound \_ -> Just $ s
        pure $ {sound: s, path: path}

  useEffect audio do
     case audio of
          Nothing -> mempty
          Just {url, text} -> launchAff_ do
             result <- fetch {fileCache: true} "GET" url {}
             path <- liftEffect result.path
             setAudioInformation ("file://" <> path) text
     pure $ launchAff_ do
        foldl unlinkFold mempty $ Map.values audioPaths
        liftEffect $ traverse_ release sound
  let fetch = fetchWaveNet tag audioPaths setAudioInformation getAudio setError numberOfAudio setNumberOfAudio
  pure $ { play: speak sound soundText fetch
         , fetch: fetch
         }
  where unlinkFold :: Aff Unit -> String -> Aff Unit
        unlinkFold accum path = accum *> (unlinkFile path)
        unlinkFile path = do
          e <- exists path
          if e then unlink path else mempty


speak :: Maybe Sound -> Maybe String -> (String -> String -> Aff (Maybe {sound :: Sound, path :: String})) -> String -> String -> Effect Unit
speak (Just sound) (Just soundText) fetch text language
  | soundText == text = launchAff_ $ stopAndPlay sound
  | otherwise = launchAff_ do
          s <- fetch text language
          traverse_ (_.sound >>> play) s
speak _ _ fetch text language = launchAff_ do
  s <- fetch text language
  traverse_ (_.sound >>> play) s

fetchWaveNet :: String -> Map String String -> (String -> String -> Aff {path :: String, sound :: Sound}) -> AudioMutate -> (EffectFn1 String Unit) -> Int -> ((Int -> Int) -> Effect Unit) -> String -> String -> Aff (Maybe {sound :: Sound, path :: String} )
fetchWaveNet tag audioPaths setAudioInformation fetchAudio setError numberOfAudio setNumberOfAudio text language = do
  case Map.lookup text audioPaths of
       Nothing -> fetchFromServer
       Just path -> do
          Just <$> (setAudioInformation path text)
  where fetchFromServer = do
          result <- try $ fetchAudio {variables: {input: {text: text, language: language}}}
          case result of
            Left error -> do
              liftEffect $ runEffectFn1 setError $ stripGraphqlError $ message error
              pure Nothing
            Right response -> do
              let filename = i audioDir "/audio-" numberOfAudio "-" tag ".mp3"
              liftEffect $ setNumberOfAudio \n -> n + 1
              writeFile filename response.textToSpeech.encodedUrl "base64"
              Just <$> (setAudioInformation filename text)

