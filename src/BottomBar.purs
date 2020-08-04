module BottomBar where

import MaterialIcon (icon)
import Prelude
import Data.Array ((!!))
import Data.Traversable (traverse_)
import Debug.Trace (spy)
import Effect.Aff (Aff, launchAff_, delay, Milliseconds(..))
import ComponentTypes (BookViewQuery, AudioInformation)
import React.Basic.Hooks (JSX, ReactComponent, component, element, useState, (/\), useRef, readRefMaybe, useEffect)
import React.Basic.Hooks as React
import React.Basic.Native as RN
import Animated (view, timing, value)
import Effect.Unsafe (unsafePerformEffect)
import Record as Record
import Platform as Platform
import Effect (Effect)
import Data.Symbol (SProxy(..))
import Slider (slider)
import Markup as M
import Effect.Uncurried (EffectFn1, mkEffectFn1)
import Paper (fab)
import React.Basic.Native.Events as RNE
import Data.Maybe (Maybe(..), fromMaybe)
import FetchBlob (fetch)
import Data.Interpolate (i)
import FS (audioBookDir, mkdir, exists)
import Data.Foldable (foldl)
import Effect.Class (liftEffect)
import Sound (play, Sound, release, stop, createSound, setCurrentTime, pause, getCurrentTime)
import Data.Number (fromString)
import Data.Number.Approximate (eqApproximate)
import Data.Int (floor)
import Data.String (split, Pattern(..))
import Math (abs)

reactComponent :: ReactComponent Props
reactComponent =
  unsafePerformEffect
    $ do
        (component "BottomBar") buildJsx

type Props
  =
    { shown :: Boolean
    , disabled :: Boolean
    , value :: Int
    , onSlidingComplete :: Number -> Effect Unit
    , bookData :: Maybe BookViewQuery
    , slug :: String
    , audioInformation :: Maybe AudioInformation
    , visibleLocation :: { start :: { percentage :: Int, cfi :: String } }
    , setAudioTime :: ((Maybe Number -> Maybe Number) -> Effect Unit)
    , audioTime :: Maybe Number
    }

data PlayState = Paused | NotStarted | Playing
derive instance eqPlayState :: Eq PlayState

barStyles showBars fade showAudio =
  { left: 0
  , right: 0
  , height: if showAudio then 110 else 55
  , position: "absolute"
  , bottom: 0
  , opacity: fade
  , zIndex: zIndex
  , elevation: zIndex
  , shadowOpacity: 0.75
  , shadowRadius: 3
  , shadowOffset: { height: 5, width: 10 }
  }
  where
  zIndex :: Int
  zIndex =
    fade.interpolate
      { inputRange: [ 0, 1 ]
      , outputRange: [ -1, 9 ]
      }


footerStyles fade =
  { backgroundColor: "white"
  , paddingTop: 0
  , height: Platform.select { ios: 64, android: 54 }
  , right: 0
  , left: 0
  , alignItems: "center"
  , justifyContent: "center"
  , flex: 1
  , flexDirection: "row"
  , borderBottomColor: "#b2b2b2"
  , borderBottomWidth: 1
  }

sliderStyles fade =
  { height: 30
  , alignItems: "center"
  , justifyContent: "center"
  , flexDirection: "row"
  , flex: 1
  {--, zIndex: zIndex--}
  {--, elevation: zIndex--}
  , marginLeft: 50
  , marginRight: 50
  }
  where
  zIndex :: Int
  zIndex =
    fade.interpolate
      { inputRange: [ 0, 1 ]
      , outputRange: [ -1, 9 ]
      }

runAnimation true fade = timing fade { toValue: 1, duration: 20 }

runAnimation false fade = timing fade { toValue: 0, duration: 20 }

opacity = SProxy :: SProxy "opacity"

fileForChapter slug chapter = (dirForBook slug) <> "/" <> "chapter-" <> show chapter <> ".mp3"
dirForBook slug = audioBookDir <> "/" <> slug

timeSame Nothing b = false
timeSame (Just a) b = eqApproximate a b
trackTime :: ((Maybe Number -> Maybe Number) -> Effect Unit) -> Maybe Number -> Sound -> Aff Unit
trackTime setTime time sound = do
  delay $ Milliseconds 250.0
  currentTime <- getCurrentTime sound
  liftEffect $ setTime \_ -> Just $ spy "CURRENT TIME" currentTime
  if timeSame time currentTime then mempty else trackTime setTime (Just currentTime) sound

playPage :: String -> Maybe AudioInformation -> SoundData -> ((SoundData -> SoundData) -> Effect Unit) -> ((Maybe Number -> Maybe Number) -> Effect Unit) -> Effect Unit
playPage slug Nothing soundData setSoundData setTime = mempty
playPage slug (Just audioInformation) {sound, isPlaying} setSoundData setTime =
  case spy "ISPLAYING" isPlaying of
    NotStarted -> setTimeAndPlay
    Paused -> launchAff_ $ do
       liftEffect $ setSoundData \_ -> {sound, isPlaying: Playing}
       case sound of
          Just s -> do
             play s
             trackTime setTime Nothing s
          Nothing -> mempty
    Playing -> mempty
  where setTimeAndPlay = launchAff_ do
          let path = fileForChapter slug audioInformation.index
          sound <- createSound path
          sound <- setCurrentTime sound $ audioSeconds audioInformation
          liftEffect $ setSoundData \_ -> {sound: Just sound, isPlaying: Playing}
          play sound
          trackTime setTime Nothing sound

audioSeconds audioInformation = fromMaybe 0.0 do
  hours <- (segments !! 0) >>= fromString
  minutes <- (segments !! 1) >>= fromString
  seconds <- (segments !! 2) >>= fromString
  pure $ (hours * 3600.0) + (minutes * 60.0) + seconds
  where segments = split (Pattern ":") audioInformation.startPageTime

pauseSound :: SoundData -> ((SoundData -> SoundData) -> Effect Unit) -> Effect Unit
pauseSound {sound: Just sound, isPlaying} setSoundData =
  case isPlaying of
    NotStarted -> mempty
    Paused -> mempty
    Playing -> launchAff_ do
       pause sound
       liftEffect $ setSoundData \_ -> {sound: Just sound, isPlaying: Paused}

pauseSound {sound: Nothing, isPlaying} setSoundData = mempty

checkChaptersDownloaded :: ((Boolean -> Boolean) -> Effect Unit) -> String -> Maybe BookViewQuery -> Effect Unit
checkChaptersDownloaded setFilesDownloaded slug bookData = launchAff_ do
    result <- highestExists $ highestChapter bookData
    liftEffect $ setFilesDownloaded \_ -> result
  where highestExists (Just chapter) = exists $ fileForChapter slug chapter
        highestExists Nothing = pure $ false
        foldHighestChapter :: Int -> {chapter :: Int, audioUrl :: String} -> Int
        foldHighestChapter accum c = if c.chapter > accum then c.chapter else accum
        highestChapter Nothing = Nothing
        highestChapter (Just b) = Just $ foldl foldHighestChapter (-1) b.book.audioChapters

fetchFiles :: ((Boolean -> Boolean) -> Effect Unit) -> String -> Maybe BookViewQuery -> Effect Unit
fetchFiles setFilesDownloaded slug bookData = launchAff_ do
  mkdir path {}
  traverse_ (\b -> foldl downloadChapter mempty $ b.book.audioChapters) $ bookData
  liftEffect $ setFilesDownloaded \_ -> true
  where path = dirForBook slug
        downloadChapter :: Aff Unit -> {chapter :: Int, audioUrl :: String} -> Aff Unit
        downloadChapter accum chapterData = accum *> do
          _ <- fetch {fileCache: true, path: fileForChapter slug chapterData.chapter} "GET" chapterData.audioUrl {}
          pure unit

audioExists Nothing = false
audioExists (Just bookData) = not $ bookData.book.audioChapters == []

-- Checks if the user changes the page while the book is playing.
-- If the audio changed the page when it reached the end this should not be hit due to the two second buffer.
conditionallyChangeTime (Just audioInformation) (Just audioTime) (Just sound)
  | abs (audioTime - (audioSeconds audioInformation)) > 2.0 = launchAff_ $ setCurrentTime sound $ audioSeconds audioInformation
  | otherwise = mempty

conditionallyChangeTime _ _ _ = mempty

type SoundData = {sound :: Maybe Sound, isPlaying :: PlayState}
--buildJsx :: Props -> JSX
buildJsx props = React.do
  fade /\ setFade <- useState $ value 1
  filesDownloaded /\ setFilesDownloaded <- useState false
  soundData /\ setSoundData <- useState $ {sound: Nothing :: Maybe Sound, isPlaying: NotStarted}
  useEffect unit do
     checkChaptersDownloaded setFilesDownloaded props.slug props.bookData
     pure mempty
  useEffect props.shown do
    launchAff_ $ runAnimation props.shown fade
    pure mempty

  useEffect (_.index <$> props.audioInformation) do
     if soundData.isPlaying == Playing
       then launchAff_ do
          case soundData.sound of
               (Just sound) -> do
                  s <- stop sound
                  liftEffect $ release s
               Nothing -> mempty
          liftEffect $ playPage props.slug props.audioInformation {sound: soundData.sound, isPlaying: NotStarted} setSoundData props.setAudioTime
       else mempty
     pure mempty

  useEffect props.audioInformation do
     if soundData.isPlaying == Playing
      then conditionallyChangeTime props.audioInformation props.audioTime soundData.sound
      else setSoundData \_ -> {sound: soundData.sound, isPlaying: NotStarted}
     pure mempty

  pure $ M.getJsx $ do
     view
        { style: M.css $ barStyles props.shown fade (audioExists props.bookData)
        } do
            M.view { style: M.css $ footerStyles fade } do
              slider { style: M.css $ sliderStyles fade, disabled: props.disabled, value: props.value, onSlidingComplete: mkEffectFn1 props.onSlidingComplete, maximumTrackTintColor: "#707070" }
            if not $ audioExists props.bookData then mempty else view { style: M.css $ footerStyles fade } do
              if not filesDownloaded
                then M.touchableOpacity {style: M.css {flex: 1, flexDirection: "row"}, onPress: RNE.capture_ $ fetchFiles setFilesDownloaded props.slug props.bookData} do
                  icon {style: M.css {flex: 2, marginLeft: 10}, name: "file-download", size: 20}
                  M.text {style: M.css {flex: 5}} $ M.string "Tap to download audio"
                else M.view {} do
                   if soundData.isPlaying == Playing then fab
                      { icon: "pause"
                      , small: true
                      , style: M.css {width: 40}
                      , onPress: RNE.capture_ $ pauseSound soundData setSoundData
                      }
                   else fab
                      { icon: "play"
                      , small: true
                      , style: M.css {width: 40}
                      , onPress: RNE.capture_ $ playPage props.slug props.audioInformation soundData setSoundData props.setAudioTime
                      }
