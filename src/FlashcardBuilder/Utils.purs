module FlashcardBuilder.Util where

import Prelude
import Data.Function (applyN)
import Data.String (splitAt, Pattern(..), trim, length)
import React.Basic.Hooks (JSX, ReactComponent, component, element, useState, (/\), useRef, readRefMaybe, useEffect, readRef, UseEffect, UseState, Hook, coerceHook, useContext)
import Markup as M
import Markup (Markup)
import React.Basic.DOM.Internal (CSS)
import Data.Maybe (fromMaybe)
import Debug.Trace (spy)
import Data.String.Utils (repeat)

underlinedWord fontWeight fontSize word = M.text {style: M.css {textDecorationLine: "underline", fontWeight: fontWeight, fontSize: fontSize}} $ M.string word

textResult :: forall a . (String -> M.Markup Unit) -> String -> String -> String -> CSS -> Markup Unit
textResult fn before word after style = do
  M.text {style: style} $ M.string before
  fn word
  M.text {style: style} $ M.string after

beginningTextResult :: forall a . (String -> M.Markup Unit) -> String -> String -> CSS -> Markup Unit
beginningTextResult fn word after style = do
  fn word
  M.text {style: style} $ M.string after

underlineWord sentence length w style fontWeight fontSize = M.getJsx $ underlineWordMarkup sentence length w style fontWeight fontSize
underlineWordMarkup :: String -> Int -> String -> CSS -> String -> Int -> Markup Unit
underlineWordMarkup sentence length w style fontWeight fontSize = applyToWord (underlinedWord fontWeight fontSize) sentence length w style

cloze word = M.view {style: M.css {borderBottomWidth: 1.6, borderBottomColor: "black", width: "15%", bottom: 2, marginLeft: 4, marginRight: 4}} $ M.text {} $ M.string $ ""

clozeWord :: String -> Int -> String -> CSS -> Markup Unit
clozeWord sentence length w style = applyToWord cloze sentence length w style

applyToWord :: (String -> Markup Unit) -> String -> Int -> String -> CSS -> Markup Unit
applyToWord fn sentence 0 w style = beginningTextResult fn word after style
  where {before: word, after: after} = splitAt (length w) (trim sentence)
applyToWord fn sentence offset w style = textResult fn before word after style
  where {before: before, after: a} = splitAt (offset + 1) (trim sentence)
        {before: word, after: after} = splitAt (length w) a

