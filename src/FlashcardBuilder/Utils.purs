module FlashcardBuilder.Util where

import Prelude
import Data.String (splitAt, Pattern(..), trim, length)
import React.Basic.Hooks (JSX, ReactComponent, component, element, useState, (/\), useRef, readRefMaybe, useEffect, readRef, UseEffect, UseState, Hook, coerceHook, useContext)
import Markup as M
import Data.Maybe (fromMaybe)

textResult :: forall a . String -> String -> String -> JSX
textResult before word after = M.getJsx do
  M.text {style: M.css {fontWeight: "bold"}} $ M.string before
  M.text {style: M.css {textDecorationLine: "underline", fontWeight: "bold"}} $ M.string word
  M.text {style: M.css {fontWeight: "bold"}} $ M.string after

beginningTextResult :: forall a . String -> String -> JSX
beginningTextResult word after = M.getJsx do
  M.text {style: M.css {textDecorationLine: "underline", fontWeight: "bold"}} $ M.string word
  M.text {style: M.css {fontWeight: "bold"}} $ M.string after

underlineWord :: String -> Int -> String -> JSX
underlineWord sentence 0 w = beginningTextResult word after
  where {before: word, after: after} = splitAt (length w) (trim sentence)
underlineWord sentence offset w = textResult before word after
  where {before: before, after: a} = splitAt (offset + 1) (trim sentence)
        {before: word, after: after} = splitAt (length w) a
