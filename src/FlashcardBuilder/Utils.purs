module FlashcardBuilder.Util where

import Prelude
import Data.String (split, Pattern(..), trim)
import Data.String (length)
import React.Basic.Hooks (JSX, ReactComponent, component, element, useState, (/\), useRef, readRefMaybe, useEffect, readRef, UseEffect, UseState, Hook, coerceHook, useContext)
import Markup as M
import Data.Foldable (foldl)

underlineWord sentence offset = _.textList $ foldl foldFn accumStart words
  where words = split (Pattern " ") sentence
        accumStart = {textList: mempty, sentenceLength: 0}
        textResult :: forall a . Record a -> String -> JSX -> JSX
        textResult style word list = list <>
                                     (M.getJsx $ M.text {style: M.css style} $ M.string word)
                                     <> (M.getJsx $ M.text {style: M.css style} $ M.string " ")
        foldFn {textList, sentenceLength} word
            | sentenceLength + length word > offset && sentenceLength <= offset =
                {
                  textList: textResult {textDecorationLine: "underline", fontWeight: "bold"} word textList,
                  sentenceLength: sentenceLength + length word + 1
                }
            | otherwise =
                  {
                    textList: textResult {fontWeight: "bold"} word textList,
                    sentenceLength: sentenceLength + length word + 1
                  }


