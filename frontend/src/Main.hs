{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE CPP #-}

module Main where

import Data.Monoid ((<>))
import Data.Text
import Data.Default
import Control.Lens ((^.))
import Language.Javascript.JSaddle.Monad (liftJSM)
import Language.Javascript.JSaddle (jsg, js1)
#ifdef ghcjs_HOST_OS
import Reflex.Dom
#else
import Language.Javascript.JSaddle.Warp (run)
import Reflex.Dom.Core
#endif


main = do
#ifndef ghcjs_HOST_OS
  run 3912 $ do
#endif
    mainWidget $ do
        ti <- textInput def
        let textE = updated . _textInput_value $ ti
        performEvent_ $ ffor textE $ \text -> do
            let (number :: Int) = read . unpack $ text

            -- jsaddle code
            liftJSM $ do
                w <- jsg "console"
                w ^. js1 "log" (show $ number * 2)
                return ()
