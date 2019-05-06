{-# LANGUAGE PackageImports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE JavaScriptFFI #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}
module Reflex.JsonEditor.FFI where

import "text"            Data.Text (Text)
import "text"            Data.Text.Encoding (encodeUtf8)
import "aeson"           Data.Aeson (ToJSON(..), FromJSON(..), decodeStrict)
import "ghcjs-base"      GHCJS.Types (JSVal)
import "ghcjs-base"      GHCJS.Marshal (fromJSVal, toJSVal_aeson)
import "ghcjs-base"      GHCJS.Foreign.Callback (Callback, syncCallback1, OnBlocked(..))
import "ghcjs-dom-jsffi" GHCJS.DOM.Types (unElement)
import "ghcjs-dom-jsffi" GHCJS.DOM.Element (IsElement, toElement)

newtype JsonEditorRef = JsonEditorRef
                      { unJsonEditorRef :: JSVal
                      }

--
newJsonEditor :: (ToJSON a, FromJSON a, IsElement element)
              => element
              -- ^ Element to use
              -> (Maybe a -> IO ())
              -- ^ trigger
              -> IO JsonEditorRef
newJsonEditor element_ trigger = do
    let js_element = unElement . toElement $ element_
    js_onchange <- syncCallback1 ContinueAsync (onChange trigger)
    ref <- js_newJsonEditor js_element
                            js_onchange
    return ref


onChange :: (ToJSON a, FromJSON a)
         => (Maybe a -> IO ())
         -- ^ trigger
         -> JSVal
         -- ^ value
         -> IO ()
onChange trigger value = do
    Just (json_ :: Text) <- fromJSVal value
    let mEvent = decodeStrict . encodeUtf8 $ json_
    trigger mEvent


foreign import javascript unsafe
    "(function() {                                         \
    \    var container = $1;                               \
    \    var options = {                                   \
    \        onChange: function () {                       \
    \            $2(JSON.stringify(editor.get()));         \
    \        },                                            \
    \        mode: 'code'                                  \
    \    };                                                \
    \    var editor = new JSONEditor(container, options);  \
    \    return editor;                                    \ 
    \})()"
    js_newJsonEditor :: JSVal
                     -- ^ Element
                     -> Callback (JSVal -> IO ())
                     -- ^ on change
                     -> IO JsonEditorRef

--
set :: (ToJSON a, FromJSON a)
    => JsonEditorRef
    -- ^ ref
    -> a
    -- ^ value
    -> IO ()
set ref json = do
    js_json <- toJSVal_aeson json
    js_set ref js_json

foreign import javascript unsafe
    "(function() {    \
    \    $1.set($2);  \
    \})()"
    js_set :: JsonEditorRef
           -> JSVal
           -> IO ()
