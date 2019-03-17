{-# LANGUAGE PackageImports #-}
{-# LANGUAGE JavaScriptFFI #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
module Reflex.ChartJS.FFI where

{-import Control.Monad.IO.Class-}
{-import Control.Concurrent-}
{-import Data.ByteString.Char8 (ByteString, unpack)-}
import "aeson"       Data.Aeson (toJSON)
import "jsaddle"     Language.Javascript.JSaddle
{-import "ghcjs-dom-jsffi"        GHCJS.DOM.Types (IsElement)-}
{-import "ghcjs-dom-jsffi"        GHCJS.DOM.Types (IsElement)-}
import GHCJS.DOM.Element -- (IsElement)
{-import "ghcjs-base"       GHCJS.Types (JSVal)-}
{-import "ghcjs-base"       GHCJS.Marshal (toJSVal_aeson)-}
{-import "ghcjs-base"       GHCJS.Foreign.Callback (Callback, syncCallback1', syncCallback1, OnBlocked(..))-}
{-import "ghcjs-dom-jsffi"  GHCJS.DOM.Types (unElement)-}
{-import "ghcjs-dom-jsffi"  GHCJS.DOM.Element (IsElement, toElement)-}
import                    Reflex.ChartJS.Types
import Control.Lens hiding (element, (#))
{-import "file-embed" Data.FileEmbed (embedFile)-}

newtype ChartJsRef = ChartJsRef
                   { unChartJsRef :: JSVal
                   }


{-chartJsSource :: ByteString-}
{-chartJsSource = $(embedFile "jsbits/chart.js")-}


--
newChart :: (IsElement element)
         => element
         -- ^ Element to use
         -> ChartJs
         -- ^ ChartJs Data
         -> ChartJsCallbacks
         -- ^ ChartJs callbacks
         -> JSM ChartJsRef
newChart element
         chartJs_
         callbacks = do
    let js_element  = unElement . toElement  $ element
    let js_chart = toJSON $ chartJs_
    {-_ <- eval $ unpack chartJsSource-}
    chart <- nextAnimationFrame $ \_ -> new (jsg "Chart") (js_element, js_chart)
    {-chart <- jsg "Chart"-}
    w <- jsg "console"
    _ <- w ^. js1 "log" chart
    {-chart <- jsg "console"-}
    return $ ChartJsRef chart
    {-js_xFormatter <- case _chartJsCallbacks_xAxisFormatter callbacks of-}
                            {-Just formatter -> syncCallback1' formatter-}
                            {-Nothing        -> syncCallback1' return-}
    {-js_yFormatter <- case _chartJsCallbacks_yAxisFormatter callbacks of-}
                            {-Just formatter -> syncCallback1' formatter-}
                            {-Nothing        -> syncCallback1' return-}
    {-js_tooltipFormatter <- case _chartJsCallbacks_tooltipFormatter callbacks of-}
                            {-Just formatter -> syncCallback1' formatter-}
                            {-Nothing        -> syncCallback1' return-}
    {-js_chartJs  <- toJSVal_aeson chartJs_-}
    {-ref <- js_newChart js_element-}
                       {-js_chartJs-}
                       {-js_xFormatter-}
                       {-js_yFormatter-}
                       {-js_tooltipFormatter-}
    {-return ref-}
    {-return undefined-}

{-foreign import javascript unsafe-}
    {-"(function() {                                     \-}
    {-\    var ctx              = $1;                    \-}
    {-\    var chartjs          = $2;                    \-}
    {-\    var xFormatter       = $3;                    \-}
    {-\    var yFormatter       = $4;                    \-}
    {-\    var tooltipFormatter = $5;                    \-}
    {-\    var tooltipLabel = function(tooltipitem, data) { \-}
    {-\        return tooltipFormatter(tooltipitem.yLabel); \-}
    {-\    };                                            \-}
    {-\                                                  \-}
    {-\    if ( xFormatter                               \-}
    {-\      && ('scales' in chartjs.options)            \-}
    {-\      && ('xAxes' in chartjs.options.scales)      \-}
    {-\       ) {                                        \-}
    {-\        var xAxes = chartjs.options.scales.xAxes; \-}
    {-\        for (i = 0; i < xAxes.length; i++) {      \ -}
    {-\            xAxes[i] = jQuery.extend(xAxes[i],    \-}
    {-\               {ticks: {callback: xFormatter}}    \ -}
    {-\            );                                    \ -}
    {-\        }                                         \-}
    {-\        chartjs.options.scales.xAxes = xAxes;     \-}
    {-\    }                                             \-}
    {-\                                                  \-}
    {-\    if ( yFormatter                               \-}
    {-\      && ('scales' in chartjs.options)            \-}
    {-\      && ('yAxes' in chartjs.options.scales)      \-}
    {-\       ) {                                        \-}
    {-\        var yAxes = chartjs.options.scales.yAxes; \-}
    {-\        for (i = 0; i < yAxes.length; i++) {      \ -}
    {-\            yAxes[i] = jQuery.extend(yAxes[i],    \-}
    {-\               {ticks: {callback: yFormatter}}    \ -}
    {-\            );                                    \ -}
    {-\        }                                         \-}
    {-\        chartjs.options.scales.yAxes = yAxes;     \-}
    {-\    }                                             \-}
    {-\                                                  \-}
    {-\    if ( tooltipFormatter                         \-}
    {-\      && ('tooltips' in chartjs.options)          \-}
    {-\      && ('callbacks' in chartjs.options.tooltips) \-}
    {-\       ) {                                        \-}
    {-\        chartjs.options.tooltips.callbacks.label = tooltipLabel; \-}
    {-\    }                                             \-}
    {-\                                                  \-}
    {-\    var chart = new Chart(ctx, chartjs);          \-}
    {-\    return chart;                                 \-}
    {-\})()"-}
    {-js_newChart :: JSVal-}
                {--- ^ Element to use-}
                {--> JSVal-}
                {--- ^ ChartJs-}
                {--> Callback (JSVal -> IO JSVal)-}
                {--- ^ xFormatter-}
                {--> Callback (JSVal -> IO JSVal)-}
                {--- ^ yFormatter-}
                {--> Callback (JSVal -> IO JSVal)-}
                {--- ^ tooltip Formatter-}
                {--> IO ChartJsRef-}

--
update :: ChartJsRef
       -- ^ ChartJs ref
       -> ChartJs
       -- ^ ChartJs data
       -> JSM ()
update ref chartJs_ = do
    chart <- valToObject . unChartJsRef $ ref
    (chart <# "type") (toJSON . _chartJs_type $ chartJs_)
    data' <- chart ^. js "data"
    (data' <# "datasets") (toJSON . _chartJsData_datasets . _chartJs_data $ chartJs_)
    _ <- chart # "update" $ ()
    return ()
    {-js_chartJs  <- toJSVal_aeson chartJs_-}
    {-js_update ref js_chartJs-}

{-foreign import javascript unsafe-}
    {-"(function() {                                  \-}
    {-\    $1.type = $2.type;                         \-}
    {-\    $1.data.datasets = $2.data.datasets;       \-}
    {-\    if ($2.data.labels !== undefined) {        \-}
    {-\       $1.data.labels = $2.data.labels;}       \-}
    {-\    $1.update();                               \-}
    {-\})()"-}
    {-js_update :: ChartJsRef-}
              {--- ^ ChartJs ref-}
              {--> JSVal-}
              {--- ^ ChartJs Data-}
              {--> IO ()-}

--
destroy :: ChartJsRef
        -> JSM ()
destroy ref = do
    undefined
    {-js_destroy ref-}

{-foreign import javascript unsafe-}
    {-"$1.destroy()"-}
    {-js_destroy :: ChartJsRef-}
               {--> IO ()-}

--
clear :: ChartJsRef
      -> JSM ()
clear ref = do
    undefined
    {-js_clear ref-}

{-foreign import javascript unsafe-}
    {-"$1.clear()"-}
    {-js_clear :: ChartJsRef-}
             {--> IO ()-}

--
registerOnClick :: (IsElement el)
                => el
                -> ChartJsRef
                -> (JSVal -> IO ())
                -> JSM ()
registerOnClick element_ chartJsRef callback = do
    undefined
    {-let js_element = unElement . toElement $ element_-}
    {-js_onclick <- syncCallback1 ContinueAsync callback-}
    {-js_registerOnClick js_element-}
                       {-chartJsRef-}
                       {-js_onclick-}

{-foreign import javascript unsafe-}
    {-"(function() {                                                     \-}
    {-\ $1.onclick = function(evt){                                      \-}
    {-\   var activePoints  = $2.getElementsAtEvent(evt);                \-}
    {-\   if (activePoints === undefined) { return; }                    \-}
    {-\   if (0 == activePoints.length) { return; }                      \-}
    {-\                                                                  \-}
    {-\   var datasetLabel = activePoints[0]._model.datasetLabel;        \-}
    {-\   var datasetIndex = activePoints[0]._datasetIndex;              \-}
    {-\   var label        = activePoints[0]._model.label;               \-}
    {-\   var index        = activePoints[0]._index;                     \-}
    {-\   var value        = $2.data.datasets[datasetIndex].data[index]; \-}
    {-\                                                                  \-}
    {-\   $3(JSON.stringify( { 'datasetLabel': datasetLabel              \-}
    {-\                      , 'datasetIndex': datasetIndex              \-}
    {-\                      , 'label':        label                     \-}
    {-\                      , 'index':        index                     \-}
    {-\                      , 'value':        value                     \-}
    {-\                      }));                                        \-}
    {-\};                                                                \-}
    {-\})()"-}
    {-js_registerOnClick :: JSVal-}
                       {--> ChartJsRef-}
                       {--> Callback (JSVal -> IO ())-}
                       {--> IO ()-}
