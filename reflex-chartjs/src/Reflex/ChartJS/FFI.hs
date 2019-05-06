{-# LANGUAGE PackageImports #-}
{-# LANGUAGE JavaScriptFFI #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
module Reflex.ChartJS.FFI where

import             Prelude hiding ((!!))
import "lens"      Control.Lens hiding (element, (#))
import "aeson"     Data.Aeson (toJSON)
import "jsaddle"   Language.Javascript.JSaddle
import             GHCJS.DOM.Element (IsElement, toElement, unElement)
import             Reflex.ChartJS.Types

newtype ChartJsRef = ChartJsRef
                   { unChartJsRef :: JSVal
                   }





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
    chart <- nextAnimationFrame $ \_ -> new (jsg "Chart") (js_element, js_chart)
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
    (data' <# "labels")   (toJSON . _chartJsData_labels   . _chartJs_data $ chartJs_)

    _ <- chart # "update" $ ()
    return ()


destroy :: ChartJsRef
        -> JSM ()
destroy ref = do
    chart <- valToObject . unChartJsRef $ ref
    _ <- chart ^. js1 "destroy" ()
    return ()



clear :: ChartJsRef
      -> JSM ()
clear ref = do
    chart <- valToObject . unChartJsRef $ ref
    _ <- chart ^. js1 "clear" ()
    return ()



registerOnClick :: (IsElement el)
                => el
                -> ChartJsRef
                -> (ChartJsSelection -> JSM ())
                -> JSM ()
registerOnClick element_ chartJsRef callback = do
    let js_element = unElement . toElement $ element_
    chart <- valToObject . unChartJsRef $ chartJsRef

    js_element ^. jss "onclick" (fun $ \_ _ [evt] -> do
        activePoints <- chart ^. js1 "getElementsAtEvent" evt
        isUndefined' <- valIsUndefined activePoints
        if isUndefined'
        then return ()
        else do
            length' <- activePoints ^. js "length" >>= valToNumber
            if 0 == length'
            then return ()
            else do
                activePoint <- activePoints !! 0

                datasetLabel <- activePoint ^. js "_model" ^. js "datasetLabel" >>= valToText
                datasetIndex <- activePoint ^. js "_datasetIndex"               >>= (fmap floor . valToNumber)
                label        <- activePoint ^. js "_model" ^. js "label"        >>= valToText
                index'       <- activePoint ^. js "_index"                      >>= (fmap floor . valToNumber)

                datasets <- chart ^. js "data" ^. js "datasets"
                dataset  <- datasets !! datasetIndex
                data'    <- dataset ^. js "data"
                value    <- data' !! index' >>= valToNumber

                let selection = ChartJsSelection
                              { _chartJsSelection_datasetLabel = Just datasetLabel
                              , _chartJsSelection_datasetIndex = Just datasetIndex
                              , _chartJsSelection_label        = Just label
                              , _chartJsSelection_index        = Just index'
                              , _chartJsSelection_value        = Just value
                              }
                callback selection

        return ()
        )
