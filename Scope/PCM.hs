{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS -Wall -fno-warn-orphans #-}
----------------------------------------------------------------------
{- |
   Module      : Scope.PCM
   Copyright   : Conrad Parker
   License     : BSD3-style (see LICENSE)

   Maintainer  : Conrad Parker <conrad@metadecks.org>
   Stability   : unstable
   Portability : unknown

   Scope plotting functions
-}

module Scope.PCM (
) where

import Control.Arrow (second)
import Data.Maybe (fromJust)
import Data.ZoomCache
import Data.ZoomCache.PCM
import Data.ZoomCache.PCM.Types

import Scope.Types hiding (b)

----------------------------------------------------------------------

instance ScopePlot (PCM Double) where
    rawLayerPlot = rawLayerPlotListPCMDouble
    summaryLayerPlot = summaryLayerPlotListPCMDouble

----------------------------------------------------------------------
-- Raw data

rawLayerPlotListPCMDouble :: PCM Double -> RGB -> LayerPlot (TimeStamp, [PCM Double])
rawLayerPlotListPCMDouble maxRange _rgb =
    LayerFold (plotRawListPCMDouble maxRange) plotRawListInitPCMDouble Nothing

plotRawListInitPCMDouble :: [DrawLayer]
plotRawListInitPCMDouble = repeat []

plotRawListPCMDouble :: PCM Double -> LayerFoldFunc (TimeStamp, [PCM Double]) (Maybe [PCM Double])
plotRawListPCMDouble yRange x w Nothing (ts, ys) = plotRawListPCMDouble yRange x w (Just ys) (ts, ys)
plotRawListPCMDouble (PCM yRange) x w (Just ys0) (ts, ys) =
    second Just $ foldl c ([], []) $ map f (zip3 (map yFunc [0..]) ys0 ys)
    where
        c :: ([a], [b]) -> ([a], b) -> ([a], [b])
        c (ds0, ss) (ds, s) = (ds0++ds, ss++[s])

        l = length ys
        yStep = 2.0 / fromIntegral l
        yFunc :: Double -> PCM Double -> Double
        yFunc n (PCM v) = (-1.0) + (n * yStep) + ((0.5) * yStep) + (v * yStep / yRange)
        f :: ((PCM Double -> Double), PCM Double, PCM Double) -> ([DrawLayer], PCM Double)
        f (y, s0, s) = second fromJust $ plotRaw1PCMDouble y x w (Just s0) (ts, s)

plotRaw1PCMDouble :: (PCM Double -> Double) -> LayerFoldFunc (TimeStamp, PCM Double) (Maybe (PCM Double))
plotRaw1PCMDouble f x w Nothing (ts, y) = plotRaw1PCMDouble f x w (Just y) (ts, y)
plotRaw1PCMDouble f x w (Just (PCM y0)) (_ts, y) = (cmds, Just (PCM y'))
    where
        cmds =
            [ [ MoveTo (x,   y0)
              , LineTo (x+w, y')
              ]
            ]
        y' = f y

----------------------------------------------------------------------
-- Summary data

summaryLayerPlotListPCMDouble :: PCM Double -> RGB -> LayerPlot [Summary (PCM Double)]
summaryLayerPlotListPCMDouble maxRange rgb =
    LayerFold (plotSummaryListPCMDouble maxRange) (plotSummaryListInitPCMDouble rgb) Nothing

plotSummaryListInitPCMDouble :: RGB -> [DrawLayer]
plotSummaryListInitPCMDouble (r, g, b) = concat $ repeat
    [ [ SetRGBA r g b 0.3 ]
    , [ SetRGB (r*0.6) (g*0.6) (b*0.6) ]
    ]

plotSummaryListPCMDouble :: PCM Double
                         -> LayerFoldFunc [Summary (PCM Double)] (Maybe [Summary (PCM Double)])
plotSummaryListPCMDouble dYRange x w Nothing ss =
    plotSummaryListPCMDouble dYRange x w (Just ss) ss
plotSummaryListPCMDouble (PCM dYRange) x w (Just ss0) ss = do
    second Just $ foldl c ([], []) $ map f (zip3 (map yFunc [0..]) ss0 ss)
    where
        c :: ([a], [b]) -> ([a], b) -> ([a], [b])
        c (ds0, sss) (ds, s) = (ds0++ds, sss++[s])

        l = length ss
        yStep = 2.0 / fromIntegral l
        yFunc n v = (-1.0) + (n * yStep) + ((0.5) * yStep) + (v * yStep / dYRange)
        f :: ((Double -> Double), Summary (PCM Double), Summary (PCM Double)) -> ([DrawLayer], Summary (PCM Double))
        f (y, s0, s) = second fromJust $ plotSummary1PCMDouble y x w (Just s0) s

-- | Plot one PCM summary
plotSummary1PCMDouble :: (Double -> Double)
                      -> LayerFoldFunc (Summary (PCM Double)) (Maybe (Summary (PCM Double)))
plotSummary1PCMDouble y x w Nothing s =
    plotSummary1PCMDouble y x w (Just s) s
plotSummary1PCMDouble y x w (Just s0) s = (cmds, Just s)
    where
        cmds =
            [ [ FillPoly [ (x,     y (pcmMax sd0))
                         , ((x+w), y (pcmMax sd))
                         , ((x+w), y (pcmMin sd))
                         , (x,     y (pcmMin sd0))
                         ]
              ]
            , [ MoveTo (x,     y (pcmAvg sd0))
              , LineTo ((x+w), y (pcmAvg sd))
              ]
            ]
        sd0 = summaryData s0
        sd = summaryData s

