{-# LANGUAGE ForeignFunctionInterface #-}

-- Functions that are imported from JS
module Imports where

foreign import ccall "renderCircle" renderCircle :: Double -> Double -> Double -> Int -> Int -> Int -> IO ()
foreign import ccall "clearCanvas" clearCanvas :: Int -> Int -> Int -> IO ()
