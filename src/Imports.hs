{-# LANGUAGE ForeignFunctionInterface #-}

-- Functions that are imported from JS
module Imports where
import Foreign.Ptr ( Ptr )
import Foreign.C ( CChar, newCStringLen )
import Foreign (free)

foreign import ccall "renderCircle" renderCircle :: Double -> Double -> Double -> Int -> Int -> Int -> IO ()
foreign import ccall "clearCanvas" clearCanvas :: Int -> Int -> Int -> IO ()
foreign import ccall "fillText" fillText :: Ptr CChar -> Int -> Int -> Int -> Int -> IO ()

fillTextHelper :: String -> Int -> Int -> Int -> IO ()
fillTextHelper textStr x y maxWidth = do
    (buf, len) <- newCStringLen textStr
    fillText buf len x y maxWidth
    free buf