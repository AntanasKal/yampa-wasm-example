{-# LANGUAGE ForeignFunctionInterface #-}

-- Functions that are imported from JS
module Imports where
import Foreign.Ptr ( Ptr )
import Foreign.C ( CChar, newCStringLen )
import Foreign (free)

foreign import ccall "clearCanvas" clearCanvas :: Int -> Int -> Int -> IO ()
foreign import ccall "fillStyle" fillStyle :: Int -> Int -> Int -> IO ()
foreign import ccall "fillRect" fillRect :: Double -> Double -> Double -> Double -> IO ()
foreign import ccall "getCanvasWidth" getCanvasWidth :: IO Int
foreign import ccall "getCanvasHeight" getCanvasHeight :: IO Int
-- void fillText(char* textPtr, int textLen, double x, double y, double maxWidth) {
foreign import ccall "fillText" fillText :: Ptr CChar -> Int -> Double -> Double -> Double -> IO ()
foreign import ccall "setFont" setFont :: Ptr CChar -> Int -> IO ()
-- void arc(double x, double y, double radius, double startAngle, double endAngle, bool counterclockwise);
foreign import ccall "arc" arc :: Double -> Double -> Double -> Double -> Double -> Bool -> IO ()
-- void ellipse(double x, double y, double radiusX, double radiusY, double rotation, double startAngle, double endAngle, bool counterclockwise);
foreign import ccall "ellipse" ellipse :: Double -> Double -> Double -> Double -> Double -> Double -> Double -> Bool -> IO ()
foreign import ccall "fill" fill :: IO ()
foreign import ccall "beginPath" beginPath :: IO ()
foreign import ccall "closePath" closePath :: IO ()
foreign import ccall "stroke" stroke :: IO ()
foreign import ccall "moveTo" moveTo :: Double -> Double -> IO ()
foreign import ccall "lineTo" lineTo :: Double -> Double -> IO ()
-- Helper function to avoid dealing with manual memory management
fillTextHelper :: String -> Double -> Double -> Double -> IO ()
fillTextHelper textStr x y maxWidth = do
  (buf, len) <- newCStringLen textStr
  fillText buf len x y maxWidth
  free buf

setFontHelper :: String -> IO ()
setFontHelper textStr = do
  (buf, len) <- newCStringLen textStr
  setFont buf len
  free buf

renderCircle :: Double -> Double -> Double -> Int -> Int -> Int -> IO ()
renderCircle posX posY radius colR colG colB = do
  beginPath
  arc posX posY radius 0 (2*pi) False
  fillStyle colR colG colB
  fill
