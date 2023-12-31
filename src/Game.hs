{-# LANGUAGE Arrows #-}
{-# LANGUAGE ForeignFunctionInterface #-}
module Game where

import Imports
import FRP.Yampa
import Foreign.Ptr ( Ptr )
import Foreign ( mallocBytes, copyBytes )
import Data.IORef ( IORef, newIORef, readIORef, writeIORef )
import GHC.IO ( unsafePerformIO ) 
import qualified GHC.Float as Math
import Foreign.C ( CChar )
import Data.ByteString.Unsafe (unsafePackMallocCStringLen, unsafeUseAsCStringLen)
import Data.ByteString as BS ( reverse )

-- TODO: review this
-- Using "the unsafePerformIO hack"
-- cf https://wiki.haskell.org/Top_level_mutable_state
gameReactHandle :: ReactHandle GameInput GameOutput
{-# NOINLINE gameReactHandle #-}
gameReactHandle = unsafePerformIO $ reactInit initialInput actuate signalFunction

data GameInput = GameInput {
    mouseX :: Double
  , mouseY :: Double
}

defaultGameInput :: GameInput
defaultGameInput = GameInput 0 0

-- positions of roatating circle and mouse to the output
data GameOutput = GameOutput {
    mousePositionX :: Double
  , mousePositionY :: Double
  , circleX :: Double
  , circleY :: Double
}

defaultGameOutput :: GameOutput
defaultGameOutput = GameOutput 0 0 0 0

signalFunction :: SF GameInput GameOutput
signalFunction = proc gi -> do
  t <- time -< ()
  let x = mouseX gi + (Math.cos (4 * t) * 50) 
  let y = mouseY gi + (Math.sin (4 * t) * 50) 
  returnA -< GameOutput (mouseX gi) (mouseY gi) x y


initialInput :: IO GameInput
initialInput = return defaultGameInput

actuate :: ReactHandle GameInput GameOutput -> Bool -> GameOutput -> IO Bool
actuate _ _ out = do
  writeIORef gameOutput out
  renderGame out
  return False

-- Exported function to perform single game step, function is called from JS
foreign export ccall runGameStep :: Double -> Double -> Double -> IO ()
runGameStep :: Double -> Double -> Double -> IO ()
runGameStep x y deltaTime = do
  _ <- react gameReactHandle (deltaTime, Just (GameInput x y))
  return ()


renderGame :: GameOutput -> IO ()
renderGame out = do
  clearCanvas 30 30 120
  renderCircle (circleX out) (circleY out) 20 70 200 150
  setFontHelper "20px serif"
  fillStyle 180 230 200
  canvas_height <- getCanvasHeight
  fillTextHelper ("Mouse position x: "++show ((floor $ mousePositionX out) :: Integer)) 20 (fromIntegral $ canvas_height - 50) 300
  fillTextHelper ("Mouse position y: "++show ((floor $ mousePositionY out) :: Integer)) 20 (fromIntegral $ canvas_height - 20) 300
  return ()

-- Game output is written to IORef variable.
-- And a couple of functions to read the output values from Javascript.
-- This way of producing output can be changed to passing some byte array
gameOutput :: IORef GameOutput
{-# NOINLINE gameOutput #-}
gameOutput = unsafePerformIO $ newIORef defaultGameOutput

foreign export ccall getOutX :: IO Double
getOutX :: IO Double
getOutX = do
  tuple <- readIORef gameOutput
  return $ circleX tuple

foreign export ccall getOutY :: IO Double
getOutY :: IO Double
getOutY = do
  tuple <- readIORef gameOutput
  return $ circleY tuple

-- An example of sending byte arrays between Javascript and WASI reactor
-- Followed parts of this example: https://github.com/willmcpherson2/ghc-wasm-experiment
-- Not used in the game, just to exhibit Javascript <-> WASM communication
foreign export ccall reverseCharArray :: Ptr CChar -> Int -> IO (Ptr CChar)
reverseCharArray :: Ptr CChar -> Int -> IO (Ptr CChar)
reverseCharArray inputPtr inputLen = do
  input <- unsafePackMallocCStringLen (inputPtr, inputLen)
  let reversedInput = BS.reverse input
  unsafeUseAsCStringLen reversedInput $ \(buf, len) -> do
    outputPtr <- mallocBytes len
    copyBytes outputPtr buf len
    return outputPtr

-- Not used, but needed to compile
main :: IO ()
main = do
  return ()
