{-# LANGUAGE Arrows #-}
{-# LANGUAGE ForeignFunctionInterface #-}
module Game where

import FRP.Yampa
import Foreign.Ptr ( Ptr )
import Foreign ( mallocBytes, copyBytes )
import Data.IORef ( IORef, newIORef, readIORef, writeIORef )
import GHC.IO ( unsafePerformIO ) 
import qualified GHC.Float as Math
import Foreign.C ( CChar )
import Data.ByteString.Unsafe (unsafePackMallocCStringLen, unsafeUseAsCStringLen)
import Data.ByteString as BS ( reverse )

foreign import ccall "renderCircle" renderCircle :: Double -> Double -> Double -> Int -> Int -> Int -> IO ()
foreign import ccall "clearCanvas" clearCanvas :: Int -> Int -> Int -> IO ()
-- TODO: review this
-- Using "the unsafePerformIO hack"
-- cf https://wiki.haskell.org/Top_level_mutable_state
gameReactHandle :: ReactHandle (Int,Int) GameOutput
{-# NOINLINE gameReactHandle #-}
gameReactHandle = unsafePerformIO $ reactInit initialInput actuate signalFunction

data GameOutput = GameOutput {
  circleX :: Double,
  circleY :: Double
}

defaultGameOutput :: GameOutput
defaultGameOutput = GameOutput 0 0

signalFunction :: SF (Int, Int) GameOutput
signalFunction = proc (mouseX, mouseY) -> do
  t <- time -< ()
  let x = fromIntegral mouseX + (Math.cos (4 * t) * 50) 
  let y = fromIntegral mouseY + (Math.sin (4 * t) * 50) 
  returnA -< GameOutput x y


initialInput :: IO (Int, Int)
initialInput = return (0, 0)

actuate ::  (ReactHandle (Int, Int) GameOutput -> Bool -> GameOutput -> IO Bool)
actuate _ _ out = do
  writeIORef gameOutput out
  return False

-- Exported function to perform single game step
foreign export ccall runGameStep :: Int -> Int -> IO ()
runGameStep :: Int -> Int -> IO ()
runGameStep x y = do
  _ <- react gameReactHandle (0.01, Just (x, y))
  return ()

-- Exported function to render the game
foreign export ccall renderGame :: IO ()
renderGame :: IO ()
renderGame = do
  out <- readIORef gameOutput
  clearCanvas 30 30 180
  renderCircle (circleX out) (circleY out) (20) (70) (200) (150)
  return ()


-- Game output is written to IORef variable.
-- And a couple of functions to read the output values from Javascript.
-- This way of producing output can be change to passing some byte array
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

main :: IO ()
main = do
  return ()
