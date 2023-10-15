{-# LANGUAGE Arrows #-}
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


-- TODO: review this
-- Using "the unsafePerformIO hack"
-- cf https://wiki.haskell.org/Top_level_mutable_state
gameReactHandle :: ReactHandle (Int,Int) (Int, Int)
{-# NOINLINE gameReactHandle #-}
gameReactHandle = unsafePerformIO $ reactInit initialInput actuate signalFunction


signalFunction :: SF (Int, Int) (Int, Int)
signalFunction = proc (mouseX, mouseY) -> do
  t <- time -< ()
  let x = mouseX + floor (Math.cos (4*t) *50) 
  let y = mouseY + floor (Math.sin (4*t) * 50) 
  returnA -< (x, y)


initialInput :: IO (Int, Int)
initialInput = return (0, 0)

actuate ::  (ReactHandle (Int, Int) (Int, Int) -> Bool -> (Int, Int) -> IO Bool)
actuate _ _ out = do
  writeIORef gameOutput out
  return False

-- Exported function to perform single game step
foreign export ccall runStep :: Int -> Int -> IO ()
runStep :: Int -> Int -> IO ()
runStep x y = do
  _ <- react gameReactHandle (0.01, Just (x, y))
  return ()


-- Game output is written to IORef variable.
-- And a couple of functions to read the output values from Javascript.
-- This way of producing output can be change to passing some byte array
gameOutput :: IORef (Int, Int)
{-# NOINLINE gameOutput #-}
gameOutput = unsafePerformIO $ newIORef (0, 0)

foreign export ccall getOutX :: IO Int
getOutX :: IO Int
getOutX = do
  tuple <- readIORef gameOutput
  return $ fst tuple

foreign export ccall getOutY :: IO Int
getOutY :: IO Int
getOutY = do
  tuple <- readIORef gameOutput
  return $ snd tuple

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
