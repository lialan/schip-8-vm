module Main where

import Data.Array.Base
import Data.Char
import qualified Data.Set as S
import qualified Data.ByteString as BS
import System.Environment 
import System.IO
import System.Random (newStdGen)
import Numeric (showHex)
import Data.Maybe

import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Data.Color (black, white)
import Graphics.Gloss.Data.Display
import Graphics.Gloss.Interface.Pure.Game

import Chip8.State (VMState(..), create, nextInstruction, showDisplay)
import Chip8.Opcodes (runInstruction)

cpuFrequency :: Int
cpuFrequency = 10

screenFrequency :: Int
screenFrequency = 1 

-- | Runs the next instruction on the VM state and returns the resulting state
step :: VMState  -- ^ The starting state
     -> VMState  -- ^ The stepped through state
step s@VMState { pc = pc, memory = memory, delayTimer = delayTimer, tick = tick, display = d} =
  case waitForKeypress s of
      Nothing -> runInstruction s' op
      Just _ -> s
  where
    op = nextInstruction s
    tick' = tick + 1
    delayTimer' = if and [delayTimer > 0, tick' `mod` ((fromIntegral cpuFrequency) `div` 60) == 0]  then delayTimer - 1 else delayTimer
    s' = s { pc = pc + 2, delayTimer = delayTimer', tick = tick' }


-- | Generates a Gloss picture to represent the current state's display
drawScreen :: VMState -> Picture
drawScreen s@VMState { display = d, extended = e } = color white $
    pictures [
        translate 200 160 $
        scale 0.25 0.25 $
        text $ "del:" ++ show (delayTimer s),

        translate (-40) 160 $
        scale 0.25 0.25 $
        text $ "pc:" ++ showHex (pc s) "",

        translate (-320) 160 $
        scale 0.25 0.25 $
        text $ "op:" ++ showHex (nextInstruction s) "",

        translate (-320) 140 $
        scale 1 (-1) $
        pictures
        [translate x' y' pixel
            | x <- [0,1..xsize]
            , y <- [0,1..ysize]
            , let x' = (fromIntegral x) * size
            , let y' = (fromIntegral y) * size
            , d ! (x, y) ]]
  where
    size = if e then 5 else 10
    xsize = if e then 127 else 63
    ysize = if e then 63 else 31 
    pixel = if e then rectangleSolid 5 5 else rectangleSolid 10 10

-- | Handles keyboard input by adding/removing pressed keys from the state
handleInput :: Event -> VMState -> VMState
handleInput (EventKey (Char c) ks _ _) s@VMState { pressed = pressed, v = v }
  | isHexDigit c = case waitForKeypress s of
      Nothing -> s { pressed = pressed' }
      Just x -> s { pressed = pressed'
                  , v = v // [(x, fromIntegral c')]  -- Set VX to the key
                  , waitForKeypress = Nothing }      -- Remove the wait flag
  | otherwise = s
  where
    c' = fromIntegral $ digitToInt c
    pressed' = case ks of
        Down -> S.insert c' pressed
        Up -> S.delete c' pressed

handleInput _ s = s

-- | Runs the programs with real(tm) graphics!
run :: VMState -> IO ()
run state =
    play
        window       -- Window info
        black        -- Background colour
        cpuFrequency -- Steps per second (100Hz)
        state        -- Starting state
        drawScreen   -- Display generating function
        handleInput  -- Input handling function
        step'        -- State stepping function
  where
    step' _ = step
    window = InWindow "SCHIP-8" (660, 380) (100, 100)

main :: IO ()
main = do
    args <- getArgs
    program <- BS.readFile $ head args
    randGen <- newStdGen
    run $ create (BS.unpack program) randGen
