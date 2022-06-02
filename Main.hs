module Main where

import System.Environment (getArgs)
import Data.Word
import Data.Array
import Data.Bits (shiftL, Bits(..))
import Control.Monad (forever)
import System.IO (hSetBuffering, stdin, BufferMode(NoBuffering))

{- Steps:
 -  Load one instruction from memory at the address of the PC register.
 -  Increment the PC register.
 -  Look at the opcode to determine which type of instruction it should perform.
 -  Perform the instruction using the parameters in the instruction.
 -  Go back to step 1.
-}

type RegisterStack = Array Int Word16
type ProgramCounter = Word16

data Stack = Stack { reg :: RegisterStack
                   , pc :: ProgramCounter
                   }

memory_max :: Word16
memory_max = 16

memory_size :: (Bits a, Num a) => a
memory_size = 1 `shiftL` 16

initialize :: Stack
initialize = Stack { reg = array (0,memory_size-1) [(i,0) | i <- [0..memory_size-1]]
                   , pc = 0x300
                   }

data Registers = R0
               | R1
               | R2
               | R3
               | R4
               | R5
               | R6
               | R7
               | RPC
               | RCOND
               | RCOUNT
               deriving (Show, Eq, Bounded, Enum)

data OpCodes = BR   -- branch
             | ADD  -- add
             | LD   -- load
             | STR  -- store
             | JSR  -- jump register
             | AND  -- bitwise and
             | LDR  -- load register
             | STRR -- store register
             | RTI  -- unused
             | NOT  -- bitwise not
             | LDI  -- load indirect
             | STI  -- store indirect
             | JMP  -- jump
             | RES  -- reserved (unused)
             | LEA  -- load effective address
             | TRAP -- execute trap
             deriving (Show, Eq, Bounded, Enum)

run :: [FilePath] -> IO ()
run = mapM_ (\f -> putStrLn ("you are number: " ++ (show f))) 

main :: IO ()
main = do hSetBuffering stdin NoBuffering
          args <- getArgs
          case (length args) < 2 of
            True -> putStrLn "lc3 [image-file]..."
            False -> forever $ run args 

