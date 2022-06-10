module Main where

import System.Environment (getArgs)
import Data.Word
import Data.Array
import Data.Bits (shiftR, shiftL, Bits(..), (.&.))
import Control.Monad (forever)
import System.IO (hSetBuffering, stdin, BufferMode(NoBuffering))

{- Steps:
 -  Load one instruction from memory at the address of the PC register.
 -  Increment the PC register.
 -  Look at the opcode to determine which type of instruction it should perform.
 -  Perform the instruction using the parameters in the instruction.
 -  Go back to step 1.
-}

-- Register Stack
data R = R0
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

type Register = Array Word16 Word16
type PC = Word16

data Registers = Registers { reg :: Register
                           , pc :: PC
                           }

initialize :: Registers
initialize = Registers { reg = array (0,memory_size-1) [(i,0) | i <- [0..memory_size-1]]
                       , pc = 0x300
                       }
-- Register Stack end

-- Memory
type Address = Word16
type Val = Word16

memory_max :: Word16
memory_max = 16

memory_size :: (Bits a, Num a) => a
memory_size = 1 `shiftL` 16

data Memroy = Memory { memory :: Array Address Val }
-- Memory End

sign_extend :: Word16 -> Int -> Word16
sign_extend bit bit_count
    | (bit `shiftR` (bit_count - 1) .&. 1) == 1 = bit .|. (0xFFFF `shiftL` bit_count)
    | otherwise = bit

update_flags :: Word16 -> Registers -> Word16
update_flags r registers
    | (rg!r) == 0               = rg!(r_cond)
    | ((rg!r) `shiftR` 15 == 1) = rg!(r_cond)
    | otherwise                 = rg!(r_cond)
        where rg = reg registers
              r_cond = cast RCOND

add :: Word16 -> Word16
add instr = case imm_flag == 1 of
    True ->  undefined
    False -> undefined
    where r0 = (instr `shiftR` 9) .&. 0x7
          r1 = (instr `shiftR` 6) .&. 0x7
          imm_flag = (instr `shiftR` 5) .&. 0x1

cast :: R -> Word16
cast val = (fromIntegral . fromEnum) val :: Word16

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

