{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE AllowAmbiguousTypes#-}

module Main where

import Data.Word (Word16, )
import Data.Bits (shiftR, shiftL, (.|.), (.&.), Bits(..))
import Data.List (foldl')
import Control.Monad
import Data.Array
import Data.Array.IO
import System.IO (hSetBuffering, BufferMode(..), hFlush, stdin, stdout)
import System.Environment (getArgs)

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

cast :: R -> Word16
cast = (fromIntegral . fromEnum)

data Status = Running
            | Halted
            deriving (Show, Eq)

type Registers = Array Word16 Word16

type Memory = Array Word16 Word16

data Machine = Machine { reg :: Registers
                       , mem :: Memory
                       , status :: Status
                       }

build_registers :: Registers
build_registers = array (0,10) [(i,0) | i <- [0..10]]

initialize_memory :: Memory
initialize_memory = array (0,memory_size-1) [(i,0) | i <- [0..(memory_size-1)]]

memory_size :: (Bits a, Num a) => a
memory_size = 1 `shiftL` 16

mem_read r = undefined

--create_machine :: Machine
create_machine = Machine { reg = build_registers 
                         , mem = initialize_memory
                         , status = Running
                         }

setup_registers regs = do regs' <- thaw regs
                          writeArray regs' (cast RCOND) fl_zro
                          writeArray regs' (cast RPC) pc_start
                          imm <- freeze regs'
                          imm

pc_start :: Word16
pc_start = 0x3000

trap_getc, trap_out, trap_puts, trap_in, trap_putsp, trap_halt :: Word16
trap_getc  = 0x20 
trap_out   = 0x21 
trap_puts  = 0x22 
trap_in    = 0x23 
trap_putsp = 0x24 
trap_halt  = 0x25 

fl_zro, fl_pos, fl_neg :: (Bits a, Num a) => a
fl_zro = 1 `shiftL` 1
fl_pos = 1 `shiftL` 0
fl_neg = 1 `shiftL` 2

sign_extend :: Word16 -> Int -> Word16
sign_extend bit bit_count
    | (bit `shiftR` (bit_count - 1) .&. 1) == 1 = bit .|. (0xFFFF `shiftL` bit_count)
    | otherwise = bit


update_flags :: MArray a Word16 m => a Word16 Word16 -> Word16 -> m ()
update_flags arr r = do rg <- readArray arr r 
                        case rg of
                            z | z == 0              -> writeArray arr r' fl_zro
                              | z `shiftR` 15 == 1  -> writeArray arr r' fl_neg
                              | otherwise           -> writeArray arr r' fl_pos
                            where r' = cast RCOND 

swap16 :: Word16 -> Word16
swap16 x = (x `shiftL` 8) .|. (x `shiftR` 8) 

merge l r = foldl' go 0x0 (zip [15,14..0] bits)
  where
    go acc (n,True) = setBit acc n
    go acc (n,False) = acc
    bits =
      map (testBit l) [7,6..0] ++
      map (testBit r) [7,6..0]

process bytes = map go (chunks 2 bytes)
    where go [_] = error "odd number"
          go [x,y] = merge x y

chunks _ [] = []
chunks n xs = let (l,r) = splitAt n xs
              in l : chunks n r

add :: MArray a Word16 m => a Word16 Word16 -> Word16 -> m ()
add arr instr = case (imm_flag == 1) of
    True -> do let imm5 = sign_extend (instr .&. 0x1F) 5
               rg1 <- readArray arr r1
               writeArray arr r0 (rg1+imm5)
               update_flags arr r0

    False -> do let r2 = instr .&. 0x4
                rg1 <- readArray arr r1
                rg2 <- readArray arr r2
                writeArray arr r0 (rg1+rg2)
                update_flags arr r0

    where r0 = (instr `shiftR` 9) .&. 0x7
          r1 = (instr `shiftR` 6) .&. 0x7
          imm_flag = (instr `shiftR` 5) .&. 0x1

ldi arr instr = do let r0 = (instr `shiftR` 9) .&. 0x7
                   let pc_offset = sign_extend (instr .&. 0x1FF) 9
                   mutable <- thaw arr
                   update_flags mutable r0

read_image_file args = undefined        

main :: IO ()
main = do hSetBuffering stdin NoBuffering 
          putStrLn "Hello!"
