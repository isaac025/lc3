{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BlockArguments #-}

module Try where

import Data.Word (Word16, )
import Data.Bits (shiftR, shiftL, (.|.), (.&.), Bits(..))
import Control.Monad.ST
import Control.Monad.State
import Data.STRef
import Data.Array
import Data.Array.ST

-- Registers START
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

data Registers s = Registers { reg :: ST s (STArray s Word16 Word16) }

--buildRegisters :: Registers
buildRegisters = Registers { reg = newArray (0,10) 0 :: ST s (STArray s Word16 Word16) }

cast :: R -> Word16
cast = (fromIntegral . fromEnum)
-- Registers END

-- Memory START
memory_size :: (Bits a, Num a) => a
memory_size = 1 `shiftL` 16

mem_read r = undefined
-- Memory END

-- TRAP routines
trap_getc, trap_out, trap_puts, trap_in, trap_putsp, trap_halt :: Word16
trap_getc  = 0x20 
trap_out   =0x21 
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

-- Instructions
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
                   rg0 <- readArray arr r0
                   r_pc <- readArray arr (cast RPC)
                   let rg0' = mem_read (mem_read (r_pc + pc_offset))
                   writeArray arr rg0'

trap mem instr = case (instr .&. 0xFF) of
    trap_getc  -> undefined
    trap_out   -> undefined
    trap_puts  -> undefined -- 1. newArray with mem & extra space
                            -- 2. loop until reached last value & execute putc
                            -- 3. flush to stdout
    trap_in    -> undefined
    trap_putsp -> undefined
    trap_halt  -> undefined

-- MAIN START
main :: IO ()
main = print "Hello!"
-- MAIN END
