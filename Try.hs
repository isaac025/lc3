{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BlockArguments #-}

module Try where

import Data.Word (Word16, )
import Data.Bits (shiftR, shiftL, (.&.), Bits(..))
import Control.Monad.ST
import Control.Monad.State
import Data.STRef
import Data.Array
import Data.Array.ST

--type Address = Word16
--type Val = Word16
--data Memory s = Memory { memory :: ST s (STArray s Address Val) }

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

--type Machine = State Registers

memory_size :: (Bits a, Num a) => a
memory_size = 1 `shiftL` 16


fl_zro, fl_pos, fl_neg :: (Bits a, Num a) => a
fl_zro = 1 `shiftL` 1
fl_pos = 1 `shiftL` 0
fl_neg = 1 `shiftL` 2

sign_extend :: Word16 -> Int -> Word16
sign_extend bit bit_count
    | (bit `shiftR` (bit_count - 1) .&. 1) == 1 = bit .|. (0xFFFF `shiftL` bit_count)
    | otherwise = bit

--buildRegisters :: Registers
buildRegisters = Registers { reg = newArray (0,10) 0 :: ST s (STArray s Word16 Word16) }

--update_flags :: STArray s Word16 Word16 -> Word16 -> STArray () Word16 ()
update_flags arr r = do rg <- readArray arr r 
                        case rg of
                            z | z == 0              -> writeArray arr r' fl_zro
                              | z `shiftR` 15 == 1  -> writeArray arr r' fl_neg
                              | otherwise           -> writeArray arr r' fl_pos
                            where r' = cast RCOND 

cast :: R -> Word16
cast = (fromIntegral . fromEnum)

--add :: (MArray a e m, Num e, Bits e) => a Word16 e -> Word16 -> m ()
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


main :: IO ()
main = print "Hello!"
