{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE AllowAmbiguousTypes#-}

module Main where

import Data.Word (Word16, Word8)
import Data.Bits (shiftR, shiftL, (.|.), (.&.), Bits(..))
import Data.List (foldl')
import qualified Data.ByteString as B
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

check_key :: IO (Word16)
check_key = do result <- B.hGetNonBlocking stdin 1
               case result of
                    x | B.null x -> pure 0
                      | otherwise -> do let [l] = B.unpack x
                                        pure $ fromIntegral l

mem_read :: Word16 -> Memory -> IO (Memory, Word16)
mem_read address memory = do key <- check_key 
                             case address == mr_kbsr of
                                True -> case key /= 0 of

                                    True ->  do mem' <- make_memory_mutable memory 
                                                writeArray mem' mr_kbsr (1 `shiftL` 15)
                                                writeArray mem' mr_kbdr key
                                                mem'' <- freeze mem'
                                                return (mem'', mem''!address)
                                    False -> do mem' <- make_memory_mutable memory
                                                writeArray mem' mr_kbsr 0
                                                mem'' <- freeze mem'
                                                return (mem'', mem''!address)

                                False -> return (memory, memory!address)

make_memory_mutable :: Memory -> IO (IOArray Word16 Word16)
make_memory_mutable memory = thaw memory

make_registers_mutable :: Registers -> IO (IOArray Word16 Word16)
make_registers_mutable regs = thaw regs

setup_registers :: Registers -> IO (Registers)
setup_registers regs = do regs' <- make_registers_mutable regs 
                          writeArray regs' (cast RCOND) fl_zro
                          writeArray regs' (cast RPC) pc_start
                          freeze regs'

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

mr_kbsr, mr_kbdr :: Word16 
mr_kbsr = 0xFE00
mr_kbdr = 0xFE02


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

merge :: Word8 -> Word8 -> Word16
merge l r = foldl' go 0x0 (zip [15,14..0] bits)
  where
    go acc (n,True) = setBit acc n
    go acc (n,False) = acc
    bits =
      map (testBit l) [7,6..0] ++
      map (testBit r) [7,6..0]

process :: [Word8] -> [Word16] 
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

go :: Registers -> Memory -> IO ()
go registers memory = do instr <- mem_read (registers!(cast RPC)) memory
                         

read_image_file :: String -> IO (Memory) 
read_image_file file = do (origin:bytes) <- process . B.unpack <$> B.readFile file
                          let pad = zip [1..origin-1] $ replicate (fromIntegral origin - 1) (0x0 :: Word16)
                              bytes_size = fromIntegral (length bytes)::Word16
                              mid = zip [1..bytes_size] $ (origin:bytes)
                              pad_size = length pad
                              mid_size = length mid
                              memory_size' = memory_size - (mid_size + pad_size)
                              end = zip [1..fromIntegral memory_size' :: Word16] $ replicate memory_size' (0x0 :: Word16)
                          pure $ array (1,fromIntegral memory_size' :: Word16) (pad ++ mid ++ end)
                        
load_args :: [String] -> IO (Memory)
load_args args = case (length args) < 1 of 
    True -> error "lc3 [image-file1]...\n"
    False -> case args of
        (file:_) -> read_image_file file

main :: IO ()
main = do hSetBuffering stdin NoBuffering   -- setup

          args <- getArgs                   -- load args
          heap <- load_args args
          regs' <- setup_registers build_registers
          let exMachina = Machine regs' heap Running  
          let loop = do unless ((status exMachina) == Halted)
                         $ do (heap', instr) <- mem_read (regs'!(cast RPC)) heap
                              let op = instr `shiftR` 12
                              (go regs' heap') >> loop
          putStrLn "Done!"
