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
import Data.Char (chr, ord)
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

data Trap = Getc
          | Out
          | Puts
          | In
          | PutsP
          | Halt
          deriving (Show, Eq, Bounded, Enum)


data OpCode = BR    -- branch
            | ADD   -- add 
            | LD    -- load
            | STR   -- store
            | JSR   -- jump register
            | AND   -- bitwise and
            | LDR   -- load register
            | STRR  -- store register
            | RTI   -- unused
            | NOT   -- bitwise not
            | LDI   -- load indirect
            | STRI   -- store indirect
            | JMP   -- jump
            | RES   -- reserved (unused)
            | LEA   -- load effective address
            | TRAP  -- execute trap 
            deriving (Eq, Ord, Show, Bounded, Enum)

cast :: R -> Word16
cast = (fromIntegral . fromEnum)

toE :: Enum e => Word16 -> e
toE = (toEnum . fromIntegral)

getOp :: Word16 -> OpCode 
getOp x = toE (x `shiftR` 12)

makeTrap :: Word16 -> Trap
makeTrap instr
    | instr == trap_getc = Getc
    | instr == trap_out = Out
    | instr == trap_puts = Puts
    | instr == trap_in = In
    | instr == trap_putsp = PutsP
    | instr == trap_halt = Halt

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

go :: Registers -> Memory -> IO ()
go registers memory = do (memory', instr) <- mem_read (registers!(cast RPC)) memory
                         registers' <- make_registers_mutable registers
                         rpc <- readArray registers' (cast RPC) 
                         writeArray registers' (cast RPC) (rpc+1) 
                         case getOp instr of
                            BR   -> do let pc_offset = sign_extend (instr .&. 0x1FF) 9
                                       let cond_flag = (instr `shiftR` 9) .&. 0x7
                                       r_pc <- readArray registers' (cast RPC)
                                       when ((cond_flag .&. r_pc) /= 0) $ do
                                        writeArray registers' (cast RPC) (r_pc + pc_offset)

                            ADD  -> do let imm_flag = (instr `shiftR` 5) .&. 0x1
                                       let r0 = (instr `shiftR` 9) .&. 0x7
                                       let r1 = (instr `shiftR` 6) .&. 0x7
                                       let pc_offset = sign_extend (instr .&. 0x1FF) 9
                                       let offset = sign_extend (instr .&. 0x3F) 6
                                       case imm_flag /= 0 of
                                        True -> do let imm5 = sign_extend (instr .&. 0x1F) 5
                                                   rg0 <- readArray registers' r0
                                                   rg1 <- readArray registers' r1
                                                   writeArray registers' rg0 (rg1+imm5)

                                        False -> do let r2 = instr .&. 0x7
                                                    rg0 <- readArray registers' r0
                                                    rg1 <- readArray registers' r1
                                                    writeArray registers' r0 (rg1+r2)

                                       putStrLn "have to fix update_flags"
                                                        
                            LD   -> do let r0 = (instr `shiftR` 9) .&. 0x7
                                       let pc_offset = sign_extend (instr .&. 0x1FF) 9
                                       --mem_read
                                       --update_flags
                                       pure ()

                            STR  -> do let r0 = (instr `shiftR` 9) .&. 0x7
                                       let pc_offset = sign_extend (instr .&. 0x1FF) 9
                                       -- mem_write
                                       pure ()

                            JSR  -> do let long_flag = (instr `shiftR` 11) .&. 1
                                       r_pc <- readArray registers' (cast RPC)
                                       writeArray registers' (cast R7) r_pc
                                       case long_flag /= 0 of
                                        True  -> do let long_pc_offset = sign_extend (instr .&. 0x7FF) 11
                                                    writeArray registers' (cast RPC) (r_pc+long_pc_offset)

                                        False -> do let r1 = (instr `shiftR` 6) .&. 0x7
                                                    r1' <- readArray registers' r1
                                                    writeArray registers' (cast RPC) r1' 

                            AND  -> do let imm_flag = (instr `shiftR` 5) .&. 0x1
                                       let r0 = (instr `shiftR` 9) .&. 0x7
                                       let r1 = (instr `shiftR` 6) .&. 0x7
                                       case (imm_flag /= 0) of
                                        True -> do let imm5 = sign_extend (instr .&. 0x1F) 5
                                                   rg1 <- readArray registers' r1
                                                   writeArray registers' r0 (rg1 .&. imm5)

                                        False -> do let r2 = instr .&. 0x7
                                                    rg1 <- readArray registers' r1
                                                    rg2 <- readArray registers' r2
                                                    writeArray registers' r0 (rg1 .&. rg2)

                                       putStrLn "have to update fix update_flags"

                            LDR  -> do let r0 = (instr `shiftR` 9) .&. 0x7
                                       let r1 = (instr `shiftR` 6) .&. 0x7
                                       let offset = sign_extend (instr .&. 0x3F) 6
                                       -- mem_read
                                       -- update_flags
                                       pure ()

                            STRR -> do let r0 = (instr `shiftR` 9) .&. 0x7
                                       let r1 = (instr `shiftR` 6) .&. 0x7
                                       let offset = sign_extend (instr .&. 0x3F) 6
                                       --mem_write
                                       pure ()

                            RTI  -> pure ()

                            NOT  -> do let r0 = (instr `shiftR` 9) .&. 0x7
                                       let r1 = (instr `shiftR` 6) .&. 0x7
                                       rg1 <- readArray registers' r1
                                       let rg1' = complement rg1   
                                       writeArray registers' r0 rg1
                                       putStrLn "fix update_flags"

                            LDI  -> do let r0 = (instr `shiftR` 9) .&. 0x7
                                       let pc_offset = sign_extend (instr .&. 0x1FF) 9
                                       r_pc <- readArray registers' (cast RPC)
                                       --writeArray registers' r0 (mem_read (mem_read (r_pc + pc_offset)))
                                       putStrLn "have to fix mem_read"
                                       putStrLn "have to fix update_flags"

                            STRI  -> do let r0 = (instr `shiftR` 9) .&. 0x7
                                        let pc_offset = sign_extend (instr .&. 0x1FF) 9
                                        -- mem_write (mem_read
                                        pure ()

                            JMP  -> do let r1 = (instr `shiftR` 6) .&. 0x7
                                       rg1 <- readArray registers' r1
                                       writeArray registers' (cast RPC) rg1

                            RES  -> pure ()

                            LEA  -> do let r0 = (instr `shiftR` 9) .&. 0x7
                                       let pc_offset = sign_extend (instr .&. 0x1FF) 9
                                       r_pc <- readArray registers' (cast RPC)
                                       writeArray registers' r0 (pc_offset + r_pc)
                                       -- update_flags
                                       
                            TRAP -> do case makeTrap (instr .&. 0xFF) of
                                        Getc  -> do r <- fromIntegral . ord <$> getChar
                                                    writeArray registers' (cast R0) r

                                        Out   -> do putChar =<< 
                                                     chr . fromIntegral <$>
                                                      readArray registers' (cast R0)

                                        Puts  -> do let r0 = (instr `shiftR` 9) .&. 9
                                                    let loop c = do
                                                        --mem_read
                                                        unless (c == 0x0000) $ do   
                                                            putChar (chr c)
                                                            loop (c+1)
                                                            hFlush stdout
                                                    loop $ fromIntegral r0

                                        In    -> undefined
                                        PutsP -> undefined
                                        Halt  -> putStrLn "HALT"
                -- loop here: go updated_registers updated_memory, updated_pc

read_image_file :: String -> IO (Memory) 
read_image_file file = do (origin:bytes) <- process . B.unpack <$> B.readFile file
                          let pad = replicate (fromIntegral origin - 1) (0x0 :: Word16)
                              mid = (origin:bytes)
                              new_size = memory_size - (length pad + length mid)
                              end = replicate new_size (0x0 :: Word16)
                              final = zip [0..memory_size-1] (pad ++ mid ++ end)
                          pure $ array (0,memory_size-1) final
                        
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
          forever $ do 
              (heap', instr) <- mem_read (regs'!(cast RPC)) heap
              let op = instr `shiftR` 12
              (go regs' heap')
          putStrLn "Done!"
