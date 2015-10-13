{-# LANGUAGE TypeFamilies #-}

module Microprogram (
	module Control.Monad,
    Microprogram (..), Address (..), Value (..),
    increment, fetchArgument, fetchNextOpcode, load, store, jump
    ) where

import Control.Monad

-- Type synonyms let us avoid the pain of converting values to addresses, yet improve readability
type Address = Int
type Value   = Int

-- Enumerated types for computational unit
-- data ComputationType = Add | Sub | Mul | Div | Shl | Shr deriving (Enum)

class Monad m => Microprogram m where
    data Register m
    data ComputationType m
    pc, opcode    :: Register m
    add           :: ComputationType m
    readMemory    :: Address -> m Value
    writeMemory   :: Address -> Value -> m ()
    readRegister  :: Register m -> m Value
    writeRegister :: Register m -> Value -> m ()
    compute       :: Register m -> Register m -> ComputationType m -> m Value

-- Increment the value stored in a register
increment :: Microprogram m => Register m -> m ()
increment register = do
    value <- readRegister register
    writeRegister register (value + 1) -- TODO: change flags

-- Increment the program counter and fetch the value it points to; used for
-- fetching instruction opcodes and immediate arguments
fetchArgument :: Microprogram m => m Value
fetchArgument = do
    increment pc
    address <- readRegister pc
    readMemory address

-- Fetch the next instruction opcode and store it in the opcode register
fetchNextOpcode :: Microprogram m => m ()
fetchNextOpcode = do
    value <- fetchArgument
    writeRegister opcode value

-- Model behaviour of the Arithmetic Logic Unit
-- compute :: Microprogram m => Register m -> Register m -> ComputationType m -> m Value
-- compute = alu

-- register1 register2 cType = do
--     operand1 <- readRegister register1
--     operand2 <- readRegister register2
--     alu operand1 operand2 cType

-- ALU operation Rn to Rn (PCIU -> IFU ALU)
rnToRn :: Microprogram m => Register m -> Register m -> ComputationType m -> m ()
rnToRn register1 register2 cType = do
    result <- compute register1 register2 cType
    writeRegister register1 result
    fetchNextOpcode

-- LDRB (register) according to ARMv6-M instruction specifications 
loadRegisterByte :: Microprogram m => Register m -> Register m -> Register m -> m ()
loadRegisterByte register1 register2 register3 = do		-- Ignore Offset Shift, because (shift_t, shift_n) = (SRType_LSL, 0); and if amount is 0 there is no shift shift_n = amount
    address <- compute register1 register2 add -- Definitely add since, add = TRUE in specifications
    value <- readMemory address
    writeRegister register3 value
    fetchNextOpcode
