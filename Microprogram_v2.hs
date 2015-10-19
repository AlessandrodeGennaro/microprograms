{-# LANGUAGE TypeFamilies #-}

module Microprogram_v2 (
	module Control.Monad,
    Microprogram_v2 (..), Address, Value,
    increment, decrement, fetchAddressImmediate, fetchInstruction,
    incAndFetchInstruction, pop, push
    ) where

import Control.Monad

-- Type synonyms let us avoid the pain of converting values to addresses, yet improve readability
type Address = Int
type Value   = Int

class Monad m => Microprogram_v2 m where
    data Register m
    data ComputationType m
    pc, ir, sp            :: Register m
    addRegs               :: Register m -> Register m -> ComputationType m
    incReg, decReg        :: Register m -> ComputationType m
    readMemory            :: Address -> Register m -> m ()
    writeMemory           :: Address -> Register m -> m ()
    readRegister          :: Register m -> m Value
    writeRegister         :: Register m -> Value -> m ()
    alu                   :: ComputationType m -> m Value

-- Increment the value stored in a register
increment :: Microprogram_v2 m => Register m -> m ()
increment register = do
    incValue <- alu (incReg register)
    writeRegister register incValue

-- Decrement the value stored in a register
decrement :: Microprogram_v2 m => Register m -> m ()
decrement register = do
    decValue <- alu (decReg register)
    writeRegister register decValue

-- Increment the program counter and fetch the address where the offset is stored
fetchAddressImmediate :: Microprogram_v2 m => m Address
fetchAddressImmediate = do
    increment pc
    readRegister pc

-- Fetch the next instruction opcode and store it in the opcode register
fetchInstruction :: Microprogram_v2 m => m ()
fetchInstruction = do
    addressNextInstruction <- readRegister pc
    readMemory addressNextInstruction ir

-- Fetch the next instruction opcode and store it in the opcode register
incAndFetchInstruction :: Microprogram_v2 m => m ()
incAndFetchInstruction = do
    addressNextInstruction <- fetchAddressImmediate
    readMemory addressNextInstruction ir

-- Pop operation. Memory pointed by the Stack Pointer
pop :: Microprogram_v2 m => Register m -> m ()
pop register = do
    decrement sp
    spAddress <- readRegister sp
    readMemory spAddress register

-- Push operation. The value is stored into memory pointed by the Stack Pointer
push :: Microprogram_v2 m => Register m -> m ()
push register = do
    stackAddress <- readRegister sp
    writeMemory stackAddress register
    increment sp
