{-# LANGUAGE TypeFamilies #-}

module Listing (Listing (..), listing) where

import ARMCortexM0
import Control.Monad

data Listing a = Listing a [String] deriving (Eq, Show)

listing :: Listing a -> String
listing (Listing _ ss) = unlines ss

instance Monad Listing where
    return a = Listing a []
    Listing a ss >>= f = Listing result (ss ++ ss')
      where
        Listing result ss' = f a

instance ARMCortexM0 Listing where
    data Register Listing = Register String
    pc = Register "pc"
    load = MemoryOperation "load"
    store = MemoryOperation "store"
    --readMemory address = Listing 0 ["readMemory " ++ show address]
    --writeMemory address value = Listing () ["writeMemory " ++ show address ++ " " ++ show value]
    memoryUnit address (Register register) (MemoryOperation mOp) = Listing ("Using memory" ++ address ++ " " ++ register ++ " " ++ show mOp)
    readRegister (Register register) = Listing 0 ["readRegister " ++ register]
    writeRegister (Register register) value = Listing () ["writeRegister " ++ register ++ " " ++ show value]
