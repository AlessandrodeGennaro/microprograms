{-# LANGUAGE TypeFamilies #-}

module Simulation (Processor (..), Simulation (..), Register (..),
                    ComputationType (..), MemoryOperation (..), simulate) where

import ARMCortexM0
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as Map

data Processor = Processor { memory :: IntMap Value, registerBank :: IntMap Value }

lookupRegister :: Processor -> Register Simulation -> Value
lookupRegister (Processor _ regs) r =
    Map.findWithDefault 0 (registerID r) regs

updateRegister :: Processor -> Register Simulation -> Value -> Processor
updateRegister (Processor mem regs) r v =
    Processor mem $ Map.insert (registerID r) v regs

lookupMemory :: Processor -> Address -> Value
lookupMemory (Processor mem _) a =
    Map.findWithDefault 0 a mem

updateMemory :: Processor -> Address -> Value -> Processor
updateMemory (Processor mem regs) a v =
    Processor (Map.insert a v mem) regs

instance Show Processor where
    show (Processor mem regs) = "Memory = " ++ show mem ++ "\nRegister bank = "
                                ++ show regs ++ "\n"

data Simulation a = Simulation (Processor -> (a, Processor))

simulate :: Simulation a -> Processor -> (a, Processor)
simulate (Simulation f) processor = f processor

instance Functor Simulation where
    fmap f (Simulation s) = Simulation $ \p ->
                              let (a, p') = s p
                              in (f a, p')

--instance Applicative Simulation where
--    pure a = Simulation $ \p -> (a, p)
--    Simulation f <*> Simulation a = Simulation $ \p ->
--                                      let (g, p') = f p
--                                          (b, p'') = a p'
--                                      in (g b, p'')

instance Monad Simulation where
    --return = pure
    Simulation f >>= g = Simulation $ \p ->
                          let
                            (a, p') = f p
                            Simulation h = g a
                          in h p'

instance ARMCortexM0 Simulation where
    data Register Simulation = R Int | RegisterPC | RegisterIR | RegisterSP
    data ComputationType Simulation =
          AddRegs (Register Simulation) (Register Simulation)
        | SubRegs (Register Simulation) (Register Simulation)
        | AddMem (Register Simulation) Address
        | SubMem (Register Simulation) Address
        | AddImm (Register Simulation)
        | SubImm (Register Simulation)
        | IncReg (Register Simulation)
        | DecReg (Register Simulation)
    data MemoryOperation Simulation = LoadOp | StoreOp | BurstLoad | BurstStore
    -- standard registers
    pc         = RegisterPC
    ir         = RegisterIR
    sp         = RegisterSP
    -- operations between registers
    addRegs    = AddRegs
    subRegs    = SubRegs
    -- operations between register and memory (2nd operand)
    addMem     = AddMem
    subMem     = SubMem
    -- operations between register and immediate (inside IR reg)
    addImm     = AddImm
    subImm     = SubImm
    -- operations with one register
    incReg     = IncReg
    decReg     = DecReg
    -- memory operations
    load       = LoadOp
    store      = StoreOp
    burstLoad  = BurstLoad
    burstStore = BurstStore

--    readMemory address = Simulation $
--        \(Processor mem regs) -> (Map.findWithDefault 0 address mem, Processor mem regs)

--    writeMemory address value = Simulation $
--        \(Processor mem regs) -> ((), Processor (Map.insert address value mem) regs)

    -- read a register and return the value stored
    readRegister register = Simulation $ \p -> (lookupRegister p register, p)

    -- write a valure into a register
    writeRegister register value = Simulation $
        \p -> ((), updateRegister p register value)

    -- emulate memory unit behaviour. Supports following operations:
    memoryUnit address register mOp = Simulation $
        \p @ (Processor mem regs) -> case (mOp) of
            -- Load operation
            LoadOp -> ((), updateRegister p register $ lookupMemory p address)
            -- Store operation
            StoreOp ->  ((), updateMemory p address $ lookupRegister p register)
            BurstLoad ->  ((), Processor mem regs) -- TODO implementing burst load
            BurstStore ->  ((), Processor mem regs) -- TODO implementing burst store

    -- emulate alu behaviour. Supports following operations:
    alu cType = Simulation $
        \p -> case (cType) of
            -- REG1 + REG2 operation
            (AddRegs reg1 reg2) -> ((lookupRegister p reg1) +
                                    (lookupRegister p reg2), p)
            -- REG1 - REG2 operation
            (SubRegs reg1 reg2) -> ((lookupRegister p reg1) -
                                    (lookupRegister p reg2), p)
            -- REG + MEM operation
            (AddMem reg1 addr) -> ((lookupRegister p reg1) +
                                    (lookupMemory p addr), p)
            -- REG - MEM operation
            (SubMem reg1 addr) -> ((lookupRegister p reg1) -
                                    (lookupMemory p addr), p)
            -- REG + IMM operation
            (AddImm reg) -> ((lookupRegister p reg) +
                            (lookupMemory p (lookupRegister p pc)), p)
            -- REG - IMM operation
            (SubImm reg) -> ((lookupRegister p reg) -
                            (lookupMemory p (lookupRegister p pc)), p)
            -- REG increment operation
            (IncReg reg) -> ((lookupRegister p reg) + 1,p)
            -- REG decrement operation
            (DecReg reg) -> ((lookupRegister p reg) + 1,p)

registerID :: Register Simulation -> Int
registerID (R n)          = n
registerID RegisterPC     = -1
registerID RegisterIR     = -2
registerID RegisterSP     = -3
