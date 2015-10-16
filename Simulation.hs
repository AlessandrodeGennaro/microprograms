{-# LANGUAGE TypeFamilies #-}

module Simulation (Processor (..), Simulation (..), Register (..), ComputationType (..), MemoryOperation (..), simulate) where

import ARMCortexM0
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as Map

data Processor = Processor { memory :: IntMap Value, registerBank :: IntMap Value }

instance Show Processor where
    show (Processor mem regs) = "Memory = " ++ show mem ++ "\nRegister bank = " ++ show regs ++ "\n"

data Simulation a = Simulation (Processor -> (a, Processor))

simulate :: Simulation a -> Processor -> (a, Processor)
simulate (Simulation f) processor = f processor

instance Monad Simulation where
    return a = Simulation $ \p -> (a, p)
    Simulation f >>= g = Simulation $ \p ->
                          let
                            (a, p') = f p
                            Simulation h = g a
                          in h p'

instance ARMCortexM0 Simulation where
    data Register Simulation = R Int | RegisterPC | RegisterOpcode | RegisterSP
    data ComputationType Simulation = AddOp | SubOp
    data MemoryOperation Simulation = LoadOp | StoreOp | BurstLoad | BurstStore
    pc         = RegisterPC
    ir         = RegisterOpcode
    sp         = RegisterSP
    add        = AddOp
    sub        = SubOp
    load       = LoadOp
    store      = StoreOp
    burstLoad  = BurstLoad
    burstStore = BurstStore

--    readMemory address = Simulation $
--        \(Processor mem regs) -> (Map.findWithDefault 0 address mem, Processor mem regs)

--    writeMemory address value = Simulation $
--        \(Processor mem regs) -> ((), Processor (Map.insert address value mem) regs)

    readRegister register = Simulation $
        \(Processor mem regs) -> (Map.findWithDefault 0 (registerID register) regs, Processor mem regs)

    writeRegister register value = Simulation $
        \(Processor mem regs) -> ((), Processor mem (Map.insert (registerID register) value regs))

    memoryUnit address register mOp = Simulation $
	\(Processor mem regs) -> case (mOp) of 
					-- Load operation
					LoadOp -> ((), Processor mem (Map.insert (registerID register) (Map.findWithDefault 0 address mem) regs))
					-- Store operation
					StoreOp ->  ((), Processor (Map.insert address (Map.findWithDefault 0 (registerID register) regs) mem) regs)
					BurstLoad ->  ((), Processor mem regs) -- TODO implementating burst load
					BurstStore ->  ((), Processor mem regs) -- TODO implementating burst store

registerID :: Register Simulation -> Int
registerID (R n)          = n
registerID RegisterPC     = -1
registerID RegisterOpcode = -2
registerID RegisterSP     = -3
