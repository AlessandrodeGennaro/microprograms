import ARMCortexM0
import Simulation

import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as Map

program :: Simulation ()
program = do

	-- read/write register test
	writeRegister (R 0) 100
	value <- readRegister (R 0)
	increment (R 0)
	writeRegister (R 1) value
	writeRegister (R 2) 500

	-- memoryUnit store test
        memoryUnit 0 (R 2) store

	-- aluRegs test
	value2 <- alu (R 0) (R 1) 0 addRegs
	writeRegister (R 3) value2

	-- aluMem test
	value3 <- alu (R 0) (R 1) 0 addMem
	writeRegister (R 4) value3

	-- memoryUnit load test
	memoryUnit 0 (R 6) load

	--let programAddress = 10000
	--writeRegister pc programAddress
	--writeMemory (programAddress + 1) 100
	--writeMemory (programAddress + 2) 222

	--load (R 1)

processor = Processor (Map.empty) (Map.empty)

main = do
	putStrLn $ "\nInitial processor:\n" ++ show processor ++ "\n"
	let (value, newProcessor) = simulate program processor
	putStrLn $ "Value = " ++ show value ++ "\n"
	putStrLn $ "New processor:\n" ++ show newProcessor ++ "\n"
