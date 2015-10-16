import ARMCortexM0
import Simulation

import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as Map

program :: Simulation ()
program = do
	--writeMemory 100 200
	writeRegister (R 0) 1234
	value <- readRegister (R 0)
	writeRegister (R 1) (value + 1)
	
	-- memoryUnit test
        memoryUnit 0 (R 1) store
        

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
