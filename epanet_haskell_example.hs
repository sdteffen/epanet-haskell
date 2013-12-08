--
-- epanet_haskell_example.hs: Example to illustrate EPANET Toolkit usage in Haskell
--
-- Author:
--   Steffen Macke (sdteffen@sdteffen.de)
--
-- Copyright (C) 2013 Steffen Macke (http://epanet.de)
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, version 3 of the License
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see <http:--www.gnu.org/licenses/>.
--

module Main where

import Epanet

main = do
  putStrLn $ "getVersion = " ++ show getVersion
  putStrLn $ "open 'Net1.inp' 'Net1.txt' 'Net1.bin' = " ++ show (open "Net1.inp" "Net1.txt" "Net1.bin")
  putStrLn $ "openH = " ++ show openH
  putStrLn $ "initH en_SAVE = " ++ show (initH en_SAVE)
  putStrLn $ "nextH = " ++ show nextH
  putStrLn $ "runH = " ++ show runH
  putStrLn $ "solveH = " ++ show solveH
  putStrLn $ "saveHydFile 'Net1.tmp' = " ++ show (saveHydFile "Net1.tmp")
  putStrLn $ "useHydFile 'Net1.tmp' = " ++ show (useHydFile "Net1.tmp")
  putStrLn $ "getControl 1 = " ++ show (getControl 1)
  putStrLn $ "getCount en_NODECOUNT = " ++ show (getCount en_NODECOUNT)
  putStrLn $ "getOption en_DEMANDMULT = " ++ show (getOption en_DEMANDMULT)
  putStrLn $ "getTimeParam en_PERIODS = " ++ show (getTimeParam en_PERIODS)
  putStrLn $ "getFlowUnits = " ++ show getFlowUnits
  putStrLn $ "getPatternIndex '1' = " ++ show (getPatternIndex "1")
  putStrLn $ "getPatternId 1 = " ++ show (getPatternId 1)
  putStrLn $ "getPatternLen 1 = " ++ show (getPatternLen 1)
  putStrLn $ "getPatternValue 1 2 = " ++ show (getPatternValue 1 2)
  putStrLn $ "getNodeIndex '11' = " ++ show (getNodeIndex "11")
  putStrLn $ "getNodeId 2 = " ++ show (getNodeId 2)
  putStrLn $ "getNodeType 2 = " ++ show (getNodeType 2)
  putStrLn $ "getNodeValue 2 en_PRESSURE = " ++ show (getNodeValue 2 en_PRESSURE)
  putStrLn $ "getLinkIndex '122' = " ++ show (getLinkIndex "122")
  putStrLn $ "getLinkId 12 = " ++ show (getLinkId 12)
  putStrLn $ "getLinkType 12 = " ++ show (getLinkType 12)
  putStrLn $ "getLinkNodes 12 = " ++ show (getLinkNodes 12)
  putStrLn $ "getLinkValue 12 en_FLOW = " ++ show (getLinkValue 12 en_FLOW)
  putStrLn $ "getNodeIndex '2' = " ++ show (getNodeIndex "2")
  putStrLn $ "setControl 1 en_PIPE 12 0.0 11 105.0 = " ++ show (setControl 1 en_PIPE 12 0.0 11 105.0)
  putStrLn $ "setNodeValue 1 en_BASEDEMAND 1.5 = " ++ show (setNodeValue 1 en_BASEDEMAND 1.5)
  putStrLn $ "getNodeValue 1 en_BASEDEMAND = " ++ show (getNodeValue 1 en_BASEDEMAND)
  putStrLn $ "setLinkValue 1 en_DIAMETER 20 = " ++ show (setLinkValue 1 en_DIAMETER 20)
  putStrLn $ "addPattern 'haskell' = " ++ show (addPattern "Haskell")
  putStrLn $ "setPatternValue 2 1 1.1 = " ++ show (setPatternValue 2 1 1.1)
  putStrLn $ "openQ = " ++ show openQ
  putStrLn $ "initQ en_NOSAVE = " ++ show (initQ en_NOSAVE)
  putStrLn $ "runQ = " ++ show runQ
  putStrLn $ "nextQ = " ++ show nextQ
  putStrLn $ "stepQ = " ++ show stepQ
  putStrLn $ "solveQ = " ++ show solveQ
  putStrLn $ "getQualType = " ++ show getQualType
  putStrLn $ "resetReport = " ++ show resetReport
  putStrLn $ "writeLine 'Hello, Haskell' = " ++ show (writeLine "Hello, Haskell")
  putStrLn $ "close = " ++ show close
  putStrLn $ "setReport 'Net1copy.txt' = " ++ show (setReport "Net1copy.txt")
  putStrLn $ "saveInpFile 'Net1copy.inp' = " ++ show (saveInpFile "Net1copy.inp")
  putStrLn $ "getError 2 80 = " ++ show (getError 2 80)

