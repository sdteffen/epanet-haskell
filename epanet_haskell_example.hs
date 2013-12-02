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
  putStrLn $ "getVersion: " ++ (show getVersion)
  putStrLn $ "open 'Net1.inp' 'Net1.txt' 'Net1.bin': " ++ show (open "Net1.inp" "Net1.txt" "Net1.bin")

