--
-- Epanet.hs: EPANET Toolkit module in Haskell
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
{-# LANGUAGE ForeignFunctionInterface #-}
module Epanet (open, saveInpFile, close, solveH, saveH, openH, getVersion) where

import Foreign
import Foreign.C
import Foreign.Ptr (Ptr, nullPtr)

en_ELEVATION = 0 -- Node parameters
en_BASEDEMAND = 1
en_PATTERN = 2
en_EMITTER = 3
en_INITQUAL = 4
en_SOURCEQUAL = 5
en_SOURCEPAT = 6
en_SOURCETYPE = 7
en_TANKLEVEL = 8
en_DEMAND = 9
en_HEAD = 10
en_PRESSURE = 11
en_QUALITY = 12
en_SOURCEMASS = 13
en_INITVOLUME = 14
en_MIXMODEL = 15
en_MIXZONEVOL = 16

en_TANKDIAM = 17
en_MINVOLUME = 18
en_VOLCURVE = 19
en_MINLEVEL = 20
en_MAXLEVEL = 21
en_MIXFRACTION = 22
en_TANK_KBULK = 23

en_DIAMETER = 0 -- Link parameters
en_LENGTH = 1
en_ROUGHNESS = 2
en_MINORLOSS = 3
en_INITSTATUS = 4
en_INITSETTING = 5
en_KBULK = 6
en_KWALL = 7
en_FLOW = 8
en_VELOCITY = 9
en_HEADLOSS = 10
en_STATUS = 11
en_SETTING = 12
en_ENERGY = 13

en_DURATION = 0 -- Time parameters
en_HYDSTEP = 1
en_QUALSTEP = 2
en_PATTERNSTEP = 3
en_PATTERNSTART= 4
en_REPORTSTEP = 5
en_REPORTSTART = 6
en_RULESTEP = 7
en_STATISTIC = 8
en_PERIODS = 9

en_NODECOUNT = 0 -- Component counts
en_TANKCOUNT = 1
en_LINKCOUNT = 2
en_PATCOUNT = 3
en_CURVECOUNT = 4
en_CONTROLCOUNT= 5

en_JUNCTION = 0 -- Node types
en_RESERVOIR = 1
en_TANK = 2

en_CVPIPE = 0 -- Link types.
en_PIPE = 1 -- See LinkType in TYPES.H
en_PUMP = 2
en_PRV = 3
en_PSV = 4
en_PBV = 5
en_FCV = 6
en_TCV = 7
en_GPV = 8

en_NONE = 0 -- Quality analysis types.
en_CHEM = 1 -- See QualType in TYPES.H
en_AGE = 2
en_TRACE = 3

en_CONCEN = 0 -- Source quality types.
en_MASS = 1 -- See SourceType in TYPES.H.
en_SETPOINT = 2
en_FLOWPACED = 3

en_CFS = 0 -- Flow units types.
en_GPM = 1 -- See FlowUnitsType
en_MGD = 2 -- in TYPES.H.
en_IMGD = 3
en_AFD = 4
en_LPS = 5
en_LPM = 6
en_MLD = 7
en_CMH = 8
en_CMD = 9

en_TRIALS = 0 -- Misc. options
en_ACCURACY = 1
en_TOLERANCE = 2
en_EMITEXPON = 3
en_DEMANDMULT = 4

en_LOWLEVEL = 0 -- Control types.
en_HILEVEL = 1 -- See ControlType
en_TIMER = 2 -- in TYPES.H.
en_TIMEOFDAY = 3

en_AVERAGE = 1 -- Time statistic types.
en_MINIMUM = 2 -- See TstatType in TYPES.H
en_MAXIMUM = 3
en_RANGE = 4

en_MIX1 = 0 -- Tank mixing models
en_MIX2 = 1
en_FIFO = 2
en_LIFO = 3

en_NOSAVE = 0 -- Save-results-to-file flag
en_SAVE = 1

en_INITFLOW =10 -- Re-initialize flows flag

foreign import ccall unsafe "toolkit.h ENopen" c_ENopen :: CString -> CString -> CString -> CInt
open :: String -> String -> String -> Int
open f1 f2 f3 = unsafePerformIO $
  withCString f1 $ \cf1 ->
    withCString f2 $ \cf2 ->
      withCString f3 $ \cf3 ->
        return $ fromIntegral $ c_ENopen cf1 cf2 cf3

foreign import ccall unsafe "toolkit.h ENsaveinpfile" c_ENsaveinpfile :: CString -> CInt
saveInpFile :: String -> Int
saveInpFile f1 = unsafePerformIO $
  withCString f1 $ \cf1 ->
    return $ fromIntegral $ c_ENsaveinpfile cf1

foreign import ccall unsafe "toolkit.h ENclose" c_ENclose :: CInt
close :: Int
close = unsafePerformIO $
  return $ fromIntegral $ c_ENclose

foreign import ccall unsafe "toolkit.h ENsolveH" c_ENsolveH :: CInt
solveH :: Int
solveH = unsafePerformIO $
  return $ fromIntegral $ c_ENsolveH

foreign import ccall unsafe "toolkit.h ENsaveH" c_ENsaveH :: CInt
saveH :: Int 
saveH = unsafePerformIO $
  return $ fromIntegral $ c_ENsaveH

foreign import ccall unsafe "toolkit.h ENopenH" c_ENopenH :: CInt
openH :: Int 
openH = unsafePerformIO $
  return $ fromIntegral $ c_ENopenH

-- int ENinitH(int)

-- int errcode ENrunH(long *t)
-- runH :: (Int, Long)

-- int errcode ENnextH(long *t)
-- nextH :: (Int, Long)

foreign import ccall unsafe "toolkit.h ENgetversion" c_ENgetversion :: Ptr CInt -> CInt
getVersion = unsafePerformIO $
  alloca $ \vptr -> do
    if 0 == (c_ENgetversion vptr)
      then do 
        v <- peek vptr
        return $ fromIntegral v
      else do
        return 0
