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
module Epanet (open, getVersion) where

import Foreign
import Foreign.C
import Foreign.Ptr (Ptr, nullPtr)

EN_ELEVATION   = 0    -- Node parameters
EN_BASEDEMAND  = 1
EN_PATTERN     = 2
EN_EMITTER     = 3
EN_INITQUAL    = 4
EN_SOURCEQUAL  = 5
EN_SOURCEPAT   = 6
EN_SOURCETYPE  = 7
EN_TANKLEVEL   = 8
EN_DEMAND      = 9
EN_HEAD        = 10
EN_PRESSURE    = 11
EN_QUALITY     = 12
EN_SOURCEMASS  = 13
EN_INITVOLUME  = 14
EN_MIXMODEL    = 15
EN_MIXZONEVOL  = 16

EN_TANKDIAM    = 17
EN_MINVOLUME   = 18
EN_VOLCURVE    = 19
EN_MINLEVEL    = 20
EN_MAXLEVEL    = 21
EN_MIXFRACTION = 22
EN_TANK_KBULK  = 23

EN_DIAMETER    = 0    -- Link parameters
EN_LENGTH      = 1
EN_ROUGHNESS   = 2
EN_MINORLOSS   = 3
EN_INITSTATUS  = 4
EN_INITSETTING = 5
EN_KBULK       = 6
EN_KWALL       = 7
EN_FLOW        = 8
EN_VELOCITY    = 9
EN_HEADLOSS    = 10
EN_STATUS      = 11
EN_SETTING     = 12
EN_ENERGY      = 13

EN_DURATION    = 0    -- Time parameters
EN_HYDSTEP     = 1
EN_QUALSTEP    = 2
EN_PATTERNSTEP = 3
EN_PATTERNSTART= 4
EN_REPORTSTEP  = 5
EN_REPORTSTART = 6
EN_RULESTEP    = 7
EN_STATISTIC   = 8
EN_PERIODS     = 9

EN_NODECOUNT   = 0   -- Component counts
EN_TANKCOUNT   = 1
EN_LINKCOUNT   = 2
EN_PATCOUNT    = 3
EN_CURVECOUNT  = 4
EN_CONTROLCOUNT= 5

EN_JUNCTION    = 0    -- Node types
EN_RESERVOIR   = 1
EN_TANK        = 2

EN_CVPIPE      = 0    -- Link types.
EN_PIPE        = 1    -- See LinkType in TYPES.H
EN_PUMP        = 2
EN_PRV         = 3
EN_PSV         = 4
EN_PBV         = 5
EN_FCV         = 6
EN_TCV         = 7
EN_GPV         = 8

EN_NONE        = 0    -- Quality analysis types.
EN_CHEM        = 1    -- See QualType in TYPES.H
EN_AGE         = 2
EN_TRACE       = 3

EN_CONCEN      = 0    -- Source quality types.     
EN_MASS        = 1    -- See SourceType in TYPES.H.
EN_SETPOINT    = 2
EN_FLOWPACED   = 3

EN_CFS         = 0    -- Flow units types.  
EN_GPM         = 1    -- See FlowUnitsType  
EN_MGD         = 2    -- in TYPES.H.        
EN_IMGD        = 3
EN_AFD         = 4
EN_LPS         = 5
EN_LPM         = 6
EN_MLD         = 7
EN_CMH         = 8
EN_CMD         = 9

EN_TRIALS      = 0   -- Misc. options
EN_ACCURACY    = 1
EN_TOLERANCE   = 2
EN_EMITEXPON   = 3
EN_DEMANDMULT  = 4

EN_LOWLEVEL    = 0   -- Control types. 
EN_HILEVEL     = 1   -- See ControlType
EN_TIMER       = 2   -- in TYPES.H.    
EN_TIMEOFDAY   = 3

EN_AVERAGE     = 1   -- Time statistic types.   
EN_MINIMUM     = 2   -- See TstatType in TYPES.H
EN_MAXIMUM     = 3
EN_RANGE       = 4

EN_MIX1        = 0   -- Tank mixing models
EN_MIX2        = 1
EN_FIFO        = 2
EN_LIFO        = 3

EN_NOSAVE      = 0   -- Save-results-to-file flag
EN_SAVE        = 1

EN_INITFLOW    =10   -- Re-initialize flows flag 

foreign import ccall unsafe "toolkit.h ENopen" c_ENopen :: CString -> CString -> CString -> CInt
open :: String -> String -> String -> Int
open f1 f2 f3 = unsafePerformIO $
  withCString f1 $ \cf1 ->
    withCString f2 $ \cf2 ->
      withCString f3 $ \cf3 ->
        return $ fromIntegral $ c_ENopen cf1 cf2 cf3

foreign import ccall unsafe "toolkit.h ENsaveinpfile" c_ENsaveinpfile :: CString -> CInt
saveInpFile :: String -> Int
open f1 = unsafePerformIO $
  withCString f1 $\cf1 ->
    return $ fromIntegral $ c_ENsaveinpfile cf1

foreign import ccall unsafe "toolkit.h Enclose" c_ENclose :: CInt
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
