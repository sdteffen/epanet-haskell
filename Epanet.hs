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
module Epanet where

import Foreign (alloca, Int64, peek)
import Foreign.C
import Foreign.Ptr (Ptr, nullPtr)
import System.IO.Unsafe (unsafePerformIO)

-- Required only for setPattern
-- import qualified Data.Vector.Storable as DVS

en_ELEVATION :: Int
en_ELEVATION = 0 -- Node parameters
en_BASEDEMAND :: Int
en_BASEDEMAND = 1
en_PATTERN :: Int
en_PATTERN = 2
en_EMITTER :: Int
en_EMITTER = 3
en_INITQUAL :: Int
en_INITQUAL = 4
en_SOURCEQUAL :: Int
en_SOURCEQUAL = 5
en_SOURCEPAT :: Int
en_SOURCEPAT = 6
en_SOURCETYPE :: Int
en_SOURCETYPE = 7
en_TANKLEVEL :: Int
en_TANKLEVEL = 8
en_DEMAND :: Int
en_DEMAND = 9
en_HEAD :: Int
en_HEAD = 10
en_PRESSURE :: Int
en_PRESSURE = 11
en_QUALITY :: Int
en_QUALITY = 12
en_SOURCEMASS :: Int
en_SOURCEMASS = 13
en_INITVOLUME :: Int
en_INITVOLUME = 14
en_MIXMODEL :: Int
en_MIXMODEL = 15
en_MIXZONEVOL :: Int
en_MIXZONEVOL = 16

en_TANKDIAM :: Int
en_TANKDIAM = 17
en_MINVOLUME :: Int
en_MINVOLUME = 18
en_VOLCURVE :: Int
en_VOLCURVE = 19
en_MINLEVEL :: Int
en_MINLEVEL = 20
en_MAXLEVEL :: Int
en_MAXLEVEL = 21
en_MIXFRACTION :: Int
en_MIXFRACTION = 22
en_TANK_KBULK :: Int
en_TANK_KBULK = 23

en_DIAMETER :: Int
en_DIAMETER = 0 -- Link parameters
en_LENGTH :: Int
en_LENGTH = 1
en_ROUGHNESS :: Int
en_ROUGHNESS = 2
en_MINORLOSS :: Int
en_MINORLOSS = 3
en_INITSTATUS :: Int
en_INITSTATUS = 4
en_INITSETTING :: Int
en_INITSETTING = 5
en_KBULK :: Int
en_KBULK = 6
en_KWALL :: Int
en_KWALL = 7
en_FLOW :: Int
en_FLOW = 8
en_VELOCITY :: Int
en_VELOCITY = 9
en_HEADLOSS :: Int
en_HEADLOSS = 10
en_STATUS :: Int
en_STATUS = 11
en_SETTING :: Int
en_SETTING = 12
en_ENERGY :: Int
en_ENERGY = 13

en_DURATION :: Int
en_DURATION = 0 -- Time parameters
en_HYDSTEP :: Int
en_HYDSTEP = 1
en_QUALSTEP :: Int
en_QUALSTEP = 2
en_PATTERNSTEP :: Int
en_PATTERNSTEP = 3
en_PATTERNSTART :: Int
en_PATTERNSTART= 4
en_REPORTSTEP :: Int
en_REPORTSTEP = 5
en_REPORTSTART :: Int
en_REPORTSTART = 6
en_RULESTEP :: Int
en_RULESTEP = 7
en_STATISTIC :: Int
en_STATISTIC = 8
en_PERIODS :: Int
en_PERIODS = 9

en_NODECOUNT :: Int
en_NODECOUNT = 0 -- Component counts
en_TANKCOUNT :: Int
en_TANKCOUNT = 1
en_LINKCOUNT :: Int
en_LINKCOUNT = 2
en_PATCOUNT :: Int
en_PATCOUNT = 3
en_CURVECOUNT :: Int
en_CURVECOUNT = 4
en_CONTROLCOUNT :: Int
en_CONTROLCOUNT= 5

en_JUNCTION :: Int
en_JUNCTION = 0 -- Node types
en_RESERVOIR :: Int
en_RESERVOIR = 1
en_TANK :: Int
en_TANK = 2

en_CVPIPE :: Int
en_CVPIPE = 0 -- Link types.
en_PIPE :: Int
en_PIPE = 1 -- See LinkType in TYPES.H
en_PUMP :: Int
en_PUMP = 2
en_PRV :: Int
en_PRV = 3
en_PSV :: Int
en_PSV = 4
en_PBV :: Int
en_PBV = 5
en_FCV :: Int
en_FCV = 6
en_TCV :: Int
en_TCV = 7
en_GPV :: Int
en_GPV = 8

en_NONE :: Int
en_NONE = 0 -- Quality analysis types.
en_CHEM :: Int
en_CHEM = 1 -- See QualType in TYPES.H
en_AGE :: Int
en_AGE = 2
en_TRACE :: Int
en_TRACE = 3

en_CONCEN :: Int
en_CONCEN = 0 -- Source quality types.
en_MASS :: Int
en_MASS = 1 -- See SourceType in TYPES.H.
en_SETPOINT :: Int
en_SETPOINT = 2
en_FLOWPACED :: Int
en_FLOWPACED = 3

en_CFS :: Int
en_CFS = 0 -- Flow units types.
en_GPM :: Int
en_GPM = 1 -- See FlowUnitsType
en_MGD :: Int
en_MGD = 2 -- in TYPES.H.
en_IMGD :: Int
en_IMGD = 3
en_AFD :: Int
en_AFD = 4
en_LPS :: Int
en_LPS = 5
en_LPM :: Int
en_LPM = 6
en_MLD :: Int
en_MLD = 7
en_CMH :: Int
en_CMH = 8
en_CMD :: Int
en_CMD = 9

en_TRIALS :: Int
en_TRIALS = 0 -- Misc. options
en_ACCURACY :: Int
en_ACCURACY = 1
en_TOLERANCE :: Int
en_TOLERANCE = 2
en_EMITEXPON :: Int
en_EMITEXPON = 3
en_DEMANDMULT :: Int
en_DEMANDMULT = 4

en_LOWLEVEL :: Int
en_LOWLEVEL = 0 -- Control types.
en_HILEVEL :: Int
en_HILEVEL = 1 -- See ControlType
en_TIMER :: Int
en_TIMER = 2 -- in TYPES.H.
en_TIMEOFDAY :: Int
en_TIMEOFDAY = 3

en_AVERAGE :: Int
en_AVERAGE = 1 -- Time statistic types.
en_MINIMUM :: Int
en_MINIMUM = 2 -- See TstatType in TYPES.H
en_MAXIMUM :: Int
en_MAXIMUM = 3
en_RANGE :: Int
en_RANGE = 4

en_MIX1 :: Int
en_MIX1 = 0 -- Tank mixing models
en_MIX2 :: Int
en_MIX2 = 1
en_FIFO :: Int
en_FIFO = 2
en_LIFO :: Int
en_LIFO = 3

en_NOSAVE :: Int
en_NOSAVE = 0 -- Save-results-to-file flag
en_SAVE :: Int
en_SAVE = 1

en_INITFLOW :: Int
en_INITFLOW =10 -- Re-initialize flows flag

foreign import ccall unsafe "toolkit.h ENepanet" c_ENepanet :: CString -> CString -> CString -> Ptr CInt -> CInt
epanet :: String -> String -> String -> Int
epanet f1 f2 f3 = unsafePerformIO $
  withCString f1 $ \cf1 ->
    withCString f2 $ \cf2 ->
      withCString f3 $ \cf3 ->
        return $ fromIntegral $ c_ENepanet cf1 cf2 cf3 nullPtr
  
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

foreign import ccall unsafe "toolkit.h ENinitH" c_ENinitH :: CInt -> CInt
initH :: Int -> Int
initH flag = unsafePerformIO $
  return $ fromIntegral $ c_ENinitH (fromIntegral flag)

foreign import ccall unsafe "toolkit.h ENrunH" c_ENrunH :: Ptr CLong -> CInt
runH :: Either Int Int
runH = unsafePerformIO $
  alloca $ \tptr -> do
    let errcode = c_ENrunH tptr
    if 0 == errcode
      then do
        t <- peek tptr
        return $ Right (fromIntegral t)
      else do
         return $ Left (fromIntegral errcode)

foreign import ccall unsafe "toolkit.h ENnextH" c_ENnextH :: Ptr CLong -> CInt
nextH :: Either Int Int
nextH = unsafePerformIO $
  alloca $ \tptr -> do
    let errcode = c_ENnextH tptr
    if 0 == errcode
      then do
        t <- peek tptr
        return $ Right (fromIntegral t)
      else do
        return $ Left (fromIntegral errcode)

foreign import ccall unsafe "toolkit.h ENcloseH" c_ENcloseH :: CInt
closeH :: Int
closeH = unsafePerformIO $
  return $ fromIntegral $ c_ENcloseH

foreign import ccall unsafe "toolkit.h ENsavehydfile" c_ENsavehydfile :: CString -> CInt
saveHydFile :: String -> Int
saveHydFile f = unsafePerformIO $
  withCString f $ \cf ->
    return $ fromIntegral $ c_ENsavehydfile cf

foreign import ccall unsafe "toolkit.h ENusehydfile" c_ENusehydfile :: CString -> CInt
useHydFile :: String -> Int
useHydFile f = unsafePerformIO $
  withCString f $ \cf ->
    return $ fromIntegral $ c_ENusehydfile cf

foreign import ccall unsafe "toolkit.h ENsolveQ" c_ENsolveQ :: CInt
solveQ :: Int
solveQ = unsafePerformIO $
  return $ fromIntegral $ c_ENsolveQ

foreign import ccall unsafe "toolkit.h ENopenQ" c_ENopenQ :: CInt
openQ :: Int
openQ = unsafePerformIO $
  return $ fromIntegral $ c_ENopenQ

foreign import ccall unsafe "toolkit.h ENinitQ" c_ENinitQ :: CInt -> CInt
initQ :: Int -> Int
initQ flag = unsafePerformIO $
  return $ fromIntegral $ c_ENinitQ (fromIntegral flag)

foreign import ccall unsafe "toolkit.h ENrunQ" c_ENrunQ :: Ptr CLong -> CInt
runQ :: Either Int Int
runQ = unsafePerformIO $
  alloca $ \tptr -> do
    let errcode = c_ENrunQ tptr
    if 0 == errcode
      then do
        t <- peek tptr
        return $ Right (fromIntegral t)
      else do
         return $ Left (fromIntegral errcode)

foreign import ccall unsafe "toolkit.h ENnextQ" c_ENnextQ :: Ptr CLong -> CInt
nextQ :: Either Int Int
nextQ = unsafePerformIO $
  alloca $ \tptr -> do
    let errcode = c_ENnextQ tptr
    if 0 == errcode
      then do
        t <- peek tptr
        return $ Right (fromIntegral t)
      else do
        return $ Left (fromIntegral errcode)

foreign import ccall unsafe "toolkit.h ENstepQ" c_ENstepQ :: Ptr CLong -> CInt
stepQ :: Either Int Int
stepQ = unsafePerformIO $
  alloca $ \tptr -> do
    let errcode = c_ENstepQ tptr
    if 0 == errcode
      then do
        t <- peek tptr
        return $ Right (fromIntegral t)
      else do
        return $ Left (fromIntegral errcode)

foreign import ccall unsafe "toolkit.h ENcloseQ" c_ENcloseQ :: CInt
closeQ :: Int
closeQ = unsafePerformIO $
  return $ fromIntegral $ c_ENcloseQ

foreign import ccall unsafe "toolkit.h ENwriteline" c_ENwriteline :: CString -> CInt
writeLine :: String -> Int
writeLine l = unsafePerformIO $
  withCString l $ \cl ->
    return $ fromIntegral $ c_ENwriteline cl

foreign import ccall unsafe "toolkit.h ENreport" c_ENreport :: CInt
report :: Int
report = unsafePerformIO $
  return $ fromIntegral $ c_ENreport

foreign import ccall unsafe "toolkit.h ENresetreport" c_ENresetreport :: CInt
resetReport :: Int
resetReport = unsafePerformIO $
  return $ fromIntegral $ c_ENresetreport

foreign import ccall unsafe "toolkit.h ENsetreport" c_ENsetreport :: CString -> CInt
setReport :: String -> Int
setReport f = unsafePerformIO $
  withCString f $ \cf ->
    return $ fromIntegral $ c_ENsetreport cf 

foreign import ccall unsafe "toolkit.h ENgetcontrol" c_ENgetcontrol :: CInt -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> CInt
getControl ::  Int -> Either Int (Int, Int, Float, Int, Float)
getControl cindex = unsafePerformIO $
  alloca $ \ctypeptr -> do
    alloca $ \lindexptr -> do
      alloca $ \settingptr -> do
        alloca $ \nindexptr -> do
          alloca $ \levelptr -> do
            let errcode = c_ENgetcontrol (fromIntegral cindex) ctypeptr lindexptr settingptr nindexptr levelptr
            if 0 == errcode
              then do
                ctype <- peek ctypeptr
                lindex <- peek lindexptr
                setting <- peek settingptr
                nindex <- peek nindexptr
                level <- peek levelptr
                return $ Right (fromIntegral ctype, fromIntegral lindex, realToFrac setting, fromIntegral nindex, realToFrac level)
              else do
                return $ Left (fromIntegral errcode) 

foreign import ccall unsafe "toolkit.h ENgetcount" c_ENgetcount :: CInt -> Ptr CInt -> CInt
getCount :: Int -> Either Int Int
getCount code = unsafePerformIO $
  alloca $ \countptr -> do
    let errcode = c_ENgetcount (fromIntegral code) countptr
    if 0 == errcode
      then do
        count <- peek countptr
        return $ Right (fromIntegral count)
      else do
        return $ Left (fromIntegral errcode)
 
foreign import ccall unsafe "toolkit.h ENgetoption" c_ENgetoption :: CInt -> Ptr CFloat -> CInt
getOption :: Int -> Either Int Float
getOption code = unsafePerformIO $
  alloca $ \valueptr -> do
    let errcode = c_ENgetoption (fromIntegral code) valueptr
    if 0 == errcode
      then do
        value <- peek valueptr
        return $ Right (realToFrac value)
      else do
        return $ Left (fromIntegral errcode)

foreign import ccall unsafe "toolkit.h ENgettimeparam" c_ENgettimeparam :: CInt -> Ptr CLong -> CInt
getTimeParam :: Int -> Either Int Int64
getTimeParam code = unsafePerformIO $
  alloca $ \valueptr -> do
    let errcode = c_ENgettimeparam (fromIntegral code) valueptr
    if 0 == errcode
      then do
        value <- peek valueptr
        return $ Right (fromIntegral value)
      else do
        return $ Left (fromIntegral errcode)

foreign import ccall unsafe "toolkit.h ENgetflowunits" c_ENgetflowunits :: Ptr CInt -> CInt
getFlowUnits :: Either Int Int
getFlowUnits = unsafePerformIO $
  alloca $ \codeptr -> do
    let errcode = c_ENgetflowunits codeptr
    if 0 == errcode
      then do
        code <- peek codeptr
        return $ Right (fromIntegral code)
      else do
        return $ Left (fromIntegral errcode)

foreign import ccall unsafe "toolkit.h ENgetpatternindex" c_ENgetpatternindex :: CString -> Ptr CInt -> CInt
getPatternIndex :: String -> Either Int Int
getPatternIndex id = unsafePerformIO $
  withCString id $ \cid ->
    alloca $ \indexptr -> do
      let errcode = c_ENgetpatternindex cid indexptr
      if 0 == errcode
        then do
          index <- peek indexptr
          return $ Right (fromIntegral index)
       else do
          return $ Left (fromIntegral errcode)

foreign import ccall unsafe "toolkit.h ENgetpatternid" c_ENgetpatternid :: CInt -> CString -> CInt
getPatternId :: Int -> Either Int String
getPatternId index = unsafePerformIO $
  alloca $ \idptr -> do
    let errcode = c_ENgetpatternid (fromIntegral index) idptr
    if 0 == errcode
      then do
        id <- peekCString idptr
        return $ Right id
      else do
        return $ Left (fromIntegral errcode)

foreign import ccall unsafe "toolkit.h ENgetpatternlen" c_ENgetpatternlen :: CInt -> Ptr CInt -> CInt
getPatternLen :: Int -> Either Int Int
getPatternLen index = unsafePerformIO $
  alloca $ \lenptr -> do
    let errcode = c_ENgetpatternlen (fromIntegral index) lenptr
    if 0 == errcode
      then do
        len <- peek lenptr
        return $ Right (fromIntegral len)
      else do
        return $ Left (fromIntegral errcode)

foreign import ccall unsafe "toolkit.h ENgetpatternvalue" c_ENgetpatternvalue :: CInt -> CInt -> Ptr CFloat -> CInt
getPatternValue :: Int -> Int -> Either Int Float
getPatternValue index period = unsafePerformIO $
  alloca $ \valueptr -> do
    let errcode = c_ENgetpatternvalue (fromIntegral index) (fromIntegral period) valueptr
    if 0 == errcode
      then do
        value <- peek valueptr
        return $ Right (realToFrac value)
      else do
        return $ Left (fromIntegral errcode)

foreign import ccall unsafe "toolkit.h ENgetqualtype" c_ENgetqualtype :: Ptr CInt -> Ptr CInt -> CInt
getQualType :: Either Int (Int, Int)
getQualType = unsafePerformIO $
  alloca $ \qualcodeptr -> do
    alloca $ \tracenodeptr -> do
      let errcode = c_ENgetqualtype qualcodeptr tracenodeptr
      if 0 == errcode
        then do
          qualcode <- peek qualcodeptr
          tracenode <- peek tracenodeptr
          return $ Right (fromIntegral qualcode, fromIntegral tracenode)
        else do
          return $ Left (fromIntegral errcode)

foreign import ccall unsafe "toolkit.h ENgeterror" c_ENgeterror :: CInt -> CString -> CInt -> CInt
getError :: Int -> Int -> Either Int String
getError code n = unsafePerformIO $
  withCString (replicate n ' ') $ \cerrmsg -> do
    let errcode = c_ENgeterror (fromIntegral code) cerrmsg (fromIntegral n)
    if 0 == errcode
      then do
	errmsg <- peekCString cerrmsg
	return $ Right errmsg
      else do
        return $ Left (fromIntegral errcode)

foreign import ccall unsafe "toolkit.h ENgetnodeindex" c_ENgetnodeindex :: CString -> Ptr CInt -> CInt
getNodeIndex :: String -> Either Int Int
getNodeIndex id = unsafePerformIO $
  withCString id $ \cid ->
    alloca $ \indexptr -> do
      let errcode = c_ENgetnodeindex cid indexptr
      if 0 == errcode
        then do
          index <- peek indexptr
          return $ Right (fromIntegral index)
       else do
          return $ Left (fromIntegral errcode)

foreign import ccall unsafe "toolkit.h ENgetnodeid" c_ENgetnodeid :: CInt -> CString -> CInt
getNodeId :: Int -> Either Int String
getNodeId index = unsafePerformIO $
  alloca $ \idptr -> do
    let errcode = c_ENgetnodeid (fromIntegral index) idptr
    if 0 == errcode
      then do
        id <- peekCString idptr
        return $ Right id
      else do
        return $ Left (fromIntegral errcode)

foreign import ccall unsafe "toolkit.h ENgetnodetype" c_ENgetnodetype :: CInt -> Ptr CInt -> CInt
getNodeType :: Int -> Either Int Int
getNodeType index = unsafePerformIO $
  alloca $ \typeptr -> do
    let errcode = c_ENgetnodetype (fromIntegral index) typeptr
    if 0 == errcode
      then do
        t <- peek typeptr
        return $ Right (fromIntegral t)
      else do
        return $ Left (fromIntegral errcode)

foreign import ccall unsafe "toolkit.h ENgetnodevalue" c_ENgetnodevalue :: CInt -> CInt -> Ptr CFloat -> CInt
getNodeValue :: Int -> Int -> Either Int Float
getNodeValue index code = unsafePerformIO $
  alloca $ \valueptr -> do
    let errcode = c_ENgetnodevalue (fromIntegral index) (fromIntegral code) valueptr
    if 0 == errcode
      then do
        value <- peek valueptr
        return $ Right (realToFrac value)
      else do
        return $ Left (fromIntegral errcode)

foreign import ccall unsafe "toolkit.h ENgetlinkindex" c_ENgetlinkindex :: CString -> Ptr CInt -> CInt
getLinkIndex :: String -> Either Int Int
getLinkIndex id = unsafePerformIO $
  withCString id $ \cid ->
    alloca $ \indexptr -> do
      let errcode = c_ENgetlinkindex cid indexptr
      if 0 == errcode
        then do
          index <- peek indexptr
          return $ Right (fromIntegral index)
       else do
          return $ Left (fromIntegral errcode)

foreign import ccall unsafe "toolkit.h ENgetlinkid" c_ENgetlinkid :: CInt -> CString -> CInt
getLinkId :: Int -> Either Int String
getLinkId index = unsafePerformIO $
  alloca $ \idptr -> do
    let errcode = c_ENgetlinkid (fromIntegral index) idptr
    if 0 == errcode
      then do
        id <- peekCString idptr
        return $ Right id
      else do
        return $ Left (fromIntegral errcode)

foreign import ccall unsafe "toolkit.h ENgetlinktype" c_ENgetlinktype :: CInt -> Ptr CInt -> CInt
getLinkType :: Int -> Either Int Int
getLinkType index = unsafePerformIO $
  alloca $ \typeptr -> do
    let errcode = c_ENgetlinktype (fromIntegral index) typeptr
    if 0 == errcode
      then do
        t <- peek typeptr
        return $ Right (fromIntegral t)
      else do
        return $ Left (fromIntegral errcode)

foreign import ccall unsafe "toolkit.h ENgetlinknodes" c_ENgetlinknodes :: CInt -> Ptr CInt -> Ptr CInt -> CInt
getLinkNodes :: Int -> Either Int (Int, Int)
getLinkNodes index = unsafePerformIO $
  alloca $ \node1ptr -> do
    alloca $ \node2ptr -> do
      let errcode = c_ENgetlinknodes (fromIntegral index) node1ptr node2ptr
      if 0 == errcode
        then do
          node1 <- peek node1ptr
          node2 <- peek node2ptr
          return $ Right (fromIntegral node1, fromIntegral node2)
        else do
          return $ Left (fromIntegral errcode)

foreign import ccall unsafe "toolkit.h ENgetlinkvalue" c_ENgetlinkvalue :: CInt -> CInt -> Ptr CFloat -> CInt
getLinkValue :: Int -> Int -> Either Int Float
getLinkValue index code = unsafePerformIO $
  alloca $ \valueptr -> do
    let errcode = c_ENgetlinkvalue (fromIntegral index) (fromIntegral code) valueptr
    if 0 == errcode
      then do
        value <- peek valueptr
        return $ Right (realToFrac value)
      else do
        return $ Left (fromIntegral errcode)

foreign import ccall unsafe "toolkit.h ENgetversion" c_ENgetversion :: Ptr CInt -> CInt
getVersion :: Int
getVersion = unsafePerformIO $
  alloca $ \vptr -> do
    if 0 == (c_ENgetversion vptr)
      then do 
        v <- peek vptr
        return $ fromIntegral v
      else do
        return $ fromIntegral 0

foreign import ccall unsafe "toolkit.h ENsetcontrol" c_ENsetcontrol :: CInt -> CInt -> CInt -> CFloat -> CInt -> CFloat -> CInt
setControl :: Int -> Int -> Int -> Float -> Int -> Float -> Int
setControl cindex ctype lindex setting nindex level = unsafePerformIO $
  return $ fromIntegral $ c_ENsetcontrol (fromIntegral cindex) (fromIntegral ctype) (fromIntegral lindex) (realToFrac setting) (fromIntegral nindex) (realToFrac level)
 
foreign import ccall unsafe "toolkit.h ENsetnodevalue" c_ENsetnodevalue :: CInt -> CInt -> CFloat -> CInt
setNodeValue :: Int -> Int -> Float -> Int
setNodeValue index code v = unsafePerformIO $
  return $ fromIntegral $ c_ENsetnodevalue (fromIntegral index) (fromIntegral code) (realToFrac v)
  
foreign import ccall unsafe "toolkit.h ENsetlinkvalue" c_ENsetlinkvalue :: CInt -> CInt -> CFloat -> CInt
setLinkValue :: Int -> Int -> Float -> Int
setLinkValue index code v = unsafePerformIO $
  return $ fromIntegral $ c_ENsetlinkvalue (fromIntegral index) (fromIntegral code) (realToFrac v)

foreign import ccall unsafe "toolkit.h ENaddpattern" c_ENaddpattern :: CString -> CInt
addPattern :: String -> Int
addPattern p = unsafePerformIO $
  withCString p $ \cp -> do
    return $ fromIntegral $ c_ENaddpattern cp

-- Use setPatternValue instead
-- foreign import ccall unsafe "toolkit.h ENsetpattern" c_ENsetpattern :: CInt -> Ptr CFloat -> CInt -> CInt
-- setPattern :: Int -> DVS.Vector CFloat -> Int
-- setPattern index f = unsafePerformIO $
--   DVS.unsafeWith f $ \fptr (return (fromIntegral (c_ENsetpattern (fromIntegral index) fptr (fromIntegral (DVS.length f))))
  
foreign import ccall unsafe "toolkit.h ENsetpatternvalue" c_ENsetpatternvalue :: CInt -> CInt -> CFloat -> CInt
setPatternValue :: Int -> Int -> Float -> Int
setPatternValue index period value = unsafePerformIO $
  return $ fromIntegral $ c_ENsetpatternvalue (fromIntegral index) (fromIntegral period) (realToFrac value)

foreign import ccall unsafe "toolkit.h ENsettimeparam" c_ENsettimeparam :: CInt -> CLong -> CInt
setTimeParam :: Int -> Int -> Int
setTimeParam code value = unsafePerformIO $
  return $ fromIntegral $ c_ENsettimeparam (fromIntegral code) (fromIntegral value)

foreign import ccall unsafe "toolkit.h ENsetoption" c_ENsetoption :: CInt -> CFloat -> CInt
setOption :: Int -> Float -> Int
setOption code v = unsafePerformIO $
  return $ fromIntegral $ c_ENsetoption (fromIntegral code) (realToFrac v)

foreign import ccall unsafe "toolkit.h ENsetstatusreport" c_ENsetstatusreport :: CInt -> CInt
setStatusReport :: Int -> Int
setStatusReport code = unsafePerformIO $
  return $ fromIntegral $ c_ENsetstatusreport (fromIntegral code)

foreign import ccall unsafe "toolkit.h ENsetqualtype" c_ENsetqualtype :: CInt -> CString -> CString -> CString -> CInt
setQualType :: Int -> String -> String -> String -> Int
setQualType qualcode chemname chemunits tracenode = unsafePerformIO $
  withCString chemname $ \cchemname -> do
    withCString chemunits $ \cchemunits -> do
      withCString tracenode $ \ctracenode -> do
        return $ fromIntegral $ c_ENsetqualtype (fromIntegral qualcode) cchemname cchemunits ctracenode

