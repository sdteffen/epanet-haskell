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

foreign import ccall unsafe "toolkit.h ENopen" c_ENopen :: CString -> CString -> CString -> CInt
open :: String -> String -> String -> Int
open f1 f2 f3 = unsafePerformIO $
  withCString f1 $ \cf1 ->
    withCString f2 $ \cf2 ->
      withCString f3 $ \cf3 ->
        return $ fromIntegral $ c_ENopen cf1 cf2 cf3

foreign import ccall unsafe "toolkit.h ENgetversion" c_ENgetversion :: Ptr CInt -> CInt
getVersion = unsafePerformIO $
  alloca $ \vptr -> do
    if 0 == (c_ENgetversion vptr)
      then do 
        v <- peek vptr
        return $ fromIntegral v
      else do
        return 0
