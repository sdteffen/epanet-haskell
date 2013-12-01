{-# LANGUAGE ForeignFunctionInterface #-}
module Main where

import Foreign
import Foreign.C
import Foreign.Ptr (Ptr, nullPtr)

foreign import ccall unsafe "toolkit.h ENgetversion" c_ENgetversion :: Ptr CInt -> CInt

main = do
  alloca $ \vptr -> do
    print $ c_ENgetversion vptr
    v <- peek vptr
    print $ fromIntegral v

