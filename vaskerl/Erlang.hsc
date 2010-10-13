{-# OPTIONS -cpp #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Erlang where

import Foreign.Ptr
import Foreign.Marshal
import Foreign.Storable
import Foreign.C

#include "erl_interface.h"

#ifndef CALLCONV
#define CALLCONV ccall
#endif

data ErlTypes =
       ErlUndef | ErlInteger | ErlUInteger | ErlAtom | ErlPid |
     | ErlPort | ErlRef | ErlCons | ErlList | ErlNil | ErlEmptyList
     | ErlTuple | ErlBinary | ErlFloat | ErlVariable | ErlSmallBig
     | ErlUSmallBig | ErlFunction | ErlBig | ErlLongLong | ErlULongLong

-- type ErlHeader = #{type Erl_Header}

data ErlMessage = ErlMessage
     { t :: Int,
       msg, from, to :: Ptr ErlTerm,
       to_name :: [Char] }

instance Storable ErlMessage where
  sizeOf _ = 4*sizeOf (Ptr a) + 
  alignment _ = alignment (undefined :: Int)

-- does not work.
-- #define UNSAFE_CALL foreign import CALLCONV unsafe

foreign import CALLCONV unsafe "vaskerl_init_connect" vaskerlInitConnect ::
        CString -> CString -> CString -> CString -> CString -> IO Int

initConnect :: String -> String -> String -> IO Int
initConnect name remoteName cookie = do
            name' <- newCString name
            cookie' <- newCString cookie
            remoteName' <- newCString remoteName

            host' <- newCString "McHoovy.rd.shawcable.net"
            ip' <- newCString "127.0.0.1"

            vaskerlInitConnect name' ip' host' remoteName' cookie'

----------------------------------------------

#ifdef AOEAOEAOEAOEAEO
UNSAFE_CALL "erl_init" init :: Ptr a -> CLong -> IO ()

UNSAFE_CALL "erl_connect_init" sconnectInit ::
             CInt -> CString -> CShort -> IO ()
--FCALL "erl_connect_xinit" connectXInit :: CString -> CString -> 

UNSAFE_CALL "erl_connect" sconnect :: CString -> IO ()
UNSAFE_CALL "erl_xconnect" connect :: Ptr a -> CString -> IO ()

UNSAFE_CALL "erl_publish" publish :: CInt -> IO ()

glGen f = alloca (\ptr -> f 1 ptr >> peek ptr)
#endif

