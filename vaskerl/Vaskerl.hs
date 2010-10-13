module Test where

import System.IO
--import Text.JSON
--import Text.JSON.Generic
import Network (withSocketsDo)
import Network.BSD

import qualified Vaskerl as Vaskerl
import Foreign.C.String (newCString)

data Map = Map

process input = reverse input

loop = do
     getContents >>= putStrLn
     hFlush stdout
     loop

main = withSocketsDo $ do
     getHostByName "localhost" >>= \l -> putStrLn (hostName l)

     Vaskerl.initConnect "vaskerl" "erl-dev" "hoovy_studio_humasect_mchoovy"
     --hSetBuffering stdout NoBuffering >> 
     --loop
