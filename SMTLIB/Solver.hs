module SMTLIB.Solver 
    (
        isSat
    )
where

import SMTLIB
import System.IO
import System.Process
import Data.String.Utils
import Control.Monad()

isSat :: Script -> IO Bool
isSat script = do
    let tmpFile = "./tmp.smt2"
    writeFile tmpFile (show script)
    (_, Just hout, _, _) <-
      createProcess (proc "z3" ["-smt2", tmpFile]){ std_out = CreatePipe }
    sOut <- hGetContents hout
    s <- return (strip sOut)
    return ("sat" == s)
    