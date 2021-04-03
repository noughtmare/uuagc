module UU.UUAGC (uuagc, uuagcTH, uuagcMain, compile, module Options) where

import Ag (uuagcLib, uuagcLibTH, uuagcExe, compile)
import Options
import Language.Haskell.TH (Q, Dec)

import System.Exit (ExitCode(..))

uuagc :: [String] -> FilePath -> IO (ExitCode, [FilePath])
uuagc = uuagcLib

uuagcTH :: [String] -> String -> Q [Dec]
uuagcTH = uuagcLibTH

uuagcMain :: IO ()
uuagcMain = uuagcExe
