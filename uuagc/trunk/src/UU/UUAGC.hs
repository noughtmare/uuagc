module UU.UUAGC (uuagc, uuagcTH, uuagcMain, compile, module Options) where

import Ag (uuagcLib, uuagcLibString, uuagcExe, compile)
import Options
import Language.Haskell.TH (Dec)

import System.Exit (ExitCode(..))

uuagc :: [String] -> FilePath -> IO (ExitCode, [FilePath])
uuagc = uuagcLib

uuagcTH :: [String] -> String -> Either String [Dec]
uuagcTH = uuagcLibString

uuagcMain :: IO ()
uuagcMain = uuagcExe
