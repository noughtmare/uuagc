module UU.UUAGC (uuagc, uuagcTH, uuagcMain, compile, module Options) where

import Ag (uuagcLib, uuagcLibTH, uuagcExe, compile)
import Options
import Language.Haskell.TH.Syntax (Q, Exp, addTopDecls, lift, newName, Dec (ValD), Body (NormalB), Pat (VarP))
import Data.Maybe (fromMaybe)
import ConcreteSyntax (AG (AG))

import System.Exit (ExitCode(..))

uuagc :: [String] -> FilePath -> IO (ExitCode, [FilePath])
uuagc = uuagcLib

uuagcTH :: String -> [String] -> [AG] -> String -> Q [Dec]
uuagcTH n opts old str = do
  name <- newName n
  (new, decs) <- uuagcLibTH opts str (AG (foldr (\(AG x) xs -> x ++ xs) [] old))
  new' <- lift new
  pure (ValD (VarP name) (NormalB new') [] : decs)

uuagcMain :: IO ()
uuagcMain = uuagcExe
