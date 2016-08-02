-- | application code
module Main where

import System.Console.GetOpt
import Language.Haskell.DataFilesGen
import System.Environment
import qualified Data.Set as S
import Data.Default

-- | Application entry point
main :: IO ()
main = do
  args <- getArgs
  (opts,fps) <- parseOpts args
  res <- mapM (generateList opts) fps
  let ind = replicate (optIndent opts) ' '
  mapM_ (\s->putStrLn (ind ++ s)) $ concat res

-- | Parse options
parseOpts :: [String] -> IO (Options,[String])
parseOpts argv =
   case getOpt Permute options argv of
      (o,n,[]  ) -> return (foldl (flip id) def o, n)
      (_,_,errs) -> ioError (userError (concat errs ++ usageInfo header options))
  where header = "Usage: data-files-gen [OPTION...] dir"

-- | Option description
options :: [OptDescr (Options -> Options)]
options =  [
    Option ['d']     ["dir"]
    (ReqArg (\f opts -> opts { optExcludeDirs = S.insert f $ optExcludeDirs opts })
              "DIR")
      "directory name to exclude"
    ,Option ['e']     ["ext"]
    (ReqArg (\f opts -> opts { optExcludeExts = S.insert ("."++f) $ optExcludeExts opts })
              "EXT")
      "extension to exclude"
    ,Option ['i']     ["indent"]
    (ReqArg (\i opts -> opts { optIndent = read i })
              "INT")
      "number of spaces to use for indentation"
  ]
