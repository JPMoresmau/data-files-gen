-- | Library module
module Language.Haskell.DataFilesGen where

import Control.Monad
import System.Directory
import System.FilePath
import qualified Data.Set as S
import Data.List (isPrefixOf, sort)
import Data.Default

-- | Options for processing
data Options = Options
 { optExcludeDirs :: S.Set String -- ^ Directory names to exclude
 , optExcludeExts :: S.Set String -- ^ Extensions to exclude
 } deriving (Read,Show,Eq,Ord)

-- | Default instance, excluding nothing
instance Default Options where
  def = Options S.empty S.empty

-- | Generate the list of matching expressions
generateList :: Options -> FilePath -> IO [String]
generateList opts root = do
  fs <- getDirectoryContents root
  let vis = filter isVisible fs
  (exts,foldrs) <- foldM collect (S.empty,S.empty) vis
  subs <- mapM (generateList opts) $ S.toList foldrs
  let fullExts = map (\x -> root </> ("*"++x)) $ S.toList exts
  return $ (sort fullExts) ++ ( sort $ concat subs)
  where
    isVisible :: FilePath -> Bool
    isVisible fn
      | fn == "." = False
      | fn == ".." = False
      | "." `isPrefixOf` fn = False
      | otherwise = True
    collect :: (S.Set String,S.Set FilePath) -> FilePath -> IO (S.Set String,S.Set FilePath)
    collect (exts,fldrs) fp = do
      let full = root </> fp
      fex <- doesFileExist full
      return $ if fex
                then let ext = takeExtension fp
                     in if ext `S.member` optExcludeExts opts
                          then (exts,fldrs)
                          else (S.insert ext exts,fldrs)
                else if fp `S.member` optExcludeDirs opts
                        then (exts,fldrs)
                        else (exts,S.insert full fldrs)
