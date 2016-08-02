module Language.Haskell.DataFilesGenSpec where

import Language.Haskell.DataFilesGen
import           Test.Hspec
import Data.Default
import qualified Data.Set as S

spec :: Spec
spec = do
  describe "data dir" $ do
    it "can list everything" $ do
       ls <- generateList def "data"
       ls `shouldBe` ["data/noext","data/*.md","data/*.txt","data/sub1/*.ext.txt","data/sub1/*.md","data/sub2/*.txt"]
    it "filters on extensions" $ do
       ls <- generateList def{optExcludeExts=S.fromList [".md"]} "data"
       ls `shouldBe` ["data/noext","data/*.txt","data/sub1/*.ext.txt","data/sub2/*.txt"]
       ls2 <- generateList def{optExcludeExts=S.fromList [".md",".txt"]} "data"
       ls2 `shouldBe` ["data/noext","data/sub1/*.ext.txt"]
       ls3 <- generateList def{optExcludeExts=S.fromList [".md",".txt",".ext.txt"]} "data"
       ls3 `shouldBe` ["data/noext"]
    it "filters on directory" $ do
       ls <- generateList def{optExcludeDirs=S.fromList ["sub1"]} "data"
       ls `shouldBe` ["data/noext","data/*.md","data/*.txt","data/sub2/*.txt"]
       ls2 <- generateList def{optExcludeDirs=S.fromList ["sub1","sub2"]} "data"
       ls2 `shouldBe` ["data/noext","data/*.md","data/*.txt"]
