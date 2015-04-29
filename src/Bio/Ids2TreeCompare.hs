{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveDataTypeable #-}
-- | Ids2TreeCompare
-- dist/build/TaxonomyTools/TaxonomyTools -i /scratch/egg/data/taxdump/ -o /home/mescalin/egg/current/Projects/Haskell/TaxonomyTools/
-- dot -Tsvg taxonomy.dot -o taxonomy.svg
module Main where

import Prelude 
import System.Console.CmdArgs    
import Bio.Taxonomy
import Data.Either.Unwrap
import Data.Graph.Inductive
import Data.Csv
import qualified Data.Vector as V
import qualified Data.ByteString.Lazy.Char8 as L
import Data.Char
--------------------------------------------------------

data Options = Options            
  { taxDumpDirectoryPath :: String,
    taxNodeList1FilePath :: String,
    taxNodeList2FilePath :: String,  
    outputDirectoryPath :: String
  } deriving (Show,Data,Typeable)

options :: Options
options = Options
  { taxDumpDirectoryPath = def &= name "i" &= help "Path to input NCBI taxonomy dump files directory",
    taxNodeList1FilePath = def &= name "t" &= help "Path to input taxonomy id list 1",
    taxNodeList2FilePath = def &= name "t" &= help "Path to input taxonomy id list 2",
    outputDirectoryPath = def &= name "o" &= help "Path to output directory"
  } &= summary "Ids2TreeCompare" &= help "Florian Eggenhofer - 2015" &= verbosity   

main :: IO ()
main = do
  Options{..} <- cmdArgs options
  graphOutput <- readNamedTaxonomy taxDumpDirectoryPath
  if (isLeft graphOutput)
     then do
       print ("Could not parse provided taxonomy dump files" ++ show (fromLeft graphOutput))
     else do 
       taxidtable1 <- readFile taxNodeListFilePath1
       taxidtable2 <- readFile taxNodeListFilePath2
       let taxidtableentries1 = map (\l -> read l :: Int) (drop 1 (lines taxidtable1))
       let taxidtableentries2 = map (\l -> read l :: Int) (drop 1 (lines taxidtable2))
       print taxidtableentries1
       print taxidtableentries2
