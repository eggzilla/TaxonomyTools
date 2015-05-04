{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveDataTypeable #-}
-- | Ids2TreeCompare
-- dist/build/TaxonomyTools/Ids2TreeCompare -i /scratch/egg/data/taxdump/ -t /home/mescalin/egg/current/Projects/Haskell/TaxonomyTools/input.csv -o /home/mescalin/egg/current/Projects/Haskell/TaxonomyTools/
-- dot -Tsvg comparison.dot -o comparison.svg
module Main where

import Prelude 
import System.Console.CmdArgs    
import Bio.Taxonomy
import Data.Either.Unwrap
import Data.Graph.Inductive
import Data.Csv
import Data.List
import qualified Data.Vector as V
import qualified Data.ByteString.Lazy.Char8 as L
import Data.Char
--------------------------------------------------------

data Options = Options            
  { taxDumpDirectoryPath :: String,
    taxNodeCSVFilePath :: String,
    outputDirectoryPath :: String
  } deriving (Show,Data,Typeable)

options :: Options
options = Options
  { taxDumpDirectoryPath = def &= name "i" &= help "Path to input NCBI taxonomy dump files directory",
    taxNodeCSVFilePath = def &= name "t" &= help "Path to input taxonomy csv, each column with comma separated taxids represents one tree",
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
       let graph = fromRight graphOutput
       treesTaxids <- extractTreesTaxidsCSV taxNodeCSVFilePath
       let subgraphs  = map (\treeTaxids -> extractTaxonomySubTreebyLevel treeTaxids graph (Just 3)) treesTaxids
       let comparisonGraph = compareSubTrees subgraphs
       print comparisonGraph
       let treeComparison = drawTreeComparison comparisonGraph
       print treeComparison
      
-- | Extract taxids from RNAlien result.csv 
extractTreesTaxidsCSV :: String -> IO [[Node]]
extractTreesTaxidsCSV treesCSVPath = do
  let myOptions = defaultDecodeOptions {
         decDelimiter = fromIntegral (ord ',')
         }
  inputCSV <- L.readFile treesCSVPath
  let decodedCsvOutput = V.toList (fromRight (decodeWith myOptions HasHeader inputCSV :: Either String (V.Vector ([Int]))))
  let treesTaxids = transpose decodedCsvOutput
  return treesTaxids 

