{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveDataTypeable #-}
-- | TaxIds2TreeCompare  
-- dist/build/TaxIds2TreeCompare/TaxIds2TreeCompare -i /scratch/egg/taxdmpnew/ -l 4 -t /home/mescalin/egg/current/Projects/Haskell/TaxonomyTools/input.csv -o /home/mescalin/egg/current/Projects/Haskell/TaxonomyTools/
-- dot -Tsvg comparison.dot -o comparison.svg
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
    taxNodeCSVFilePath :: String,
    levels :: Int,                      
    outputDirectoryPath :: String
  } deriving (Show,Data,Typeable)

options :: Options
options = Options
  { taxDumpDirectoryPath = def &= name "i" &= help "Path to input NCBI taxonomy dump files directory",
    taxNodeCSVFilePath = def &= name "t" &= help "Path to input taxonomy csv, each column with comma separated taxids represents one tree",
    levels = (1 ::Int) &= name "l" &= help "Number defining maximum distance from root for nodes in subtree.",
    outputDirectoryPath = def &= name "o" &= help "Path to output directory"
  } &= summary "TaxIds2TreeCompare - Multiple lists of taxonomy ids are converted into a visualisation of the taxonomic tree highlighting the input nodes corresponding to their list." &= help "Florian Eggenhofer - 2015" &= verbosity   

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
       let subgraphs  = map (\treeTaxids -> extractTaxonomySubTreebyLevel treeTaxids graph (Just levels)) treesTaxids
       let comparisonGraph = compareSubTrees subgraphs
       let treeComparison = drawTreeComparison comparisonGraph
       writeFile (outputDirectoryPath ++ "comparison.dot") treeComparison
      
-- | Extract taxids from RNAlien result.csv 
extractTreesTaxidsCSV :: String -> IO [[Node]]
extractTreesTaxidsCSV treesCSVPath = do
  let myOptions = defaultDecodeOptions {
         decDelimiter = fromIntegral (ord ',')
         }
  inputCSV <- L.readFile treesCSVPath
  let decodedCsvOutput = V.toList (fromRight (decodeWith myOptions NoHeader inputCSV :: Either String (V.Vector ([Int]))))
  --print (fromLeft (decodeWith myOptions NoHeader inputCSV :: Either String (V.Vector ([Int]))))
  let treesTaxids = decodedCsvOutput
  return treesTaxids 
  --return [[]]

