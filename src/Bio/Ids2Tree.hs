{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveDataTypeable #-}
-- | TaxonomyTools
-- dist/build/Ids2Tree/Ids2Tree -i /scratch/egg/data/taxdump/ -o /home/mescalin/egg/current/Projects/Haskell/TaxonomyTools/ -t /home/mescalin/egg/current/Projects/Haskell/TaxonomyTools/RF00169_accessionnumbers.tax -f json -l 4
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
import qualified Data.Aeson.Encode as E
--------------------------------------------------------

data Options = Options            
  { taxDumpDirectoryPath :: String,
    taxNodeListFilePath :: String,
    alienCSVFilePath ::String,
    levels :: Int,
    outputFormat :: String,
    outputDirectoryPath :: String
  } deriving (Show,Data,Typeable)

options :: Options
options = Options
  { taxDumpDirectoryPath = def &= name "i" &= help "Path to input NCBI taxonomy dump files directory",
    taxNodeListFilePath = def &= name "t" &= help "Path to input taxonomy id list without header",
    alienCSVFilePath = def &= name "r" &= help "Path to RNAlienResult CSV. Alternative to input taxonomy id list",
    levels = (1 ::Int) &= name "l" &= help "Number defining maximum distance from root for nodes in subtree.",
    outputFormat = "dot" &= name "f" &= help "Requested output format (json,dot). Default: dot",
    outputDirectoryPath = def &= name "o" &= help "Path to output directory"
  } &= summary "TaxonomyTools" &= help "Florian Eggenhofer - 2015" &= verbosity   

main :: IO ()
main = do
  Options{..} <- cmdArgs options
  graphOutput <- readNamedTaxonomy taxDumpDirectoryPath
  if isLeft graphOutput then 
    print ("Could not parse provided taxonomy dump files" ++ show (fromLeft graphOutput))
    else   
    do if null taxNodeListFilePath then
         do if null alienCSVFilePath then
              putStrLn "Provide a path to input taxonomy id list or to RNAlienResult CSV."
              else 
              do -- input AlienCSV path present
                taxidtableentries <- extractTaxidsAlienCSV alienCSVFilePath
                let graph = fromRight graphOutput
                let subgraph  = extractTaxonomySubTreebyLevel taxidtableentries graph (Just levels)                
                --let subdiagram = drawTaxonomy (grev subgraph)
                --writeFile (outputDirectoryPath ++ "taxonomy.dot") subdiagram
                generateOutput outputFormat outputDirectoryPath subgraph
         else 
         do -- input taxid path present
            taxidtable <- readFile taxNodeListFilePath
            let taxidtableentries = map (\l -> read l :: Int) (lines taxidtable)
            let graph = fromRight graphOutput
            let subgraph  = extractTaxonomySubTreebyLevel taxidtableentries graph (Just levels)                
            --let subdiagram = drawTaxonomy (grev subgraph)
            --writeFile (outputDirectoryPath ++ "taxonomy.dot") subdiagram
            generateOutput outputFormat outputDirectoryPath subgraph

-- | generate output
generateOutput :: String -> String -> Gr SimpleTaxon Double -> IO ()
generateOutput requestedFormat outputDirectoryPath inputGraph = do
  case requestedFormat of
    "dot" -> generateDotOutput outputDirectoryPath inputGraph
    "json" -> generateJsonOutput outputDirectoryPath inputGraph
    _ -> generateDotOutput outputDirectoryPath inputGraph


generateDotOutput :: String -> Gr SimpleTaxon Double -> IO ()
generateDotOutput outputDirectoryPath inputGraph = do
  let diagram = drawTaxonomy (grev inputGraph)
  writeFile (outputDirectoryPath ++ "taxonomy.dot") diagram
  

generateJsonOutput :: String -> Gr SimpleTaxon Double -> IO ()
generateJsonOutput outputDirectoryPath inputGraph = do
  let jsonOutput = E.encode (grev inputGraph)
  L.writeFile (outputDirectoryPath ++ "taxonomy.json") jsonOutput


-- | Extract taxids from RNAlien result.csv 
extractTaxidsAlienCSV :: String -> IO [Node]
extractTaxidsAlienCSV alienCSVPath = do
  let myOptions = defaultDecodeOptions {
         decDelimiter = fromIntegral (ord ';')
         }
  inputCSV <- L.readFile alienCSVPath
  let decodedCsvOutput = V.toList (fromRight (decodeWith myOptions HasHeader inputCSV :: Either String (V.Vector (String,String,String))))
  let taxnodes = map (\(a,_,_) -> read a :: Node) decodedCsvOutput
  return taxnodes 
