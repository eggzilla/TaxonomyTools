{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveDataTypeable #-}
-- | TaxonomyTools
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
    taxNodeListFilePath :: String,                        
    outputDirectoryPath :: String
  } deriving (Show,Data,Typeable)

options :: Options
options = Options
  { taxDumpDirectoryPath = def &= name "i" &= help "Path to input NCBI taxonomy dump files directory",
    taxNodeListFilePath = def &= name "t" &= help "Path to input taxonomy id list",
    outputDirectoryPath = def &= name "o" &= help "Path to output directory"
  } &= summary "TaxonomyTools" &= help "Florian Eggenhofer - 2015" &= verbosity   

main :: IO ()
main = do
  Options{..} <- cmdArgs options
  graphOutput <- readNamedTaxonomy taxDumpDirectoryPath
  if isLeft graphOutput then 
    print ("Could not parse provided taxonomy dump files" ++ show (fromLeft graphOutput))
    else 
    do taxidtable <- readFile taxNodeListFilePath
       let taxidtableentries = map (\l -> read l :: Int) (drop 1 (lines taxidtable))
       let graph = fromRight graphOutput
       let subgraph  = extractTaxonomySubTreebyLevel taxidtableentries graph (Just 3)                
       let subdiagram = drawTaxonomy (grev subgraph)
       writeFile (outputDirectoryPath ++ "taxonomy.dot") subdiagram

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
