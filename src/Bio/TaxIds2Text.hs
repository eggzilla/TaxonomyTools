{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE Arrows #-}

-- ./dist/build/TaxIds2Text/TaxIds2Text -t /home/mescalin/egg/current/Projects/Haskell/TaxonomyTools/RF00169_accessionnumbers.tax -r Phylum -i /scratch/egg/taxdmpnew/ > out.csv
module Main where
    
import System.Console.CmdArgs    
import Data.Either.Unwrap
import qualified Data.ByteString.Char8 as BC
import Bio.Taxonomy
import Data.Maybe
import Text.Read

data Options = Options            
  { taxDumpDirectoryPath :: String,
    taxonomicRank :: String,
    taxNodeListFilePath :: String
  } deriving (Show,Data,Typeable)
             
options :: Options
options = Options
  { taxDumpDirectoryPath = def &= name "i" &= help "Path to input NCBI taxonomy dump files directory",
    taxonomicRank = "Class" &= name "r" &= help "Requested taxonomic rank - default Class",
    taxNodeListFilePath = def &= name "t" &= help "Path to input taxonomy id list without header"
  } &= summary "TaxIds2Text - List of taxonomy ids is converted in a short text summary for each node." &= help "Florian Eggenhofer - 2015" &= verbosity   
        
main :: IO ()
main = do
  Options{..} <- cmdArgs options
  graphOutput <- readNamedTaxonomy taxDumpDirectoryPath
  let currentRank = readMaybeRank taxonomicRank 
  if isNothing currentRank
    then putStrLn "Please provide a valid taxonomic Rank (e.g. Class)."
    else do
      if isLeft graphOutput 
       then print ("Could not parse provided taxonomy dump files" ++ show (fromLeft graphOutput))
       else do 
         if null taxNodeListFilePath 
           then do 
             putStrLn "Provide a path to input taxonomy id list or to RNAlienResult CSV."
           else  do -- input taxid path present
             taxidtable <- readFile taxNodeListFilePath
             let taxidtableentries = map (\l -> read l :: Int) (lines taxidtable)
             let graph = fromRight graphOutput
             let maybeParentNodes = map (\taxidtableentry -> getParentbyRank taxidtableentry graph (Just Phylum)) taxidtableentries
             let parentNodeStrings = map (\maybeParentNode -> maybe "not found,not found" (\(_,n) ->  printSimpleNode n) maybeParentNode) maybeParentNodes
             let outputCSV = map (\(txid,parentNodeString) -> (show txid) ++ "," ++ parentNodeString)   (zip taxidtableentries parentNodeStrings)
             mapM_ putStrLn outputCSV

printSimpleNode :: SimpleTaxon -> String 
printSimpleNode snode = show (simpleRank snode) ++ "," ++ BC.unpack (simpleScientificName snode)

readMaybeRank :: String -> Maybe Rank
readMaybeRank inputString = readMaybe inputString
