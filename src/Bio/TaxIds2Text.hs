{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE Arrows #-}

-- /dist/build/TaxIds2Text/TaxIds2Text -t /home/mescalin/egg/current/Projects/Haskell/TaxonomyTools/RF00169_accessionnumbers.tax -i /scratch/egg/data/taxdmpnew/

module Main where
    
import System.Console.CmdArgs    
import Data.Either.Unwrap
import qualified Data.ByteString.Char8 as BC
import Bio.Taxonomy
 
data Options = Options            
  { taxDumpDirectoryPath :: String,
    taxNodeListFilePath :: String
  } deriving (Show,Data,Typeable)
             
options :: Options
options = Options
  { taxDumpDirectoryPath = def &= name "i" &= help "Path to input NCBI taxonomy dump files directory",
    taxNodeListFilePath = def &= name "t" &= help "Path to input taxonomy id list without header"
  } &= summary "TaxIds2Text" &= help "Florian Eggenhofer - 2015" &= verbosity   
        
main :: IO ()
main = do
  Options{..} <- cmdArgs options
  graphOutput <- readNamedTaxonomy taxDumpDirectoryPath
  if isLeft graphOutput then 
    print ("Could not parse provided taxonomy dump files" ++ show (fromLeft graphOutput))
    else   
    do if null taxNodeListFilePath then
              putStrLn "Provide a path to input taxonomy id list or to RNAlienResult CSV."
         else 
         do -- input taxid path present
            taxidtable <- readFile taxNodeListFilePath
            let taxidtableentries = map (\l -> read l :: Int) (lines taxidtable)
            let graph = fromRight graphOutput
            let maybeParentNodes = map (\taxidtableentry -> getParentbyRank taxidtableentry graph (Just Class)) taxidtableentries
            let parentNodeStrings = map (\maybeParentNode -> maybe "not found,not found" (\(_,n) ->  printSimpleNode n) maybeParentNode) maybeParentNodes
            let outputCSV = map (\(txid,parentNodeString) -> (show txid) ++ "," ++ parentNodeString)   (zip taxidtableentries parentNodeStrings)
            mapM_ putStrLn outputCSV

printSimpleNode :: SimpleTaxon -> String 
printSimpleNode snode = show (simpleRank snode) ++ "," ++ BC.unpack (simpleScientificName snode)
