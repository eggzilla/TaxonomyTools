{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveDataTypeable #-}
-- | TaxonomyTools
-- dist/build/TaxIdsDistance/TaxIdsDistance -i /scratch/egg/data/taxdump/ -a 9986 -b 4039
module Main where

import Prelude 
import System.Console.CmdArgs    
import Bio.Taxonomy
import Data.Either.Unwrap
import Data.Graph.Inductive
--------------------------------------------------------

data Options = Options            
  { taxDumpDirectoryPath :: String,
    organism1 :: Int,
    organism2 :: Int
  } deriving (Show,Data,Typeable)

options :: Options
options = Options
  { taxDumpDirectoryPath = def &= name "i" &= help "Path to input NCBI taxonomy dump files directory",
    organism1 = def &= name "a" &= help "NCBI Taxonomy Id of the first organism",
    organism2 = def &= name "b" &= help "NCBI Taxonomy Id of the second organism"
  } &= summary "TaxIdsDistance - Computes the distance between two input nodes on the given tree." &= help "Florian Eggenhofer - 2015" &= verbosity   

main :: IO ()
main = do
  Options{..} <- cmdArgs options
  graphOutput <- readNamedTaxonomy taxDumpDirectoryPath
  if isLeft graphOutput then 
    print ("Could not parse provided taxonomy dump files" ++ show (fromLeft graphOutput))
    else   
    do let distance = sp (organism1 :: Node) (organism2 :: Node) (undir (fromRight graphOutput))
       print (length distance)
