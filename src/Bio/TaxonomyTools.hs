{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveDataTypeable #-}
-- | TaxonomyTools

module Main where

import Prelude 
import System.Console.CmdArgs    
import Bio.Taxonomy
import Data.Either.Unwrap
import Data.Graph.Inductive

--------------------------------------------------------

data Options = Options            
  { taxNodesFilePath :: String,
    outputPath :: String
  } deriving (Show,Data,Typeable)

options :: Options
options = Options
  { taxNodesFilePath = def &= name "i" &= help "Path to input NCBI taxonomy dump nodes file",
    outputPath = def &= name "o" &= help "Path to output directory"
  } &= summary "TaxonomyTools" &= help "Florian Eggenhofer - 2015" &= verbosity   

main :: IO ()
main = do
  Options{..} <- cmdArgs options
  graphOutput <- readTaxonomy taxNodesFilePath
  if (isLeft graphOutput)
     then do
       print ("Could not parse provided tax nodes file" ++ (fromLeft graphOutput))
     else do 
       let graph = fromRight graphOutput
       let rootNode = bfs (1 :: Node) graph
       print rootNode
       let colirootpath = sp (561 :: Node) (1 :: Node) graph
       print colirootpath
       let colilabel = lab graph (561 :: Node)
       print colilabel
       print taxNodesFilePath
