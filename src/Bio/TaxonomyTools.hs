{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveDataTypeable #-}
-- | TaxonomyTools

module Main where

import Prelude 
import System.Console.CmdArgs    
import Bio.Taxonomy

--------------------------------------------------------

data Options = Options            
  { inputFilePath :: String,
    outputPath :: String
  } deriving (Show,Data,Typeable)

options = Options
  { inputFilePath = def &= name "i" &= help "Path to input fasta file",
    outputPath = def &= name "o" &= help "Path to output directory"
  } &= summary "TaxonomyTools" &= help "Florian Eggenhofer - 2015" &= verbosity   

main :: IO ()
main = do
  Options{..} <- cmdArgs options
  print inputFilePath 
