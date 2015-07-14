{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE Arrows #-}

--dist/build/Accessions2TaxIds/Accessions2TaxIds -i /scratch/egg/AccessionList > ~egg/TaxidOut

module Main where
    
import System.Console.CmdArgs    
import Text.ParserCombinators.Parsec
import Data.List
import Data.Either.Unwrap
import Control.Concurrent
import Bio.EntrezHTTP
import Text.XML.HXT.Core  
 
data Options = Options            
  { inputFilePath :: String
  } deriving (Show,Data,Typeable)

options :: Options            
options = Options
  { inputFilePath = def &= name "i" &= help "Path to input accession file containing taxids seperated by linebreaks."
  } &= summary "Accessions2TaxIds" &= help "Florian Eggenhofer - 2015" &= verbosity   
  
main :: IO [()]    
main = do
  Options{..} <- cmdArgs options       
  accessionsFile <- readFile inputFilePath
  let accessions = lines accessionsFile
  gis <- retrieveGIsEntrez accessions
  taxIds <- retrieveTaxIdsEntrez gis
  mapM putStrLn (map show taxIds)


retrieveTaxIdsEntrez :: [Int] -> IO [Int]
retrieveTaxIdsEntrez geneIds = do
  taxIdsOutput <- retrieveElementsEntrez geneIds retrieveTaxIdEntrez
  let taxids = concatMap readEntrezTaxId taxIdsOutput
  return taxids

retrieveGIsEntrez :: [String] -> IO [Int]
retrieveGIsEntrez accessions = do
  gisOutput <- retrieveElementsEntrez accessions retrieveGIEntrez
  let parsedGisOutput = map parseGIfromEntrez gisOutput
  let gis = concatMap fromRight parsedGisOutput
  return gis

retrieveGIEntrez :: [String] -> IO String
retrieveGIEntrez accessions = do
       let idList = intercalate "," accessions
       let query' = "retmax=400&term=" ++ idList
       let entrezQuery = EntrezHTTPQuery (Just "esearch") (Just "nucleotide") query'
       threadDelay 10000000                  
       result <- entrezHTTP entrezQuery
       return result

parseGIfromEntrez :: String -> Either ParseError [Int]
parseGIfromEntrez input = parse genParserGIfromEntrez "genParserGIfromEntrez" input

genParserGIfromEntrez :: GenParser Char st [Int]
genParserGIfromEntrez = do
  string "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n<!DOCTYPE eSearchResult PUBLIC \"-//NLM//DTD esearch 20060628//EN\" \"http://eutils.ncbi.nlm.nih.gov/eutils/dtd/20060628/esearch.dtd\">\n"
  string "<eSearchResult><Count>"
  many1 digit
  string "</Count><RetMax>"
  many1 digit 
  string "</RetMax><RetStart>"
  many1 digit
  string "</RetStart><IdList>\n"
  gis <- many1 (Text.ParserCombinators.Parsec.try parseGI)
  string "</IdList><TranslationSet/><QueryTranslation/></eSearchResult>"
  return gis

parseGI :: GenParser Char st Int
parseGI = do
  string "<Id>"
  gi <- many1 digit
  string "</Id>\n"
  return (readInt gi)
  
retrieveTaxIdEntrez :: [Int] -> IO String
retrieveTaxIdEntrez geneIds = do
  let geneIdStrings = map show geneIds
  let idList = intercalate "," geneIdStrings
  let query' = "id=" ++ idList
  let entrezQuery = EntrezHTTPQuery (Just "esummary") (Just "nucleotide") query'
  threadDelay 10000000                  
  result <- entrezHTTP entrezQuery
  return result

readEntrezTaxId :: String -> [Int]
readEntrezTaxId input = runLA (xreadDoc >>> getEntrezTaxId) input

getEntrezTaxId :: ArrowXml a => a XmlTree Int
getEntrezTaxId = getChildren >>> atTag "DocSum" >>>
  proc entrezDocSum -> do
  taxId <- atName "TaxId" >>> getChildren >>> getText -< entrezDocSum
  returnA -< read taxId :: Int
             
readInt :: String -> Int
readInt = read

-- | gets all subtrees with the specified tag name
atTag :: ArrowXml a =>  String -> a XmlTree XmlTree
atTag tag = deep (isElem >>> hasName tag)

atName :: ArrowXml a => String -> a XmlTree XmlTree
atName elementId = deep (isElem >>> hasAttrValue "Name" (== elementId))

