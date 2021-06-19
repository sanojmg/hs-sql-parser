{-# LANGUAGE OverloadedStrings #-}
module Main where

-- import Data.Semigroup ((<>))
-- import Options.Applicative
-- import System.IO
-- import qualified Data.Text as T  
-- import qualified Data.Text.IO as TIO 
-- import qualified Text.Parsec as P (ParseError) 

-- import Prettyprinter
-- import Prettyprinter.Render.Text

-- import Parser
-- import Pretty
-- import Type

-- data Conf = Conf { inputFile :: Maybe FilePath,
--                    outputFile :: Maybe FilePath,
--                    prettyPrint :: Bool
--             }
--              deriving Show

-- conf :: Parser Conf
-- conf = Conf 
--     <$> (optional $ strOption 
--          ( long "input-file"
--          <> short 'i'
--          <> metavar "INPUTFILE"
--          <> help "Input File" ))
--     <*> (optional $ strOption 
--          ( long "output-file"
--          <> short 'o'
--          <> metavar "OUTPUTFILE"
--          <> help "Output File" ))
--     <*> switch
--          ( long "pretty"
--          <> short 'p'
--          <> help "Whether to pretty print the AST" )
 
-- opts :: ParserInfo Conf 
-- opts = info (conf <**> helper)
--     ( fullDesc 
--      <> progDesc "Parse the SQL query and pretty print the AST"
--      <> header "hs-sql-parser: A parser in Haskell for simple SQL like queries")

main :: IO ()
main = print "Undefined"

-- main :: IO ()
-- main = do
--     config <- execParser opts
--     parseSQL config 

-- parseSQL :: Conf -> IO ()
-- parseSQL conf = do
--     sqlStr <- readInFile $ inputFile conf
--     let ast = runParser $ T.unpack sqlStr
--     writeAST ast (outputFile conf) (prettyPrint conf)
    
-- readInFile (Just fileName) = TIO.readFile fileName
-- readInFile Nothing = TIO.getContents

-- writeAST :: Either P.ParseError SelectStmnt -> Maybe FilePath -> Bool -> IO ()
-- writeAST (Left err) _ _ = print $ "Error parsing SQL: " <> show err
-- writeAST (Right sel) outFile isPretty = 
--     if isPretty then
--         writeDocOutFile outFile (pretty sel)
--     else
--         writeOutFile outFile (T.pack $ show sel)
    
-- writeOutFile (Just fileName) str = TIO.writeFile fileName str
-- writeOutFile Nothing str = TIO.putStr str   

-- writeDocOutFile Nothing doc = hPutDoc stdout doc 
-- writeDocOutFile (Just fileName) doc = withFile fileName WriteMode act
--     where
--         act h = hPutDoc h doc
 