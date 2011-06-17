{-# LANGUAGE OverloadedStrings, ViewPatterns #-}
{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts #-}
module DviAsm
    ( DataLine
    , SectionHeader
    , Section
    , loadDviAsm
    , saveDviAsm
    ) where

import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy.Char8 as L8
import Control.Applicative hiding (many, (<|>), optional)
import Control.Monad.State
import Data.Maybe
import Text.Parsec
import Text.Parsec.ByteString.Lazy

type DataLine = (B8.ByteString, B8.ByteString)
type SectionHeader = [B8.ByteString]
type Section = (SectionHeader, [DataLine])

spaces' = skipMany (char ' ')

alstr :: Parser B8.ByteString
alstr = B8.pack <$> many1 alphaNum

sectionHeader :: Parser SectionHeader
sectionHeader = between (char '[') (char ']') (sepBy1 alstr spaces') <* newline

dataLine :: Parser DataLine
dataLine = pure (,) <*> op <*> (B8.pack <$> manyTill anyChar newline)
    where op = alstr <* (string ":" <* optional (char ' '))

dataLines :: Parser [DataLine]
dataLines = catMaybes <$> many1 (spaces' *> line)
    where line = (newline *> pure Nothing) <|> (Just <$> dataLine)

section :: Parser Section
section = pure (,) <*> sectionHeader <*> dataLines

sections :: Parser [Section]
sections = many1 section <* eof

-- TODO: lazy parser


format :: [Section] -> L8.ByteString
format s = L8.unlines $ map (L8.fromChunks . (:[])) $ concatMap formatSection s

formatSection :: Section -> [B8.ByteString]
formatSection (h, ls) = formatHeader h : evalState (mapM formatDataLine ls) 0 ++ [""]

formatHeader :: SectionHeader -> B8.ByteString
formatHeader h = '[' `B8.cons` B8.intercalate " " h `B8.append` "]"

formatDataLine :: (MonadState Int m) => DataLine -> m B8.ByteString
formatDataLine (a, b) = do
    when (a == "pop") $ modify pred
    i <- get
    when (a == "push") $ modify succ
    return $ B8.replicate (i * 2) ' ' `B8.append` a `B8.append` ": " `B8.append` b


loadDviAsm :: String -> IO [Section]
loadDviAsm f = do
    Right x <- parseFromFile sections f
    return x

saveDviAsm :: FilePath -> [Section] -> IO ()
saveDviAsm f = L8.writeFile f . format
