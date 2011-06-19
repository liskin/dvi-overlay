{-# LANGUAGE OverloadedStrings, ViewPatterns, FlexibleContexts #-}
module DviAsm
    ( DataLine
    , SectionHeader
    , Section
    , loadDviAsm
    , saveDviAsm
    ) where

import qualified Data.ByteString.Lazy.Char8 as L8
import Control.Monad.State
import Data.Char
import Data.Int

type DataLine = (L8.ByteString, L8.ByteString)
type SectionHeader = L8.ByteString
type Section = (SectionHeader, [DataLine])


isEmptyLine :: L8.ByteString -> Bool
isEmptyLine l = L8.all isSpace l || L8.head l == '%'

parseSectionHeader :: L8.ByteString -> SectionHeader
parseSectionHeader h | L8.head h == '[' && L8.last h == ']' = h'
    where h' = L8.tail $ L8.init h
parseSectionHeader _ = error "malformed section header"

parseDataLine :: L8.ByteString -> DataLine
parseDataLine (L8.dropWhile isSpace -> l) = (a, snd $ L8.splitAt 2 b)
    where (a, b) = L8.break (==':') l

parseSection :: [L8.ByteString] -> (Section, [L8.ByteString])
parseSection (h:r) = ((parseSectionHeader h, map parseDataLine ls), r')
    where (ls, r') = break ("[" `L8.isPrefixOf`) r

parseSections :: [L8.ByteString] -> [Section]
parseSections [] = []
parseSections s = x : parseSections rest
    where (x, rest) = parseSection s

loadDviAsm :: String -> IO [Section]
loadDviAsm f = do
    f <- (filter (not . isEmptyLine) . L8.lines) `fmap` L8.readFile f
    return $ parseSections f


format :: [Section] -> L8.ByteString
format s = L8.unlines $ concatMap formatSection s

formatSection :: Section -> [L8.ByteString]
formatSection (h, ls) = formatHeader h : evalState (mapM formatDataLine ls) 0 ++ [""]

formatHeader :: SectionHeader -> L8.ByteString
formatHeader h = '[' `L8.cons` h `L8.append` "]"

formatDataLine :: (MonadState Int64 m) => DataLine -> m L8.ByteString
formatDataLine (a, b) = do
    when (a == "pop") $ modify pred
    i <- get
    when (a == "push") $ modify succ
    return $ L8.replicate (i * 2) ' ' `L8.append` a `L8.append` ": " `L8.append` b

saveDviAsm :: FilePath -> [Section] -> IO ()
saveDviAsm f = L8.writeFile f . format


