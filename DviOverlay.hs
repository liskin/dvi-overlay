{-# LANGUAGE OverloadedStrings, ViewPatterns, TemplateHaskell #-}
{-# LANGUAGE ParallelListComp #-}
module Main where

import Control.Monad.Error
import Control.Monad.Identity
import qualified Data.ByteString.Char8 as B8
import Data.Function
import Data.Maybe
import Debug.Trace.LocationTH
import System.Environment
import Text.Regex.Posix

import DviAsm

pages :: [Section] -> [Section]
pages = filter (\((h:_), _) -> h == "page")

filterSection :: [B8.ByteString] -> (B8.ByteString -> Bool) -> [Section] -> Maybe Section
filterSection name elems sections = do
    ls <- lookup name sections
    return (name, filter (elems . fst) ls)

checkSection :: [B8.ByteString] -> [B8.ByteString] -> [Section] -> [Section] -> Bool
checkSection n el d1 d2 = f d1 /= f d2
    where f = filterSection n (`elem` el)

maxBy cmp x y = case cmp x y of
    GT -> x
    _  -> y

maxPt :: B8.ByteString -> B8.ByteString -> B8.ByteString
maxPt = maxBy (compare `on` f)
    where
        f (reads . B8.unpack -> [(i, "pt")]) = i :: Double
        f s = error $ B8.unpack s ++ " is not a value in points"

maxBSInt :: B8.ByteString -> B8.ByteString -> Int
maxBSInt = max `on` f
    where
        f (B8.readInt -> Just (i, "")) = i
        f s = error $ B8.unpack s ++ " is not a number"

makePostamble :: [Section] -> [Section] -> Section
makePostamble d1 d2 =
    ( ["postamble"], [ ("maxv" , maxv)
                     , ("maxh" , maxh)
                     , ("maxs" , maxs)
                     , ("pages", pages) ] )
    where
        Just (_, p1) = filterSection ["postamble"] (const True) d1
        Just (_, p2) = filterSection ["postamble"] (const True) d2
        lk x = $check fromJust . lookup x
        maxv = maxPt (lk "maxv" p1) (lk "maxv" p2)
        maxh = maxPt (lk "maxh" p1) (lk "maxh" p2)
        maxs = B8.pack $ show $ maxBSInt (lk "maxs" p1) (lk "maxs" p2) + 1
        pages = lk "pages" p1

makeFontDefs :: [Section] -> [Section] -> Section
makeFontDefs d1 d2 = ( ["font", "definitions"], f1 ++ f2 )
    where
        Just (_, f1) = filterSection ["font", "definitions"] (const True) d1
        Just (_, f2) = filterSection ["font", "definitions"] (const True) d2

fixRef :: B8.ByteString -> B8.ByteString
fixRef s = case getAllSubmatches $ s =~ regex of
    [_, (ind, len)] -> let (a, b) = B8.splitAt ind s in a `B8.append` B8.drop len b
    _ -> s
    where regex = "pdf:bann.*GoTo/D\\(([^\\)]*:)*[^\\)]*:" :: String

fixRefs :: [DataLine] -> [DataLine]
fixRefs = map f
    where f ("xxx", s) = ("xxx", fixRef s)
          f x = x

overlayPage :: Section -> Section -> Section
overlayPage (h, p1) (_, p2) = (h, f p1 ++ f p2)
    where f p = ("push", "") : fixRefs p ++ [("pop", "")]

overlay :: [Section] -> [Section] -> [Section]
overlay d1 d2 = runErr $ do
    when (checkSection ["preamble"] ["id", "numerator", "denumerator", "magnification"] d1 d2) $
        fail "preambles don't match"
    when (checkSection ["postamble"] ["pages"] d1 d2) $
        fail "number of pages differ"
    let begin = [ $check fromJust $ filterSection ["preamble"] (const True) d1
                , makePostamble d1 d2, makeFontDefs d1 d2 ]
        ps = [ overlayPage a b | a <- pages d1 | b <- pages d2 ]
    return $ begin ++ ps

runErr :: Either String a -> a
runErr m = case m of
    Left str -> error str
    Right x  -> x

main = do
    [f1, f2, out] <- getArgs
    d1 <- loadDviAsm f1
    d2 <- loadDviAsm f2
    let x = overlay d1 d2
    saveDviAsm out x
