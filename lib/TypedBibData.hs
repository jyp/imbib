{-# LANGUAGE RecordWildCards, TupleSections  #-}

module TypedBibData where

import Control.Applicative hiding ((<|>),many)
import Control.Monad
import Control.Monad.Trans
import Data.Char
import Data.List
import Data.List.Split
import Data.Maybe
import Data.Tree
import Text.BibTeX.Entry as Entry
import Text.BibTeX.Parse
import Data.Function
import Text.ParserCombinators.Parsec as Parsec


-------------------------------------
-- Bib Data manipulation

data Entry = Entry {
      kind :: String,
      authors :: [(String,String)],
      files :: [(String,String)],      -- ^ name, type
      seeAlso :: [(String,String)],
      otherFields :: [(String,String)]
    } deriving (Show, Eq, Ord)


renderTex = filter (`notElem` "{}")

sanitizeIdent = filter (\c -> (c >= 'a' && c <= 'z') || isDigit c || c `elem` ".-_?+") . 
                map ((\c -> if isSpace c then '_' else c) . toLower)


parseBib = skippingSpace $ skippingLeadingSpace Text.BibTeX.Parse.file

pAuth :: Parser (String, String)
pAuth = do
  many Parsec.space
  p1 <- pAuthNamePart `Parsec.sepBy` (many Parsec.space)
  do Parsec.char ','
     many Parsec.space
     first <- pAuthName
     return (first, intercalate " " p1)
    <|> let (first,[last]) = splitAt (length p1 - 1) p1 in
            return (intercalate " " first, last)

pAuthName = concat <$> many (pAuthBlock True)
pAuthNamePart = concat <$> many1 (pAuthBlock False)

pAuthBlock :: Bool -> Parser String
pAuthBlock spaceOk =
   liftM3 (\open body close -> open : body ++ close : [])
      (Parsec.char '{') pAuthName (Parsec.char '}') <|>
   sequence
      [Parsec.char '\\',
       Parsec.oneOf "{}'`^&%\".,~# " <|> Parsec.letter] <|>
   fmap (:[]) (Parsec.noneOf $ [' ' | not spaceOk] ++ "},")


-- When searching ignore special characters
project = map toLower . filter isAlphaNum

-- | Does a node contain a string (for search)
contains :: Entry -> String -> Bool
contains t needle = or [needle `isInfixOf` project txt | txt <- findTitle t : map snd (authors t)]

matchSearch entry pattern =  all (contains entry) (map project $ words pattern)  

findCiteAuth :: Entry -> String
findCiteAuth Entry {..} = renderTex $ case map snd authors of
      [] -> "????"
      [a] -> a
      [a,b] -> a ++ " and " ++ b
      [a,b,c] -> a ++ ", " ++ b ++ " and " ++ c
      (a:_) -> a ++ " et al."

findYear :: Entry -> String
findYear = findField "year" 

findTitle :: Entry -> String
findTitle = findField "title"

findField :: String -> Entry -> String
findField f t = findField' f t ?? "????"

findField' f  Entry {..} = map snd (filter ((== f) . fst) otherFields)

findFirstAuthor :: Entry -> String
findFirstAuthor Entry{..} = map snd authors ?? "????"

findCite t = findCiteAuth t ++ " " ++ findYear t
findNiceKey t = findField' "forcedkey" t ?? 
                (intercalate "_" $ map sanitizeIdent $ [findFirstAuthor t, title, findYear t])
    where title = ((filter ((> 2) . length) . map sanitizeIdent . words . findTitle $ t) 
                   \\ ["de","am","for","le","an","to","be","on","make","the","how","why","its","from","towards","does"])
                  ?? "????"

findFullText Entry {..} = map fst . filter ((`elem` ["pdf","ps"]) . snd) $ files

addFile f (Entry {..})= Entry {files = f:files,..}

partitions :: [a -> Bool] -> [a] -> [[a]]
partitions [] l = [l]
partitions (x:xs) l = yes : partitions xs no
    where (yes,no) = partition x l

entryToTree :: Entry.T -> Entry
entryToTree Entry.Cons{..} = Entry {..}
  where
    [auths,fils,seeAlsos,otherFields] = partitions (map (\k -> (k ==) . fst) ["author","file","see"]) fields
    kind = entryType
    authors = [authorToTree a | (_,as) <- auths, a <- splitOn " and " as]
    ident = identifier
    files = [fileToTree f | (_,fs) <- fils, f <- splitOn ";" fs]
    seeAlso = [seeAlsoToTree r | (_,rs) <- seeAlsos, not $ null rs, r <- splitOn ";" rs ]

treeToEntry :: Entry -> Entry.T
treeToEntry t@Entry {..} = Entry.Cons{..}
   where fields = ("author", intercalate " and " [first ++ " " ++ last | (first,last) <- authors]) :
                  [("file",intercalate ";" [":" ++ f ++ ":" ++ t | (f,t) <- files]) | not $ null files] ++
                  otherFields ++
                  [("see",intercalate ";" [how ++ ":" ++ what | (how,what) <- seeAlso]) | not $ null seeAlso]
         entryType = kind
         identifier = findNiceKey t 

fileToTree (':':fs) = (f, t) 
    where (f,':':t) = break (== ':') fs

authorToTree :: String -> (String,String)
authorToTree s = case parse pAuth ("in " ++ s) s of
                            Left err -> error $ show err
                            Right r -> r

seeAlsoToTree r = (how,what)
    where (how,':':what) = break (== ':') r

l ?? b = head $ l ++ [b]

bibToForest = map entryToTree 

formatEntry :: Entry.T -> String
formatEntry (Entry.Cons entryType bibId items) =
   let formatItem (name, value) =
         "\t"++name++" = {"++value++"}"
   in  "@" ++ entryType ++ "{" ++ bibId ++ ",\n" ++
       intercalate ",\n" (map formatItem items) ++
       "\n},\n\n"


e1 `isSeeAlso` e2 = findNiceKey e1 `elem` (map snd (seeAlso e2))

areRelated e1 e2 = e1 == e2 || e1 `isSeeAlso` e2 || e2 `isSeeAlso` e1
