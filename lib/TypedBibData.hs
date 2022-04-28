{-# LANGUAGE LambdaCase #-}
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
import Text.ParserCombinators.Parsek as Parsek


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

-- >>> test
-- Left [([("\"{\"",Just '\n')],"satisfy"),([("\"\\\\\"",Just '\n')],"satisfy"),([],"satisfy"),([("\"}\"",Just '\n')],"satisfy")]

test = parse (pAuthBlock True) longestResult
 "{students of the\nUtrecht University Generic Programming class}"
 -- "Ba, Jimmy"


pAuthLastFirst = do
  lst <- pAuthName
  spaces
  _ <- string ","
  spaces
  frst <- pAuthName
  return (frst,lst)

pAuthFirstLast = do
  frst <- pAuthName
  _ <- some space
  lst <- pAuthNamePart
  return (frst,lst)

pAuthLastOnly = do
  lst <- pAuthNamePart
  return ("",lst)

pAuth :: Parser Char ([Char], [Char])
pAuth = pAuthFirstLast <|> pAuthLastFirst <|> pAuthLastOnly

pAuthors :: Parser Char [([Char], [Char])]
pAuthors = pAuth `Parsek.sepBy1` (some space >> string "and" >> some space)
           <|> return [("","UnknownAuthor")]


pAuthName :: Parser Char [Char]
pAuthName = intercalate " " <$> (pAuthNamePart `sepBy1` some space)

pAuthNamePart :: Parser Char [Char]
pAuthNamePart =
  do n <- concat <$> some (pAuthBlock False)
     when (n == "and") (fail "and is not a name")
     return n

pAuthBlock :: Bool -> Parser Char String
pAuthBlock allowSpace =
   (\open body close -> open : body ++ close : []) <$> (Parsek.char '{') <*> (concat <$> many (pAuthBlock True)) <*> (Parsek.char '}') <|>
   sequence
      [Parsek.char '\\',
       Parsek.oneOf "{}'`^&%\".,~# " <|> Parsek.letter] <|>
   munch1 (not . (`elem` ((if allowSpace then "" else "\n\t " ) ++ "{},")))


-- | When searching ignore special characters
project :: String -> String
project = map toLower . filter isAlphaNum

-- | Does a node contain a string (for search)
contains :: Entry -> String -> Bool
contains t needle = or [needle `isInfixOf` project txt | txt <- findTitle t : map snd (authors t)]

matchSearch :: Entry -> String -> Bool
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

eqField :: [Char] -> [Char] -> Bool
eqField = (==) `on` (map toLower)

findField' :: String -> Entry -> [String]
findField' f  Entry {..} = map snd (filter ((eqField f) . fst) otherFields)

findFirstAuthor :: Entry -> String
findFirstAuthor Entry{..} = map snd authors ?? "UnknownAuthor"

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

entryToTree :: Entry.T -> Either String Entry
entryToTree Entry.Cons{..} =
  do authors <- authorsToTree (concatMap snd auths)
     return Entry {..}
  where
    [auths,fils,seeAlsos,otherFields] = partitions (map (\k -> (eqField k) . fst) ["author","file","see"]) fields
    kind = entryType
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

fileToTree :: [Char] -> ([Char], [Char])
fileToTree = \case
  (':':fs) -> (f, t)
    where (f,':':t) = break (== ':') fs
  other -> (error $ "fileToTree: unexpected: " ++ show (take 10 other))

authorsToTree :: String -> Either String [(String,String)]
authorsToTree s = case parse (pAuthors <* spaces) longestResultWithLeftover s of
   Right (r,[]) -> Right r
   _ -> Left ("parse error in authors name: " ++ s)

seeAlsoToTree r = (how,what)
    where (how,':':what) = break (== ':') r

l ?? b = head $ l ++ [b]

bibToForest :: [T] -> Either String [Entry]
bibToForest xs = mapM entryToTree xs

formatEntry :: Entry.T -> String
formatEntry (Entry.Cons entryType bibId items) =
   let formatItem (name, value) =
         "\t"++name++" = {"++value++"}"
   in  "@" ++ entryType ++ "{" ++ bibId ++ ",\n" ++
       intercalate ",\n" (map formatItem items) ++
       "\n}\n\n"


e1 `isSeeAlso` e2 = findNiceKey e1 `elem` (map snd (seeAlso e2))

areRelated e1 e2 = e1 `isSeeAlso` e2 || e2 `isSeeAlso` e1


