--- Query Parser
--- 
-- file: ch16/csv6.hs
{-# LANGUAGE OverloadedStrings #-}
module QueryParser where

import Control.Applicative
import qualified Data.Text as T
import Text.ParserCombinators.Parsec hiding (many, optional, (<|>))

-- one kind of level
data LevelT = LevelI | LevelW | LevelE
    deriving (Show, Eq, Ord)

data LevelFilterQ = LevelFilterQCtor [LevelT]
    deriving (Show, Eq, Ord)

p_level_q :: CharParser () LevelFilterQ
p_level_q = LevelFilterQCtor <$> p_level_eq

p_level_eq :: CharParser () [LevelT]
p_level_eq = spaces *> (p_level_token) *> spaces *> char '=' *> spaces *> p_level_many

p_level_token :: CharParser () String
p_level_token = try (string "$level")
                <|> (string "$l")

p_level_many :: CharParser () [LevelT]
p_level_many = (p_level1 <* spaces) `sepBy` (char '|' <* spaces)

p_level1 :: CharParser () LevelT
p_level1 = LevelI <$ string "I"
        <|> LevelW <$ string "W"
        <|> LevelE <$ string "E"

-- one type filter
data TypeFilter = TypeContains T.Text --- TypeContains String TypeFilter String
    deriving (Show, Eq, Ord)

data TypeFilterQ = TypeFilterOr [TypeFilter]
                    | TypeFilterAnd [TypeFilter]
    deriving (Show, Eq, Ord)

-- "$type = FM"
-- "$t~FM"
p_type_eq :: CharParser () TypeFilter
p_type_eq = p_type_token *> spaces *> p_type_operator *> spaces *> p_type1

p_type_token :: CharParser () String
p_type_token = try (string "$type")
                <|> (string "$t")

p_type_operator :: CharParser () String
p_type_operator = string "=" 
                <|> string "~"

p_type1 :: CharParser () TypeFilter
p_type1 = (TypeContains . T.pack)  <$> (spaces *> many1 letter)

-- 
--
data MessageFilter = MessageContains T.Text --- MessageContains String -- MessageIs String --- MessageMatchesPattern String
    deriving (Show, Eq, Ord)

p_message_eq :: CharParser () MessageFilter
p_message_eq = p_message_token *> spaces *> p_message_operator *> spaces *> p_message1

p_message_token :: CharParser () String
p_message_token = try (string "$message")
                <|> (string "$m")

p_message_operator :: CharParser () String
p_message_operator = p_type_operator

p_message1 :: CharParser () MessageFilter
p_message1 = (MessageContains . T.pack) <$> (spaces *> many1 letter)


data CombinedFilter = CombinedFilterMessage MessageFilter
                        | CombinedFilterType TypeFilter
                        | CombinedFilterLevel LevelFilterQ
                    deriving (Show, Eq, Ord)

-- "$l = E, $t=fm, $m=kljl"
p_combinedFilter :: CharParser () [CombinedFilter]
p_combinedFilter = (p_combinedFilter1 <* spaces) `sepBy` (char ',' <* spaces)

p_combinedFilter1 :: CharParser () CombinedFilter
p_combinedFilter1 = 
                try (CombinedFilterMessage <$> p_message_eq)
                <|> try (CombinedFilterType <$> p_type_eq)
                <|> try (CombinedFilterLevel <$> p_level_q)

runCombinedFilterParserOnQuery :: T.Text -> Either ParseError [CombinedFilter]
runCombinedFilterParserOnQuery input = parse p_combinedFilter "(unknown)" (T.unpack input)

quotedText = 
    do char '"'
       content <- many quotedChar
       char '"' <?> "quote at end of cell"
       return content

quotedChar =
        noneOf "\"\\"
    <|> try (string "\\\"" >> return '"')
    <|> try (string "\\\\" >> return '\\')

--- parser ----