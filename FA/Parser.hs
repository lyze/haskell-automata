{-# OPTIONS -Wall -fwarn-tabs -fno-warn-orphans #-}
{-# LANGUAGE ScopedTypeVariables, ViewPatterns, GADTs, StandaloneDeriving,
  MultiParamTypeClasses, FlexibleInstances, FlexibleContexts #-}

module FA.Parser
  ( genFAParser
  ) where

import Data.Traversable
import Data.Char
import FA.Automaton
import FA.DirectState
import FA.IndirectState
import FA.FromListable (FromListable)
import qualified FA.FromListable as FromListable
import qualified FA.Mapping as Mapping
import FA.Mapping (Mapping)
import FA.MapMaybeable (MapMaybeable)
import Text.ParserCombinators.Parsec
import Text.Read (readMaybe)

genFAParser :: (Eq a, Eq t, Eq id, Read t, Read id, Read a, FromListable coll, Traversable coll, MapMaybeable map a, Mapping map a) => Parser (GenFA coll (GenFAState id t map a))
genFAParser = do st's <- endBy1 dfaState'P spaces
                 let sts@(st:_) = indirectToDirectStates st's
                 return $ GenFA (FromListable.fromList sts) st
  where -- dfaState'P :: Parser (id, (GenFAState' id t map a))
        dfaState'P = do k <- identifier
                        spaces1
                        t <- stateTypeP
                        spaces
                        nIds <- symbolToIdMap
                        return $ GenFAState' k t nIds
          where symbol = readPM "expected symbol of alphabet" symbolString

                -- symbolToIdMap :: Parser (map a id)
                symbolToIdMap = do
                  symbolToIdList <- endBy transition spaces
                            <?> "expected (symbol, state identifier) list"
                  return $ Mapping.fromList symbolToIdList

                identifier = readPM "expected state identifier" identifierString
                stateTypeP = readPM "expected state type" stateTypeString
                transition = symbol `tuple` identifier

identifierString :: Parser String
identifierString = many1 alphaNum

stateTypeString :: Parser String
stateTypeString = many1 alphaNum

symbolString :: Parser String
symbolString = many1 $ satisfy $ \c -> not $ isSpace c || c == ','

readPM     :: Read a => String -> Parser String -> Parser a
readPM err = (readM =<<)
  where readM s = case readMaybe s of
                    Just a  -> return a
                    Nothing -> fail err

spaces1 :: Parser ()
spaces1 = space >> skipMany space

parens :: Parser a -> Parser a
parens = between (char '(') (char ')')

tuple     :: Parser a -> Parser b -> Parser (a, b)
tuple x y = parens $ do
              spaces
              a <- x
              spaces
              _ <- char ','
              spaces
              b <- y
              spaces
              return (a, b)
