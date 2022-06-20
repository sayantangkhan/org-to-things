{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module: OrgToThings.ParserMonad
--
-- Parser monad to parse String, \[Inline\], and \[Block\].
module OrgToThings.Parser where

import Control.Applicative (Alternative (..))
import Control.Monad
import Control.Monad.Trans.State.Strict
import Data.Bifunctor (first)
import Data.Char (isDigit)
import qualified Data.Text as T
import OrgToThings.Definitions
import Text.Pandoc.Definition

-- | Generalized parser Monad, where t is the token type, e is the error type, and a is the return type.
newtype TokenParser t e a = TokenParser {unParser :: StateT [t] (Either e) a}

parserConstructor :: ([t] -> Either e (a, [t])) -> TokenParser t e a
parserConstructor = TokenParser . StateT

type CharParser = TokenParser Char String

type InlineParser = TokenParser Inline (String, Maybe Inline)

runParser :: TokenParser t e a -> [t] -> Either e (a, [t])
runParser = runStateT . unParser

evalParser :: TokenParser t e a -> [t] -> Either e a
evalParser = evalStateT . unParser

instance Functor (TokenParser t e) where
  fmap :: (a -> b) -> TokenParser t e a -> TokenParser t e b
  fmap f p = TokenParser $ f <$> unParser p

instance Applicative (TokenParser t e) where
  pure :: a -> TokenParser t e a
  pure a = TokenParser $ pure a
  (<*>) :: TokenParser t e (a -> b) -> TokenParser t e a -> TokenParser t e b
  f <*> a = TokenParser $ unParser f <*> unParser a

instance Alternative CharParser where
  empty :: CharParser a
  empty = TokenParser $ StateT Left
  (<|>) :: CharParser a -> CharParser a -> CharParser a
  a <|> b = TokenParser $ StateT $ \s -> combine (runStateT (unParser a) s) (runStateT (unParser b) s)
    where
      combine (Left _) y = y
      combine x _ = x

instance Alternative InlineParser where
  empty :: InlineParser a
  empty = TokenParser $
    StateT $ \case
      [] -> Left ("Expected non-empty input", Nothing)
      (x : _) -> Left ("Unexpected Inline", Just x)
  (<|>) :: InlineParser a -> InlineParser a -> InlineParser a
  a <|> b = TokenParser $ StateT $ \s -> combine (runStateT (unParser a) s) (runStateT (unParser b) s)
    where
      combine (Left _) y = y
      combine x _ = x

instance Monad (TokenParser t e) where
  (>>=) :: TokenParser t e a -> (a -> TokenParser t e b) -> TokenParser t e b
  a >>= f = TokenParser $
    StateT $ \s -> do
      (a', s') <- runParser a s
      runParser (f a') s'

expectedMessage :: String -> CharParser a -> CharParser a
expectedMessage expected parser = parserConstructor applyParser
  where
    applyParser s = case runParser parser s of
      Left _ -> Left ("Expected " ++ expected ++ ". Found " ++ s)
      x -> x

parseAnyChar :: CharParser Char
parseAnyChar = parserConstructor $ \case
  [] -> empty
  (c : cs) -> pure (c, cs)

parseStringEOL :: CharParser ()
parseStringEOL = parserConstructor $ \case
  [] -> pure ((), [])
  c -> Left $ "Expected PARSESTRINGEOL. Found '" ++ c ++ "'"

peekAnyChar :: CharParser Char
peekAnyChar = parserConstructor $ \case
  [] -> empty
  (c : cs) -> pure (c, c : cs)

satisfyCombinatorString :: (Char -> Bool) -> CharParser Char
satisfyCombinatorString predicate = do
  c <- peekAnyChar
  guard $ predicate c
  parseAnyChar

parseChar :: Char -> CharParser Char
parseChar = satisfyCombinatorString . (==)

parseDigit :: CharParser Char
parseDigit = satisfyCombinatorString isDigit

parseYear :: CharParser Int
parseYear = read <$> replicateM 4 parseDigit

parseDate :: CharParser (Int, Int, Int)
parseDate = do
  year <- read <$> replicateM 4 parseDigit
  void $ parseChar '-'
  month <- read <$> replicateM 2 parseDigit
  void $ parseChar '-'
  day <- read <$> replicateM 2 parseDigit
  return (year, month, day)

parseTime :: CharParser (Int, Int)
parseTime = do
  hour <- read <$> replicateM 2 parseDigit
  void $ parseChar ':'
  minute <- read <$> replicateM 2 parseDigit
  return (hour, minute)

parseScheduledString :: CharParser Scheduled
parseScheduledString = expectedMessage "schedule" $ (planningDateTime <|> planningDate) <* parseStringEOL
  where
    planningDateTime = do
      date <- parseChar '<' *> parseDate <* replicateM 5 parseAnyChar
      time <- parseTime <* parseChar '>'
      return $ DateTimeS (date, time)
    planningDate = do
      date <- parseChar '<' *> parseDate <* replicateM 4 parseAnyChar <* parseChar '>'
      return $ DateS date

parseDeadlineString :: CharParser Deadline
parseDeadlineString = expectedMessage "deadline" $ planningDate <* parseStringEOL
  where
    planningDate = do
      date <- parseChar '<' *> parseDate <* replicateM 4 parseAnyChar <* parseChar '>'
      return $ DateD date

readScheduled :: T.Text -> Either String Scheduled
readScheduled t = evalParser parseScheduledString (T.unpack t)

readDeadline :: T.Text -> Either String Deadline
readDeadline t = evalParser parseDeadlineString (T.unpack t)

parseWhitespaceInline :: InlineParser ()
parseWhitespaceInline = parserConstructor $ \case
  (Space : xs) -> pure ((), xs)
  (x : _) -> Left ("Expected Space", Just x)
  _ -> Left ("Expected input", Nothing)

parseEOLInline :: InlineParser ()
parseEOLInline = parserConstructor $ \case
  [] -> pure ((), [])
  (x : _) -> Left ("Expected empty input", Just x)

parseSingleStrInline :: T.Text -> InlineParser T.Text
parseSingleStrInline text = parserConstructor $ \case
  (x@(Strong [Str inner_text]) : xs) ->
    if inner_text == text
      then pure (inner_text, xs)
      else Left ("Expected" ++ T.unpack text ++ ".", Just x)
  (x : _) -> Left ("Expected" ++ T.unpack text ++ ".", Just x)
  _ -> Left ("Expected input", Nothing)

parseDeadlineMarkInline :: InlineParser ()
parseDeadlineMarkInline = void $ parseSingleStrInline "DEADLINE:"

parseScheduledMarkInline :: InlineParser ()
parseScheduledMarkInline = void $ parseSingleStrInline "SCHEDULED:"

parseDeadlineInline :: InlineParser Deadline
parseDeadlineInline = parseDeadlineMarkInline *> parseWhitespaceInline *> parseInlineDeadline
  where
    parseInlineDeadline = parserConstructor $
      \case
        (Emph [Str deadline_text] : xs) -> case readDeadline deadline_text of
          Right d -> pure (d, xs)
          Left e -> Left (e, Just $ Emph [Str deadline_text])
        (x : _) -> Left ("Expected deadline", Just x)
        _ -> Left ("Expected input", Nothing)

parseScheduledInline :: InlineParser Scheduled
parseScheduledInline = parseScheduledMarkInline *> parseWhitespaceInline *> parseInlineScheduled
  where
    parseInlineScheduled = parserConstructor $
      \case
        (Emph [Str scheduled_text] : xs) -> case readScheduled scheduled_text of
          Right d -> pure (d, xs)
          Left e -> Left (e, Just $ Emph [Str scheduled_text])
        (x : _) -> Left ("Expected schedule", Just x)
        _ -> Left ("Expected input", Nothing)

parsePlanningInline :: InlineParser (Maybe Scheduled, Maybe Deadline)
parsePlanningInline = scheduled_then_deadline <|> deadline_then_scheduled <|> only_scheduled <|> only_deadline
  where
    scheduled_then_deadline = do
      scheduled <- parseScheduledInline
      parseWhitespaceInline
      deadline <- parseDeadlineInline
      return (Just scheduled, Just deadline)
    deadline_then_scheduled = do
      deadline <- parseDeadlineInline
      parseWhitespaceInline
      scheduled <- parseScheduledInline
      return (Just scheduled, Just deadline)
    only_scheduled = do
      scheduled <- parseScheduledInline
      return (Just scheduled, Nothing)
    only_deadline = do
      deadline <- parseDeadlineInline
      return (Nothing, Just deadline)

parseStrInline :: InlineParser T.Text
parseStrInline = T.concat <$> some singleParser
  where
    singleParser = parserConstructor $ \case
      (Str inner_text : xs) -> pure (inner_text, xs)
      (Space : xs) -> pure (" ", xs)
      (x : _) -> Left ("Expected Str or Space", Just x)
      _ -> Left ("Expected input Str", Nothing)

-- | A useful parser combinator
greedyManyTill :: TokenParser t e a -> TokenParser t e end -> TokenParser t e [a]
greedyManyTill parser endParser = parserConstructor innerLambda
  where
    innerLambda s = case runParser parser s of
      Left _ -> baseCase s
      Right (a, u) -> case innerLambda u of
        Right (as, v) -> Right (a : as, v)
        Left _ -> baseCase s
    baseCase s = case runParser endParser s of
      Right (_, u) -> Right ([], u)
      Left x -> Left x

sepBy :: TokenParser t e a -> TokenParser t e sep -> TokenParser t e [a]
sepBy parser sepParser = parserConstructor innerLambda
  where
    innerLambda s = case runParser parser s of
      Right (a, u) -> case runParser sepParser u of
        Right (_, v) -> first (a :) <$> innerLambda v
        Left _ -> Right ([a], u)
      Left x -> Left x

optional :: TokenParser t e a -> TokenParser t e (Maybe a)
optional parser = parserConstructor innerLambda
  where
    innerLambda s = case runParser parser s of
      Right (a, u) -> Right (Just a, u)
      Left _ -> Right (Nothing, s)

parseTodoAndTagsInline :: InlineParser (T.Text, [T.Text])
parseTodoAndTagsInline = do
  isTodo
  parseWhitespaceInline
  title <- parseTitle
  tags <- extract <$> optional parseTags
  return (title, tags)
  where
    isTodo = parserConstructor $ \case
      ((Span _ [Str "TODO"]) : xs) -> Right ((), xs)
      (x : _) -> Left ("Expected 'TODO'", Just x)
      _ -> Left ("Expected input", Nothing)
    parseStr :: InlineParser T.Text
    parseStr = parserConstructor $ \case
      (Str inner_text : xs) -> pure (inner_text, xs)
      (Space : xs) -> pure (" ", xs)
      (x : _) -> Left ("Expected Str or Space", Just x)
      _ -> Left ("Expected input", Nothing)
    parseTitle = T.concat <$> greedyManyTill parseStr (parseWhitespaceInline <|> parseEOLInline)
    parseTag = parserConstructor $ \case
      (Span (_, _, [("tag-name", tag)]) _ : xs) -> pure (tag, xs)
      (x : _) -> Left ("Expected tag", Just x)
      _ -> Left ("Expected input", Nothing)
    parseSep = parserConstructor $ \case
      (Str "\160" : xs) -> pure ((), xs)
      (x : _) -> Left ("Expected separator", Just x)
      _ -> Left ("Expected input", Nothing)
    parseTags = sepBy parseTag parseSep
    extract :: Maybe [a] -> [a]
    extract Nothing = []
    extract (Just x) = x

parseNotesInline :: InlineParser T.Text
parseNotesInline = T.concat <$> many combinedParser <* parseEOLInline
  where
    combinedParser = parseStrInline <|> parseSpecialInline

parseSpecialInline :: InlineParser T.Text
parseSpecialInline = parserConstructor $ \case
  (Link _ text (url, _) : xs) -> do
    parsed_text <- evalParser (parseStrInline <* parseEOLInline) text
    return (T.concat ["(", parsed_text, ")[", url, "]"], xs)
  (Emph text : xs) -> do
    parsed_text <- evalParser (parseStrInline <* parseEOLInline) text
    return (T.concat ["*", parsed_text, "*"], xs)
  (Strong text : xs) -> do
    parsed_text <- evalParser (parseStrInline <* parseEOLInline) text
    return (T.concat ["**", parsed_text, "**"], xs)
  (Code _ text : xs) -> Right (T.concat ["`", text, "`"], xs)
  (x : _) -> Left ("Expected link", Just x)
  _ -> Left ("Expected input", Nothing)
