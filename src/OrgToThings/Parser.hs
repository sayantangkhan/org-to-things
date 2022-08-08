{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module: OrgToThings.ParserMonad
--
-- Parser to parse @[Block]@ and return @[Block]@ with links appended at the appropriate locations.
module OrgToThings.Parser
  ( filterFunc,
    parseArea,
    parseProject,
    parseHeading,
    parseTodoInArea,
    parseTodoWithProject,
    parseTodoWithProjectAndHeading,
    parseNotesBlock,
    parseChecklistBlock,
    parseSingleChecklistInline,
    parseNotesInline,
    parseSpecialInline,
    parseTodoAndTagsInline,
    parsePlanningInline,
    parseDeadlineString,
    parseScheduledString,
    runParser,
  )
where

import Control.Applicative (Alternative (..))
import Control.Monad
import Control.Monad.Trans.State.Strict (StateT (..), evalStateT)
import Data.Bifunctor (first)
import Data.Char (isDigit)
import qualified Data.Text as T
import OrgToThings.Definitions
import OrgToThings.Linkgen (linkFromProject, linkFromTodo)
import Text.Pandoc.Definition

filterFunc :: Pandoc -> Pandoc
filterFunc (Pandoc meta blocks) = case Pandoc meta <$> evalParser parseAgenda blocks of
  Right p -> p
  Left e -> error $ "Filter error: " ++ show e

--
-- Parser monad and combinators
--

-- | Generalized parser Monad, where t is the token type, e is the error type, and a is the return type.
newtype TokenParser t e a = TokenParser {unParser :: StateT [t] (Either e) a}

parserConstructor :: ([t] -> Either e (a, [t])) -> TokenParser t e a
parserConstructor = TokenParser . StateT

type CharParser = TokenParser Char String

type InlineParser = TokenParser Inline (String, Maybe Inline)

type BlockParser = TokenParser Block (String, Maybe Block)

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

instance Alternative BlockParser where
  empty :: BlockParser a
  empty = parserConstructor $ \case
    [] -> Left ("Expected non-empty input", Nothing)
    (x : _) -> Left ("Unexpected Block", Just x)
  (<|>) :: BlockParser a -> BlockParser a -> BlockParser a
  a <|> b = parserConstructor $ \s -> combine (runStateT (unParser a) s) (runStateT (unParser b) s)
    where
      combine (Left _) y = y
      combine x _ = x

instance Monad (TokenParser t e) where
  (>>=) :: TokenParser t e a -> (a -> TokenParser t e b) -> TokenParser t e b
  a >>= f = TokenParser $
    StateT $ \s -> do
      (a', s') <- runParser a s
      runParser (f a') s'

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

optionalBlocks :: BlockParser ([Block], a) -> BlockParser ([Block], Maybe a)
optionalBlocks parser = parserConstructor innerLambda
  where
    innerLambda s = case runParser parser s of
      Right ((b, a), u) -> Right ((b, Just a), u)
      Left _ -> Right (([], Nothing), s)

--
-- Block parsers
--

parseEOLBlock :: BlockParser ([Block], ())
parseEOLBlock = parserConstructor $ \case
  [] -> pure (([], ()), [])
  (x : _) -> Left ("Expected empty input", Just x)

parseChecklistBlock :: BlockParser ([Block], [T.Text])
parseChecklistBlock = parserConstructor $ \case
  (BulletList blocks : xs) -> case evalParser parseChecklistItems (concat blocks) of
    Left _ -> Left ("Failed to parse BulletList", Just (BulletList blocks))
    Right items -> Right (([BulletList blocks], items), xs)
    where
      parseChecklistItems = some parseChecklistItem <* parseEOLBlock
      parseChecklistItem = parserConstructor $ \case
        (Plain item : ys) -> case evalParser parseSingleChecklistInline item of
          Right y -> Right (y, ys)
          Left (message, Just _) -> Left (message, Just (Plain item))
          Left (message, Nothing) -> Left (message, Nothing)
        (x : _) -> Left ("Expected Plain wrapping a checklist item", Just x)
        [] -> Left ("Expected non-empty blocks", Nothing)
  (x : _) -> Left ("Expected a BulletList", Just x)
  [] -> Left ("Expected a Block", Nothing)

parseNotesBlock :: BlockParser ([Block], T.Text)
parseNotesBlock = parserConstructor $ \case
  (Para inlines : xs) -> case evalParser parseNotesInline inlines of
    Left (message, _) -> Left (message, Just $ Para inlines)
    Right notes -> Right (([Para inlines], notes), xs)
  (x : _) -> Left ("Expected Para", Just x)
  [] -> Left ("Expected a Block", Nothing)

parsePlanningBlock :: BlockParser ([Block], (Maybe Scheduled, Maybe Deadline))
parsePlanningBlock = parserConstructor $ \case
  (Plain inlines : xs) -> case evalParser parsePlanningInline inlines of
    Left (message, _) -> Left (message, Just $ Plain inlines)
    Right planningInfo -> Right (([Plain inlines], planningInfo), xs)
  (x : _) -> Left ("Expected Plain", Just x)
  [] -> Left ("Expected a Block", Nothing)

parseTodoAndTagsBlock :: BlockParser ([Block], (T.Text, [T.Text]))
parseTodoAndTagsBlock = parserConstructor $ \case
  (Para inlines : xs) -> case evalParser parseTodoAndTagsInline inlines of
    Left (message, _) -> Left (message, Just $ Para inlines)
    Right todoAndTags -> Right (([Para inlines], todoAndTags), xs)
  (Header 1 attr inlines : _) -> Left ("Expected Header 2 or Header 3", Just $ Header 1 attr inlines)
  (Header level attr inlines : xs) -> case evalParser parseTodoAndTagsInline inlines of
    Left (message, _) -> Left (message, Just $ Header level attr inlines)
    Right todoAndTags -> Right (([Header level attr inlines], todoAndTags), xs)
  (x : _) -> Left ("Expected Para", Just x)
  [] -> Left ("Expected a Block", Nothing)

parseTitleAndTagsBlock :: BlockParser ([Block], (T.Text, [T.Text]))
parseTitleAndTagsBlock = parserConstructor $ \case
  (Header 2 attr inlines : xs) -> case evalParser parseTitleAndTagsInline inlines of
    Left (message, _) -> Left (message, Just $ Header 2 attr inlines)
    Right titleAndTags -> Right (([Header 2 attr inlines], titleAndTags), xs)
  (x : _) -> Left ("Expected Header 2", Just x)
  [] -> Left ("Expected a Block", Nothing)

parseTitleBlock :: BlockParser ([Block], T.Text)
parseTitleBlock = parserConstructor $ \case
  (Header 3 attr inlines : xs) -> case evalParser parseTitleInline inlines of
    Left (message, _) -> Left (message, Just $ Header 3 attr inlines)
    Right title -> Right (([Header 3 attr inlines], title), xs)
  (x : _) -> Left ("Expected Header 3", Just x)
  [] -> Left ("Expected a Block", Nothing)

parseSingleTodoWithProjectAndHeading :: Area -> Project -> Heading -> BlockParser [Block]
parseSingleTodoWithProjectAndHeading area project heading = do
  (titleBlocks, (title, tags)) <- parseTodoAndTagsBlock
  (planning_blocks, optional_planning) <- optionalBlocks parsePlanningBlock
  (notes_blocks, optional_notes) <- optionalBlocks parseNotesBlock
  (checklist_blocks, optional_checklist) <- optionalBlocks parseChecklistBlock
  _ <- parseEOLBlock
  let todo = constructTodo title tags optional_planning optional_notes optional_checklist (Just heading) (Just project) area
  return $ titleBlocks ++ planning_blocks ++ notes_blocks ++ checklist_blocks ++ linkFromTodo todo

parseTodoWithProjectAndHeading :: Area -> Project -> Heading -> BlockParser [Block]
parseTodoWithProjectAndHeading area project heading = parserConstructor $ \case
  (OrderedList attr blocks : xs) -> case sequence $ evalParser (parseSingleTodoWithProjectAndHeading area project heading) <$> blocks of
    Right transformedBlocks -> Right ([OrderedList attr transformedBlocks], xs)
    Left m -> Left m
  (x : _) -> Left ("Expected OrderedList", Just x)
  [] -> Left ("Expected a Block", Nothing)

parseTodoWithProject :: Area -> Project -> BlockParser [Block]
parseTodoWithProject area project = do
  (titleBlocks, (title, tags)) <- parseTodoAndTagsBlock
  (planning_blocks, optional_planning) <- optionalBlocks parsePlanningBlock
  (notes_blocks, optional_notes) <- optionalBlocks parseNotesBlock
  (checklist_blocks, optional_checklist) <- optionalBlocks parseChecklistBlock
  let todo = constructTodo title tags optional_planning optional_notes optional_checklist Nothing (Just project) area
  return $ titleBlocks ++ planning_blocks ++ notes_blocks ++ checklist_blocks ++ linkFromTodo todo

parseTodoInArea :: Area -> BlockParser [Block]
parseTodoInArea area = do
  (titleBlocks, (title, tags)) <- parseTodoAndTagsBlock
  (planning_blocks, optional_planning) <- optionalBlocks parsePlanningBlock
  (notes_blocks, optional_notes) <- optionalBlocks parseNotesBlock
  (checklist_blocks, optional_checklist) <- optionalBlocks parseChecklistBlock
  let todo = constructTodo title tags optional_planning optional_notes optional_checklist Nothing Nothing area
  return $ titleBlocks ++ planning_blocks ++ notes_blocks ++ checklist_blocks ++ linkFromTodo todo

parseHeadingMetadata :: Area -> Project -> BlockParser ([Block], Heading)
parseHeadingMetadata area project = do
  (metadataBlocks, title) <- parseTitleBlock
  return (metadataBlocks, Heading title project area)

parseHeading :: Area -> Project -> BlockParser [Block]
parseHeading area project = do
  (titleBlocks, heading) <- parseHeadingMetadata area project
  optionalNestedBlock <- optional $ parseTodoWithProjectAndHeading area project heading
  case optionalNestedBlock of
    Just nestedBlock -> return $ titleBlocks ++ nestedBlock
    Nothing -> return titleBlocks

parseProjectMetadata :: Area -> BlockParser ([Block], Project)
parseProjectMetadata area = do
  (titleBlocks, (title, tags)) <- parseTitleAndTagsBlock
  (planning_blocks, optional_planning) <- optionalBlocks parsePlanningBlock
  (notes_blocks, optional_notes) <- optionalBlocks parseNotesBlock
  let project = constructProject title tags optional_planning optional_notes area
  return (titleBlocks ++ planning_blocks ++ notes_blocks ++ linkFromProject project, project)

parseProject :: Area -> BlockParser [Block]
parseProject area = do
  (metadataBlock, project) <- parseProjectMetadata area
  nestedBlocks <- concat <$> many (parseHeading area project <|> parseTodoWithProject area project)
  return $ metadataBlock ++ nestedBlocks

parseAreaMetadata :: BlockParser ([Block], Area)
parseAreaMetadata = parserConstructor $ \case
  (Header 1 attr inlines : xs) -> case evalParser parseStrInline inlines of
    Left (message, _) -> Left (message, Just $ Header 1 attr inlines)
    Right areaTitle -> Right (([Header 1 attr inlines], Area areaTitle), xs)
  (x : _) -> Left ("Expected Header 1", Just x)
  [] -> Left ("Expected a Block", Nothing)

parseArea :: BlockParser [Block]
parseArea = do
  (metadataBlocks, area) <- parseAreaMetadata
  nestedBlocks <- concat <$> many (parseProject area <|> parseTodoInArea area)
  return $ metadataBlocks ++ nestedBlocks

parseRawBlock :: BlockParser [Block]
parseRawBlock = parserConstructor $ \case
  (RawBlock format text : xs) -> Right ([RawBlock format text], xs)
  (x : _) -> Left ("Expected RawBlock", Just x)
  [] -> Left ("Expected a Block", Nothing)

parseAgenda :: BlockParser [Block]
parseAgenda = do
  rawBlocks <- concat <$> many parseRawBlock
  dataBlocks <- concat <$> many parseArea
  _ <- parseEOLBlock
  return $ rawBlocks ++ dataBlocks

--
-- Inline parsers
--

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

parseTitleAndTagsInline :: InlineParser (T.Text, [T.Text])
parseTitleAndTagsInline = do
  title <- parseTitle
  tags <- extract <$> optional parseTags
  return (title, tags)
  where
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

parseTitleInline :: InlineParser T.Text
parseTitleInline = T.concat <$> greedyManyTill parseStr parseEOLInline
  where
    parseStr :: InlineParser T.Text
    parseStr = parserConstructor $ \case
      (Str inner_text : xs) -> pure (inner_text, xs)
      (Space : xs) -> pure (" ", xs)
      (x : _) -> Left ("Expected Str or Space", Just x)
      _ -> Left ("Expected input", Nothing)

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

parseSingleChecklistInline :: InlineParser T.Text
parseSingleChecklistInline = checkMark *> parseNotesInline
  where
    checkMark = parserConstructor $ \case
      (Str "\9744" : Space : xs) -> Right ((), xs)
      (Str "\9746" : Space : xs) -> Right ((), xs)
      (x : _) -> Left ("Expected to start with a checkmark", Just x)
      [] -> Left ("Expected a non-empty item in checklist", Nothing)

--
-- Char Parsers
--

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
