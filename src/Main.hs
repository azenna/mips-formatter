{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Applicative ((<|>))
import Control.Monad (void)
import Data.Functor (($>), (<&>))
import Data.Foldable (traverse_)
import Data.Char (isAlphaNum, isAlpha, isNumber, isSpace)
import Data.Either (fromRight)
import Data.Maybe (fromMaybe, catMaybes, maybe)
import Data.List (intercalate)
import qualified Text.Megaparsec as M
import qualified Text.Megaparsec.Char as M
import qualified Text.Megaparsec.Error.Builder as M
import Text.Read (readMaybe)

type MipsParser = M.Parsec String String

notSpace :: MipsParser String
notSpace = M.takeWhileP Nothing (not . isSpace)

ident :: MipsParser String
ident = M.takeWhileP Nothing isAlphaNum

word :: MipsParser String
word = M.some M.letterChar

next :: MipsParser a -> MipsParser a
next = (M.hspace >>)

type Comment = String

parseComment :: MipsParser Comment
parseComment = M.char '#' >> next (intercalate " " <$> M.sepBy notSpace M.hspace1)

writeComment :: Comment -> String
writeComment = ("# " <>)

type Reg = String

parseReg :: MipsParser Reg
parseReg = M.char '$' >> ident

writeReg :: Reg -> String
writeReg = ('$' :)

type Op = String

type Label = String

parseLabel :: MipsParser Label
parseLabel = ident <* M.char ':'

writeLabel :: Label -> String
writeLabel = (<> ":")

type Directive = String

parseDirective :: MipsParser Directive
parseDirective = M.char '.' >> word

writeDirective :: Directive -> String
writeDirective =  ('.' : )

type MipsInt = Int

parseMipsInt :: MipsParser MipsInt
parseMipsInt = do
  text <- M.some (M.digitChar <|> M.char '-')
  case readMaybe text of
    Just x -> pure x
    Nothing -> fail text

writeMipsInt :: MipsInt -> String
writeMipsInt = show

type MipsString = String

parseMipsString :: MipsParser MipsString
parseMipsString = M.between (M.char '"') (M.char '"') (M.takeWhileP Nothing (/='"'))

writeMipsString :: MipsString -> String
writeMipsString ms = "\"" <> ms <> "\""

type MipsArray = (MipsInt, MipsInt)

parseMipsArray :: MipsParser MipsArray
parseMipsArray = do
  x <- parseMipsInt
  void . next $ M.char ':'
  y <- next parseMipsInt
  pure (x, y)

writeMipsArray :: MipsArray -> String
writeMipsArray (x, y) = show x <> " : " <> show y

data DataValue =
    DVString MipsString
  | DVInt MipsInt
  | DVArray MipsArray deriving (Eq, Show)

parseDataValue :: MipsParser DataValue
parseDataValue = M.choice
  [ DVString <$> parseMipsString
  , M.try (DVArray <$> parseMipsArray)
  , DVInt <$> parseMipsInt
  ]

writeDataValue :: DataValue -> String
writeDataValue sd = case sd of
  DVString ms -> writeMipsString ms
  DVInt mi -> writeMipsInt mi
  DVArray marr ->  writeMipsArray marr

data Data = Data
  { label :: String
  , mDirective :: Maybe String
  , value :: DataValue
  } deriving (Eq, Show)

parseData :: MipsParser Data
parseData = do
  label <- parseLabel
  mDirective <- M.optional $ next parseDirective
  value <- next parseDataValue
  pure $ Data
    { label
    , mDirective
    , value
    }

writeData :: Data -> String
writeData sdata =  intercalate " "
  [ writeLabel (label sdata)
  , fromMaybe "" (writeDirective <$> mDirective sdata)
  , writeDataValue (value sdata)
  ]

data OpArg =
    OAReg Reg
  | OAInt MipsInt
  | OAOffset MipsInt Reg
  | OAIdent String
  deriving (Eq, Show)

parseOpArg :: MipsParser OpArg
parseOpArg = M.choice
  [ OAReg <$> parseReg
  , M.try parseOAOffset
  , OAInt <$> parseMipsInt
  , OAIdent <$> ident
  ]
  where
    parseOAOffset = do
      m <- parseMipsInt
      reg <- M.between (M.char '(') (M.char ')') parseReg
      pure $ OAOffset m reg

writeOpArg :: OpArg -> String
writeOpArg = \case
  OAReg r -> writeReg r
  OAInt i -> writeMipsInt i
  OAOffset off r -> writeMipsInt off <> "(" <> writeReg r <> ")"
  OAIdent i -> i

data MipsText = MipsText
  { mLabel :: Maybe Label
  , op :: Op
  , args :: [OpArg]
  } deriving (Eq, Show)

parseMipsText :: MipsParser MipsText
parseMipsText = do
  mLabel <- M.optional $ M.try parseLabel
  op <- next word
  args <- M.sepBy1 (next parseOpArg) (M.char ',')
  pure $ MipsText
    { mLabel
    , op
    , args
    }

writeMipsText :: MipsText -> String
writeMipsText mt = intercalate " "
  [ maybe "" writeLabel (mLabel mt)
  , op mt
  , intercalate ", " . fmap writeOpArg $ args mt
  ]

data LineContent =
    LCDirective Directive (Maybe DataValue)
  | LCData Data
  | LCText MipsText
  deriving (Eq, Show)

parseLineContent :: MipsParser LineContent
parseLineContent = M.choice
  [ M.try (LCData <$> parseData)
  , M.try (LCText <$> parseMipsText)
  , LCDirective <$> parseDirective <*> M.optional (next parseDataValue)
  ]

writeLineContent :: LineContent -> String
writeLineContent = \case
  LCDirective d mdv -> writeDirective d <> maybe "" writeDataValue mdv
  LCData d -> writeData d
  LCText t -> writeMipsText t

data Line = Line
  { mContent :: Maybe LineContent
  , mComment :: Maybe Comment
  } deriving (Eq, Show)

parseLine :: MipsParser Line
parseLine = do
  mContent <- M.optional (next parseLineContent)
  mComment <- M.optional (next parseComment)
  pure $ Line
    { mContent
    , mComment }

writeLine :: Line -> String
writeLine line = intercalate " " $ catMaybes
  [ writeLineContent <$> mContent line
  , writeComment <$> mComment line
  ]

type Ast = [Line]

parseAst :: MipsParser Ast
parseAst = do
  line <- parseLine
  mRest <- M.optional (next M.newline >> parseAst)
  case mRest of
    Just rest -> pure (line : rest)
    Nothing -> pure [line]

writeAst :: Ast -> String
writeAst = intercalate "\n" . fmap writeLine

main :: IO ()
main = do
  cont <- readFile "fib.asm"
  writeFile "out.asm" $ fromRight "" (writeAst <$> M.runParser parseAst "" cont)
