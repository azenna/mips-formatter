{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Main where

import Control.Applicative ((<|>))
import Control.Monad (void)
import Data.Function (on)
import Data.Char (isAlphaNum, isSpace)
import Data.Maybe (catMaybes)
import Data.List (intercalate)
import qualified Text.Megaparsec as M
import qualified Text.Megaparsec.Char as M
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
parseComment = M.char '#' >> next (unwords <$> M.sepBy notSpace M.hspace1)

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

dataToIndentRules :: Data -> IndentRules
dataToIndentRules dat = IndentRules
  { labelLength = length . writeLabel . label $ dat
  , instructionLength = maybe 0 (length . writeDirective) (mDirective dat)
  , argumentLength = length . writeDataValue . value $ dat
  }

writeData :: IndentRules -> Data -> String
writeData ir sdata =  unwords
  [ processRule labelLength ir . writeLabel $ label sdata
  , processRule instructionLength ir . maybe "" writeDirective $ mDirective sdata
  , processRule argumentLength ir . writeDataValue $ value sdata
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

writeOpArgs :: [OpArg] -> String
writeOpArgs =  intercalate ", " . map writeOpArg

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

mipsTextToIndentRules :: MipsText -> IndentRules
mipsTextToIndentRules mt = IndentRules
  { labelLength = maybe 0 (length . writeLabel) (mLabel mt)
  , instructionLength = length $ op mt
  , argumentLength = length . writeOpArgs . args $ mt
  }

writeMipsText :: IndentRules -> MipsText -> String
writeMipsText ir mt = unwords
  [ processRule labelLength ir $ maybe "" writeLabel (mLabel mt)
  , processRule instructionLength ir $ op mt
  , processRule argumentLength ir . writeOpArgs $ args mt
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

writeLineContent :: IndentRules -> LineContent -> String
writeLineContent ir = \case
  LCDirective d mdv -> unwords
    [ processRule labelLength ir ""
    , processRule instructionLength ir $ writeDirective d
    , processRule argumentLength ir $ maybe "" writeDataValue mdv
    ]
  LCData d -> writeData ir d
  LCText t -> writeMipsText ir t

lineContentToIndentRules :: LineContent -> IndentRules
lineContentToIndentRules = \case
  LCDirective d mdv -> IndentRules
    { labelLength = 0
    , instructionLength = length $ writeDirective d
    , argumentLength = maybe 0 (length . writeDataValue) mdv }
  LCData d -> dataToIndentRules d
  LCText t -> mipsTextToIndentRules t

data Line = Line
  { mContent :: Maybe LineContent
  , mComment :: Maybe Comment
  } deriving (Eq, Show)

lineToIndentRules :: Line -> IndentRules
lineToIndentRules = maybe defaultIndentRules lineContentToIndentRules . mContent

parseLine :: MipsParser Line
parseLine = do
  mContent <- M.optional (next parseLineContent)
  mComment <- M.optional (next parseComment)
  pure $ Line
    { mContent
    , mComment }

writeLine :: IndentRules -> Line -> String
writeLine ir line = unwords $ catMaybes
  [ writeLineContent ir <$> mContent line
  , writeComment <$> mComment line
  ]

type Ast = [Line]

parseAst :: MipsParser Ast
parseAst =  M.sepBy parseLine (next M.newline)

writeAst :: IndentRules -> Ast -> String
writeAst ir = intercalate "\n" . fmap (writeLine ir)

data IndentRules = IndentRules
  { labelLength :: Int
  , instructionLength :: Int
  , argumentLength :: Int
  } deriving (Eq, Show)


processRule :: (IndentRules -> Int) -> IndentRules -> String -> String
processRule sel ir s = s <> replicate (sel ir - length s) ' '

defaultIndentRules :: IndentRules
defaultIndentRules = IndentRules
  { labelLength = 0
  , instructionLength = 0
  , argumentLength = 0
  }

zipIndentRules :: IndentRules -> IndentRules -> IndentRules
zipIndentRules ir1 ir2 = IndentRules
  { labelLength = zipir labelLength
  , instructionLength = zipir instructionLength
  , argumentLength = zipir argumentLength
  }
  where zipir sel = on max sel ir1 ir2

main :: IO ()
main = do
  cont <- readFile "fib.asm"
  case M.runParser parseAst "" cont of
    Left err -> error "didn't work"
    Right res ->
      writeFile "out.asm" $ writeAst (foldr (zipIndentRules . lineToIndentRules) defaultIndentRules res) res
