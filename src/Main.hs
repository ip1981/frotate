module Main
  ( main
  ) where

import System.IO (hPutStrLn, stderr)

import Data.Time.Calendar (Day)
import Options.Applicative
  ( Parser
  , (<**>)
  , (<|>)
  , argument
  , auto
  , execParser
  , flag'
  , fullDesc
  , help
  , helper
  , info
  , long
  , metavar
  , option
  , optional
  , short
  , showDefault
  , some
  , value
  )

import Lib (partitionDays)

data Mode
  = Keep
  | Delete

data Options = Options
  { mode :: Maybe Mode
  , base :: Double
  , days :: [Day]
  }

parseMode :: Parser Mode
parseMode = keep <|> delete
  where
    keep = flag' Keep (long "keep" <> short 'k' <> help "Print days to keep")
    delete =
      flag' Delete (long "delete" <> short 'd' <> help "Print days to delete")

parseOptions :: Parser Options
parseOptions =
  Options <$> optional parseMode <*>
  option
    auto
    (long "base" <> short 'b' <> metavar "BASE" <> showDefault <> value 1.1 <>
     help "Base of the exponent") <*>
  some (argument auto (metavar "DAY..."))

main :: IO ()
main = do
  opts <- execParser $ info (parseOptions <**> helper) fullDesc
  let groups = partitionDays (\n -> floor (base opts ^ (n - 1))) (days opts)
  case mode opts of
    Just Keep ->
      putStrLn $ unwords . map show . concatMap (take 1 . reverse) $ groups
    Just Delete ->
      putStrLn $ unwords . map show . concatMap (drop 1 . reverse) $ groups
    Nothing -> mapM_ (hPutStrLn stderr . unwords . map show . reverse) groups
