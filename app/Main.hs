{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Time.Clock          ( addUTCTime
                                , getCurrentTime
                                , nominalDay
                                )
import Data.Text                ( Text )
import Data.Word
import Options.Applicative
import Util
import System.Environment       ( getProgName )

import qualified Data.Text.IO   as TIO

main :: IO ()
main = do
  pn <- getProgName
  execParser (opts pn) >>= run
  where
    opts name = info ( commandLineOptions <**> helper )
                     ( fullDesc
                     <> progDesc "Check DNSSEC signatures for domains"
                     <> header (name ++ " -- check DNSSEC signatures")
                     )

run :: Options -> IO ()
run options = do
  thr <- addUTCTime (fromIntegral (daysToExpire options) * nominalDay)
                   <$> getCurrentTime
  dm  <- checkSignatures thr
                         (domainsToCheck options)
  TIO.putStrLn $ if wantJSON options
                    then resultJSON dm
                    else resultCSV dm

data Options = Options { wantJSON       :: !Bool
                       , daysToExpire   :: !Word32
                       , domainsToCheck :: [Text]
                       }
                       deriving (Show)

commandLineOptions :: Parser Options
commandLineOptions = Options <$> switch
                                 (  long "json"
                                 <> short 'j'
                                 <> help "Output as JSON object"
                                 )
                             <*> option auto
                                 (  long "days"
                                 <> short 'd'
                                 <> help "Days to expiration"
                                 <> showDefault
                                 <> value 1
                                 <> metavar "INT"
                                 )
                             <*> some (argument str
                                 (  metavar "DOMAIN1 DOMAIN2 ..."
                                 <> help "One or more domains to check"
                                 )
                                 )

