{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Util where

import Control.Concurrent.Async ( mapConcurrently )
import Data.Aeson               ( ToJSON(..) )
import Data.Aeson.Encode.Pretty ( encodePretty )
import Data.Int
import Data.ByteString.Lazy     ( toStrict )
import Data.Csv                 ( EncodeOptions(..)
                                , DefaultOrdered(..)
                                , ToField
                                , ToRecord(..)
                                , ToNamedRecord(..)
                                , (.=)
                                , defaultEncodeOptions
                                , encodeDefaultOrderedByNameWith
                                , namedRecord
                                , toField
                                )
import Data.Either              ( either )
import Data.Maybe               ( maybe )
import Data.Text                ( Text )
import Data.Text.Encoding       ( decodeUtf8
                                , encodeUtf8
                                )
import Data.Time.Clock          ( UTCTime )
import Data.Time.Clock.POSIX    ( posixSecondsToUTCTime )
import Data.Vector              ( fromList )
import GHC.Generics
import Network.DNS.Resolver     ( Resolver
                                , ResolvSeed
                                , defaultResolvConf
                                , makeResolvSeed
                                , withResolver
                                )
import Network.DNS.LookupRaw    ( lookupRawCtl )
import Network.DNS.Types        ( Domain
                                , DNSMessage(..)
                                , FlagOp(..)
                                , TYPE(..)
                                , RData(..)
                                , RD_RRSIG(..)
                                , ResourceRecord(..)
                                , adFlag
                                , doFlag
                                )

import qualified Data.Map as DM

data Detail = Detail { rrType     :: !TYPE
                     , inception  :: !Int64
                     , expiration :: !Int64
                     , alert      :: !Bool
            } deriving (Generic)

instance ToJSON   Detail

instance ToRecord Detail

{- Take advante of TYPE having a Show instance -}
instance ToJSON TYPE where
  toJSON = toJSON . show 

instance ToField TYPE where
  toField = toField . show

instance ToField Bool where
  toField True  = "yes"
  toField False = "no"

newtype Results = Results { getResults :: DM.Map Text [Detail] }
                deriving (Generic,Semigroup)

--instance Semigroup Results where
--  (Results m0) <> (Results m1) = Results $ m0 <> m1

instance Monoid Results where
  mempty  = Results DM.empty 

instance ToJSON Results

data ReportRow = ReportRow { d :: !Text
                           , f :: !TYPE
                           , i :: !Int64
                           , e :: !Int64
                           , a :: !Bool
               } deriving (Generic)

instance DefaultOrdered ReportRow where
  headerOrder rr = fromList [ "domain"
                            , "rrtype"
                            , "inception"
                            , "expiration"
                            , "alert"
                            ]

instance ToRecord ReportRow

instance ToNamedRecord ReportRow where
  toNamedRecord rr = namedRecord [ "domain"     .= d rr
                                 , "rrtype"     .= f rr
                                 , "inception"  .= i rr
                                 , "expiration" .= e rr
                                 , "alert"      .= a rr
                                 ]

resultJSON :: Results -> Text
resultJSON = decodeUtf8 . toStrict . encodePretty

resultCSV :: Results -> Text
resultCSV = decodeUtf8 
          . toStrict 
          . encodeDefaultOrderedByNameWith reportOptions
          . resultToReport
  where
    resultToReport :: Results -> [ReportRow]
    resultToReport (Results rm) =
      concatMap (\(n,rs) -> map (\r -> ReportRow { d = n
                                                 , f = rrType r
                                                 , i = inception r
                                                 , e = expiration r
                                                 , a = alert r}) rs) $
        DM.toList rm

    reportOptions = defaultEncodeOptions {
                      encIncludeHeader = True
                    }

checkSignatures :: UTCTime -> [Text] -> IO Results
checkSignatures threshold domains = do
  rs <- makeResolvSeed defaultResolvConf
  mconcat <$> mapConcurrently (checkDomain rs) domains

    where
      checkDomain :: ResolvSeed
                  -> Text
                  -> IO Results
      checkDomain seed domain = withResolver seed $ \res ->
        Results . maybe 
          DM.empty 
          (\details -> DM.insert domain details DM.empty)
          <$> findDetails res (encodeUtf8 domain)
  
      findDetails :: Resolver -> Domain -> IO (Maybe [Detail])
      findDetails res name =
        either 
          (const Nothing)
          (Just . detailsFor . answer)
          <$> lookupRawCtl res name RRSIG (mconcat [ adFlag FlagSet
                                                   , doFlag FlagSet
                                                   ])
      
      detailsFor :: [ResourceRecord] -> [Detail]
      detailsFor = map (computeDetails . rdata) .
                        filter (\r -> rrtype r == RRSIG)

      computeDetails :: RData -> Detail
      computeDetails (RD_RRSIG rr) = Detail { 
         rrType     = rrsigType rr
       , inception  = rrsigInception rr
       , expiration = rrsigExpiration rr
       , alert      = posixSecondsToUTCTime (fromIntegral $ rrsigExpiration rr) < threshold
       }

foo :: IO ()
foo = putStrLn "hello"


