# changelog-diff
Generates changelogs of functions between Haskell packages.
## Usage
`./changelog-diff-exe packageOldVersion.hoo packageNewVersion.hoo`

## Example output
`./changelog-diff-exe cassava.0.1.0.1.hoo cassava.0.4.4.0.hoo`

outputs:

```
Data.Csv
    + QuoteAll :: Quoting
    + QuoteMinimal :: Quoting
    + QuoteNone :: Quoting
    + encIncludeHeader :: EncodeOptions -> !Bool
    + encUseCrLf :: EncodeOptions -> !Bool
    + encodeDefaultOrderedByName :: [a] -> ByteString
    + encQuoting :: EncodeOptions -> !Quoting
    + encodeDefaultOrderedByNameWith :: EncodeOptions -> [a] -> ByteString
    + headerOrder :: a -> Header
    + header :: [ByteString] -> Header
    + index :: Record -> Int -> Parser a
    + unsafeIndex :: Record -> Int -> Parser a
    + lookup :: NamedRecord -> ByteString -> Parser a
    
    - EncodeOptions :: {-# UNPACK #-} !Word8 -> EncodeOptions
    
    ~ encode :: [a] -> ByteString
                Vector a -> ByteString
    ~ encodeWith :: EncodeOptions -> [a] -> ByteString
                    EncodeOptions -> Vector a -> ByteString
    ~ encodeByName :: Header -> [a] -> ByteString
                      Header -> Vector a -> ByteString



Data.Csv.Builder
    + encodeRecord :: a -> Builder
    + encodeDefaultOrderedNamedRecord :: a -> Builder
    + encodeHeader :: Header -> Builder
    + encodeRecordWith :: EncodeOptions -> a -> Builder
    + encodeDefaultOrderedNamedRecordWith :: EncodeOptions -> a -> Builder
    + encodeHeaderWith :: EncodeOptions -> Header -> Builder
    + encodeNamedRecord :: Header -> a -> Builder



Data.Csv.Incremental
    + HasHeader :: HasHeader
    + NoHeader :: HasHeader
    + PartialH :: (ByteString -> HeaderParser a) -> HeaderParser a
    + Done :: [Either String a] -> Parser a
    + encodeDefaultOrderedByName :: NamedBuilder a -> ByteString
    + encode :: Builder a -> ByteString
    + decodeHeader :: HeaderParser ByteString
    + FailH :: !ByteString -> String -> HeaderParser a
    + Fail :: !ByteString -> String -> Parser a
    + Many :: [Either String a] -> (ByteString -> Parser a) -> Parser a
    + encodeRecord :: a -> Builder a
    + DoneH :: !Header -> a -> HeaderParser a
    + encodeNamedRecord :: a -> NamedBuilder a
    + decode :: HasHeader -> Parser a
    + encodeDefaultOrderedByNameWith :: EncodeOptions -> NamedBuilder a -> ByteString
    + encodeWith :: EncodeOptions -> Builder a -> ByteString
    + encodeByName :: Header -> NamedBuilder a -> ByteString
    + decodeHeaderWith :: DecodeOptions -> HeaderParser ByteString
    + decodeWith :: DecodeOptions -> HasHeader -> Parser a



Data.Csv.Parser
    ~ name :: Word8 -> Parser Name
              Word8 -> Parser Field



Data.Csv.Streaming
    + Nil :: (Maybe String) -> ByteString -> Records a
    + Cons :: (Either String a) -> (Records a) -> Records a
    + decode :: HasHeader -> ByteString -> Records a


```
