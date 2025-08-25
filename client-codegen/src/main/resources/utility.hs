import Control.Monad ((>=>))
import Data.Aeson
import qualified Data.Aeson.KeyMap as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.Bifunctor
import qualified Data.Bifunctor as Bifunctor
import Data.ByteString (ByteString, StrictByteString, toStrict)
import qualified Data.ByteString as BS
import Data.ByteString.Char8 as Char8 (unpack)
import qualified Data.CaseInsensitive as CI
import Data.Foldable (traverse_)
import Data.Function
import Data.Int (Int16, Int64, Int8)
import qualified Data.Map as M
import Data.Maybe
import Data.String (fromString)
import Data.Text (Text, pack, toLower, unpack)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8, decodeUtf8', encodeUtf8)
import Data.Time (defaultTimeLocale, formatTime, parseTimeM)
import Data.Time.Clock (UTCTime)
import Data.Time.Clock.POSIX (POSIXTime)
import Data.Time.Format.ISO8601 (iso8601ParseM)
import GHC.Generics (Generic)
import GHC.Int (Int32)
import Network.HTTP.Client (BodyReader, Response)
import qualified Network.HTTP.Client as HTTP
import Network.HTTP.Date (HTTPDate, formatHTTPDate, parseHTTPDate)
import qualified Network.HTTP.Types as HTTP
import qualified Network.URI as Network
import Text.Read (readEither)
import Data.Int (Int32)

instance ToJSON HTTPDate where
  toJSON = Data.Aeson.String . decodeUtf8 . formatHTTPDate

instance FromJSON HTTPDate where
  parseJSON = withText "HTTPDate" $ \t ->
    parseHTTPDate (encodeUtf8 t)
      & maybe (fail "Failed to parse HTTP date") pure

class RequestSegment a where
  toRequestSegment :: (Show a) => a -> Text

class ResponseSegment a where
  fromResponseSegment :: ByteString -> Either Text a

instance RequestSegment Int64 where
  toRequestSegment = toLower . pack . show

instance ResponseSegment Int64 where
  fromResponseSegment = mapLeft pack . eitherDecodeStrict'

instance RequestSegment Int32 where
  toRequestSegment = toLower . pack . show

instance ResponseSegment Int32 where
  fromResponseSegment = mapLeft pack . eitherDecodeStrict'

instance RequestSegment Int16 where
  toRequestSegment = toLower . pack . show

instance ResponseSegment Int16 where
  fromResponseSegment = mapLeft pack . eitherDecodeStrict'

instance RequestSegment Int8 where
  toRequestSegment = toLower . pack . show

instance ResponseSegment Int8 where
  fromResponseSegment = mapLeft pack . eitherDecodeStrict'

instance RequestSegment Integer where
  toRequestSegment = toLower . pack . show

instance ResponseSegment Integer where
  fromResponseSegment = mapLeft pack . eitherDecodeStrict'

instance RequestSegment Double where
  toRequestSegment = toLower . pack . show

instance ResponseSegment Double where
  fromResponseSegment = mapLeft pack . eitherDecodeStrict'

instance RequestSegment Float where
  toRequestSegment = toLower . pack . show

instance ResponseSegment Float where
  fromResponseSegment = mapLeft pack . eitherDecodeStrict'

instance RequestSegment Text where
  toRequestSegment = id

instance ResponseSegment Text where
  fromResponseSegment = mapLeft (pack . show) . decodeUtf8'

instance RequestSegment Bool where
  toRequestSegment = toLower . pack . show

instance ResponseSegment Bool where
  fromResponseSegment = mapLeft pack . eitherDecodeStrict'

instance RequestSegment HTTPDate where
  toRequestSegment = decodeUtf8 . formatHTTPDate

instance ResponseSegment HTTPDate where
  fromResponseSegment = maybe (Left "Failed to parse HTTPDate") Right . parseHTTPDate

instance RequestSegment UTCTime where
  toRequestSegment utc = pack $ (formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S%Q" utc) ++ "+00:00"

instance ResponseSegment UTCTime where
  fromResponseSegment = mapLeft pack . readUTCTime . Char8.unpack
    where
      readUTCTime :: String -> Either String UTCTime
      readUTCTime ts = case parseTimeM True defaultTimeLocale "%Y-%m-%dT%H:%M:%S%Q%z" ts of
        Just ts -> Right ts
        _ -> Left "Failed to parse as UTCTime."

instance RequestSegment POSIXTime where
  toRequestSegment = pack . show

instance ResponseSegment POSIXTime where
  fromResponseSegment = mapLeft pack . readEither . Char8.unpack

mapLeft :: (a -> b) -> Either a c -> Either b c
mapLeft f = either (Left . f) Right


-- | Generalized state monad, to avoid pulling `in mtl` or `transformers`.
-- - Though, mtl ships as part of GHC, so maybe it's not a big deal.
newtype StateM s a = StateM {runStateM :: s -> (a, s)}

instance Functor (StateM s) where
  fmap f2 (StateM f1) = StateM $ \s ->
    let (x, s') = f1 s in (f2 x, s')

instance Applicative (StateM s) where
  pure x = StateM (x,)
  StateM f1 <*> StateM f2 = StateM $ \s ->
    let (f, s') = f1 s
        (x, s'') = f2 s'
     in (f x, s'')

instance Monad (StateM s) where
  StateM f1 >>= f2 =
    StateM $ \s ->
      let (a, s') = f1 s
          StateM f3 = f2 a
       in f3 s'

setSt :: (s -> s) -> StateM s ()
setSt setter = StateM $ \s -> ((), setter s)

getSt :: (s -> a) -> StateM s a
getSt getter = StateM $ \s -> (getter s, s)

-- | Type class for things used in headers, query-params, url params or even to
-- serialize the payload.
class SerDe t where
  serializeElement :: t -> StrictByteString
  deSerializeElement :: StrictByteString -> Either String t

  default serializeElement :: (ToJSON t) => t -> StrictByteString
  serializeElement = toStrict . encode
  default deSerializeElement :: (FromJSON t) => StrictByteString -> Either String t
  deSerializeElement = eitherDecodeStrict

-- | Smithy Byte
instance SerDe Int8

-- | Smithy Short
instance SerDe Int16

-- | Smithy Integer
instance SerDe Int32

-- | Smithy Long
instance SerDe Int64

-- | Smithy Float
instance SerDe Float

-- | Smithy Double
instance SerDe Double

-- | Smithy Boolean
instance SerDe Bool

-- | Smithy BigInteger
instance SerDe Integer

-- | Smithy Document
instance SerDe Aeson.Value

-- | Smithy Blob
instance SerDe StrictByteString where
  serializeElement = id
  deSerializeElement = Right

-- | All string & string-like types require special handling.
-- We need to ensure that quotes are trimmed when serializing and added when
-- attempting to deserialize, as per smithy's contracts:
-- Query: https://smithy.io/2.0/spec/http-bindings.html#id12
-- Path: https://smithy.io/2.0/spec/http-bindings.html#id8
-- Header: https://smithy.io/2.0/spec/http-bindings.html#serialization-rules
-- TODO Handle ENUMs!
instance SerDe Text where
  serializeElement = encodeUtf8
  deSerializeElement = Right . decodeUtf8

encodeTrimQuotes :: (ToJSON t) => t -> StrictByteString
encodeTrimQuotes = BS.init . BS.tail . toStrict . encode

quoteAndDecode :: (FromJSON t) => StrictByteString -> Either String t
quoteAndDecode bs = eitherDecodeStrict ("\"" <> bs <> "\"")

instance SerDe UTCTime where
  serializeElement = encodeTrimQuotes
  deSerializeElement = quoteAndDecode

instance SerDe HTTPDate where
  serializeElement = encodeTrimQuotes
  deSerializeElement = quoteAndDecode

instance SerDe POSIXTime where
  serializeElement = encodeTrimQuotes
  deSerializeElement = quoteAndDecode

type Path = [StrictByteString]

type ContentType = String

data RequestBody
  = NoBody
  | Opaque ContentType StrictByteString
  | Json Aeson.Object

data RequestBuilderSt = RequestBuilderSt
  { _headers :: [HTTP.Header],
    _method :: HTTP.Method,
    _body :: RequestBody,
    _query :: HTTP.SimpleQuery,
    _path :: Path
  }

type RequestBuilder = StateM RequestBuilderSt

newRequestBuilderSt :: RequestBuilderSt
newRequestBuilderSt =
  RequestBuilderSt
    { _headers = mempty,
      _method = HTTP.methodGet,
      _body = NoBody,
      _query = mempty,
      _path = mempty
    }

mergeWithHTTPRequest :: RequestBuilderSt -> HTTP.Request -> HTTP.Request
mergeWithHTTPRequest st req =
  req
    { HTTP.method = _method st,
      HTTP.requestHeaders = _headers st,
      HTTP.queryString = HTTP.renderSimpleQuery True (_query st),
      HTTP.path = HTTP.urlEncode False (BS.intercalate "/" (_path st)),
      HTTP.requestBody = case _body st of
        NoBody -> HTTP.requestBody req
        Opaque ct bs -> HTTP.RequestBodyLBS (BS.fromStrict bs)
        Json obj -> HTTP.RequestBodyLBS (encode obj)
    }

data ParamLocation = Query | Header

-- | This type class handles mutli-value parameters in headers & query params.
-- - The idea is that if the type is a list, we add multiple entries, otherwise
-- - we add a single entry. See the rules for serializing lists in each location:
-- - Headers: https://smithy.io/2.0/spec/http-bindings.html#serialization-rules
-- - Query: https://smithy.io/2.0/spec/http-bindings.html#id12
class SerializeParameter p where
  serParameter :: ParamLocation -> String -> p -> RequestBuilder ()

instance {-# OVERLAPPABLE #-} (SerDe p) => SerializeParameter p where
  serParameter location name value =
    let v = serializeElement value
        f = case location of
          Query -> \s -> s {_query = (fromString name, v) : _query s}
          Header -> \s -> s {_headers = (fromString name, v) : _headers s}
     in setSt f

instance (SerDe p, SerializeParameter p) => SerializeParameter [p] where
  -- Repeat for each query-parameter.
  serParameter Query name = traverse_ (serParameter Query name)
  -- Intercalate w/ commas & set it.
  serParameter Header name =
    serParameter Header name
      . BS.intercalate ","
      . map serializeElement

instance (SerializeParameter p) => SerializeParameter (Maybe p) where
  -- Only add it if it's non-null.
  serParameter location name = traverse_ (serParameter location name)

data ParameterMapT
  = QueryMap
  | HeaderMap String -- Prefix

class SerializeParameterMap p where
  serParameterMap :: ParameterMapT -> p -> RequestBuilder ()

instance (SerializeParameter p) => SerializeParameterMap (M.Map Text p) where
  serParameterMap QueryMap = traverse_ (\(k, v) -> serQuery (T.unpack k) v) . M.toList
  serParameterMap (HeaderMap prefix) =
    traverse_ (\(k, v) -> serParameter Header (T.unpack $ fromString prefix <> k) v) . M.toList

instance (SerializeParameter p) => SerializeParameterMap (Maybe (M.Map Text p)) where
  serParameterMap pm = traverse_ (serParameterMap pm)

serHeader :: (SerializeParameter t) => String -> t -> RequestBuilder ()
serHeader = serParameter Header

serHeaderMap :: (SerializeParameterMap t) => String -> t -> RequestBuilder ()
serHeaderMap = serParameterMap . HeaderMap

serQuery :: (SerializeParameter t) => String -> t -> RequestBuilder ()
serQuery = serParameter Query

serQueryMap :: (SerializeParameterMap t) => t -> RequestBuilder ()
serQueryMap = serParameterMap QueryMap

setMethod :: HTTP.Method -> RequestBuilder ()
setMethod method = setSt (\s -> s {_method = method})

class SerializeBody t where
  serBody :: String -> t -> RequestBuilder ()

  -- | For `structures` we can default to JSON serialization.
  default serBody :: (ToJSON t) => String -> t -> RequestBuilder ()
  serBody contentType body =
    setSt (\s -> s {_body = Opaque contentType (toStrict $ encode body)})

-- | Setting the body is curious, it's possible that the model binds the body
-- - to a string or some other type that is not a `structure`, and has different
-- - serialization rules: https://smithy.io/2.0/spec/http-bindings.html#id10
-- - This is why using `serializeElement` is the right choice here, it is compliant
-- - w/ this contract.
instance {-# OVERLAPPABLE #-} (SerDe t) => SerializeBody t where
  serBody contentType body =
    setSt (\s -> s {_body = Opaque contentType (serializeElement body)})

instance (SerializeBody t) => SerializeBody (Maybe t) where
  serBody contentType = traverse_ (serBody contentType)

-- | Serializing fields should direclty use `Aeson`.
serField :: (ToJSON t) => Aeson.Key -> t -> RequestBuilder ()
serField k v = do
  body <- getSt _body
  let obj = case body of
        Json obj -> obj
        _ -> mempty
  setSt (\s -> s {_body = Json $ Aeson.insert k (Aeson.toJSON v) obj})

setPath :: Path -> RequestBuilder ()
setPath path = setSt (\s -> s {_path = path})

type HttpResponse = Response BodyReader

-- | Lazly parsed body, to avoid re-parsing into an object if we need
-- - to parse out multiple fields from it.
-- - REVIEW Do we need to handle a NoBody case?
-- - Probably not, as that will just lead to more boilerplate, questionable case-matches.
data Body = Raw StrictByteString | Obj Aeson.Object

-- | Prases a type from an HTTP response.
newtype HttpResponseParser a
  = HttpResponseParser
  { runParser :: (HttpResponse, Body) -> (Either String a, Body)
  }

instance Functor HttpResponseParser where
  fmap f (HttpResponseParser p) = HttpResponseParser $ \r ->
    let (ea, b) = p r in (fmap f ea, b)

instance Applicative HttpResponseParser where
  pure a = HttpResponseParser $ \r -> (Right a, snd r)
  HttpResponseParser pf <*> HttpResponseParser pa = HttpResponseParser $ \r ->
    let (ef, b1) = pf r
        (ea, b2) = pa (fst r, b1)
     in (ef <*> ea, b2)

instance Monad HttpResponseParser where
  HttpResponseParser p >>= f = HttpResponseParser $ \r ->
    let (ea, b1) = p r
     in case ea of
          Right a -> runParser (f a) (fst r, b1)
          Left e -> (Left e, b1)

class DeSerializeHeader t where
  deSerHeader :: String -> HttpResponseParser t

instance {-# OVERLAPPABLE #-} (SerDe t) => DeSerializeHeader t where
  deSerHeader name = HttpResponseParser $ \(r, b) ->
    let headers = HTTP.responseHeaders r
        mHeader = lookup (fromString name) headers
     in case mHeader of
          Just v -> (deSerializeElement v, b)
          Nothing -> (Left $ "Header not found: " ++ name, b)

instance (SerDe t) => DeSerializeHeader [t] where
  deSerHeader name = HttpResponseParser $ \(r, b) ->
    let (bs :: Either String StrictByteString, _) = runParser (deSerHeader name) (r, b)
     in case bs of
          -- Split on commas (44 is the ASCII code for comma) & then de-serialize each value.
          Right v -> (traverse deSerializeElement (BS.split 44 v), b)
          Left e -> (Left e, b)

instance (DeSerializeHeader t) => DeSerializeHeader (Maybe t) where
  deSerHeader name = HttpResponseParser $ \(r, b) ->
    case runParser (deSerHeader name) (r, b) of
      (Right v, _) -> (Right (Just v), b)
      _ -> (Right Nothing, b)

class DeSerializeHeaderMap t where
  deSerHeaderMap :: String -> HttpResponseParser t

instance (DeSerializeHeader t) => DeSerializeHeaderMap (M.Map Text t) where
  deSerHeaderMap prefix = do
    headers <- HTTP.responseHeaders <$> getResponse
    let prefix' = T.toLower $ fromString prefix
        relevantHeaders =
          filter (T.isPrefixOf prefix') $
            map (T.toLower . decodeUtf8 . CI.original . fst) headers
    M.fromList <$> traverse (\h -> (h,) <$> deSerHeader (T.unpack h)) relevantHeaders

instance (DeSerializeHeader t) => DeSerializeHeaderMap (Maybe (M.Map Text t)) where
  deSerHeaderMap prefix = HttpResponseParser $ \(r, b) ->
    case runParser (deSerHeaderMap prefix) (r, b) of
      (Right v, _) -> (Right (Just v), b)
      _ -> (Right Nothing, b)

getResponse :: HttpResponseParser HttpResponse
getResponse = HttpResponseParser $ Bifunctor.first Right

embed :: Either String a -> HttpResponseParser a
embed ea = HttpResponseParser $ \(_, b) -> (ea, b)

parseError :: String -> HttpResponseParser a
parseError msg = HttpResponseParser $ \(_, b) -> (Left msg, b)

getContentType :: HttpResponseParser ByteString
getContentType = HttpResponseParser $ \(r, b) ->
  case lookup "Content-Type" (HTTP.responseHeaders r) of
    Just ct -> (Right ct, b)
    Nothing -> (Left "No content-type available.", b)

getBody :: HttpResponseParser Body
getBody = HttpResponseParser $ \(r, b) -> (Right b, b)

-- There's probably a better way to do this, but this works for now.
-- At-least the behaviour is well defined now.
class DeSerializeBody t where
  deSerBody :: HttpResponseParser t

instance {-# OVERLAPPABLE #-} (FromJSON t) => DeSerializeBody t where
  deSerBody = do
    ctype <- getContentType
    body <- getBody
    if ctype == "application/json"
      then case body of
        Raw bs -> embed $ eitherDecodeStrict bs
        Obj o -> embed $ eitherDecode $ encode o
      else parseError $ "Unsupported content-type: " ++ show ctype

instance DeSerializeBody Text where
  deSerBody = do
    ctype <- getContentType
    body <- getBody
    case ctype of
      "application/json" -> case body of
        Raw bs -> embed $ eitherDecodeStrict bs
        Obj o -> embed $ eitherDecode $ encode o
      "text/plain" -> case body of
        Raw bs -> pure $ decodeUtf8 bs
        Obj o -> pure $ decodeUtf8 $ toStrict $ encode o
      ct -> parseError $ "Unsupported content-type: " ++ show ct

instance DeSerializeBody StrictByteString where
  deSerBody = do
    ctype <- getContentType
    body <- getBody
    case ctype of
      "application/octet-stream" -> case body of
        Raw bs -> pure bs
        Obj o -> pure $ toStrict $ encode o
      ct -> parseError $ "Unsupported content-type: " ++ show ct

deSerField :: (FromJSON t) => Key -> HttpResponseParser t
deSerField key = HttpResponseParser $ \(_, body) ->
  -- NOTE `application/json` should be asserted here
  let decoded = case body of
        Raw bs -> eitherDecodeStrict bs
        Obj o -> Right o
      parse = Aeson.parseEither (Aeson..: key)
   in -- NOTE Need to change the body to it's decoded version to avoid re-parsing.
      (decoded >>= parse, either (const body) Obj decoded)

class IntoRequestBuilder t where
  intoRequestBuilder :: t -> RequestBuilder ()

class FromResponseParser t where
  expectedStatus :: HTTP.Status
  responseParser :: HttpResponseParser t

class OperationError e where
  getErrorParser :: HTTP.Status -> Maybe (HttpResponseParser e)
  mkDeSerializationError :: Text -> e
  mkUnexpectedError :: Text -> e
  mkUnexpectedStatusError :: HTTP.Status -> e

runOperation ::
  (FromResponseParser t, OperationError e, IntoRequestBuilder i) =>
  Network.URI ->
  HTTP.Manager ->
  RequestBuilder () ->
  Either Text i ->
  IO (Either e t)
runOperation _ _ _ (Left err) = pure $ Left $ mkUnexpectedError err
runOperation endpoint manager setAuth (Right i) = do
  let (_, reqSt) = runStateM (intoRequestBuilder i >> setAuth) newRequestBuilderSt
      initReq = mergeWithHTTPRequest reqSt <$> HTTP.requestFromURI endpoint
  case initReq of
    Just req -> HTTP.withResponse req manager parseOutput
    -- NOTE Should we create this in the client it-self? Would make things alot simpler IMO.
    _ -> pure (Left $ mkUnexpectedError "Failed to parse endpoint, make sure that it's a well-formed http url.")

parseOutput :: forall t e. (FromResponseParser t, OperationError e) => HttpResponse -> IO (Either e t)
parseOutput response = do
  body <- HTTP.brRead (HTTP.responseBody response)
  let status = HTTP.responseStatus response
      parseInput = (response, Raw body)
  if status == (expectedStatus @t)
    then case runParser responseParser parseInput of
      (Right v, _) -> pure $ Right v
      (Left e, _) -> pure $ Left $ mkDeSerializationError (pack e)
    else case getErrorParser status of
      Just p -> pure $ case runParser p parseInput of
        (Right v, _) -> Left v
        (Left e, _) -> Left $ mkDeSerializationError (pack e)
      Nothing -> pure $ Left $ mkUnexpectedStatusError status

-- TESTING

data JsonType = JsonType {_foo :: Text, _bar :: Int64} deriving (Show, Eq, Generic)

instance FromJSON JsonType

instance ToJSON JsonType

instance SerializeBody JsonType

data SomeType = SomeType
  { _someField :: Text,
    _anotherField :: Maybe Int64,
    _aListField :: [Bool],
    _optionalListField :: Maybe [Double],
    _aUTCTimeField :: UTCTime,
    _aHeaderMap :: M.Map Text Int64,
    _jsonField :: Maybe JsonType
    -- _blobField :: ByteString
  }
  deriving (Show, Eq)

instance IntoRequestBuilder SomeType where
  intoRequestBuilder self = do
    setMethod HTTP.methodPost
    setPath ["v1", "some", "endpoint"]
    serHeader "X-Custom-Header" (_someField self)
    serHeader "X-Optional-Int" (_anotherField self)
    serHeader "X-Bool-List" (_aListField self)
    serHeader "X-Optional-Double-List" (_optionalListField self)
    serHeaderMap "X-Prefix-" (_aHeaderMap self)
    serField "timestamp" (_aUTCTimeField self)
    serBody "application/json" (_jsonField self)

instance FromResponseParser SomeType where
  expectedStatus = HTTP.status200
  responseParser =
    SomeType
      <$> deSerHeader "X-Custom-Header"
      <*> deSerHeader "X-Optional-Int"
      <*> deSerHeader "X-Bool-List"
      <*> deSerHeader "X-Optional-Double-List"
      <*> deSerField "timestamp"
      <*> deSerHeaderMap "X-Prefix-"
      <*> deSerBody
