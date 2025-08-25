{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import Com.Example.Command.TestHttpDocument qualified as TestHttpDocument
import Com.Example.Command.TestHttpDocumentDeserialization qualified as TestHttpDocumentDeserialization
import Com.Example.Command.TestHttpHeaders qualified as TestHttpHeaders
import Com.Example.Command.TestHttpLabels qualified as TestHttpLabels
import Com.Example.Command.TestHttpPayload qualified as TestHttpPayload
import Com.Example.Command.TestHttpPayloadDeserialization qualified as TestHttpPayloadDeserialization
import Com.Example.Command.TestQuery qualified as TestQuery
import Com.Example.Command.TestReservedWords qualified as TestReservedWords
import Com.Example.ExampleServiceClient qualified as Client
import Com.Example.Model.CoffeeCustomization qualified as CoffeeCustomization
import Com.Example.Model.CoffeeItem qualified as CoffeeItem
import Com.Example.Model.CoffeeType qualified as CoffeeType
import Com.Example.Model.MilkType qualified as MilkType
import Com.Example.Model.TemperaturePreference qualified as TemperaturePreference
import Com.Example.Model.TestHttpDocumentDeserializationInput qualified as TestHttpDocumentDeserializationInput
import Com.Example.Model.TestHttpDocumentDeserializationInput qualified as TestHttpPayloadDeserializationInput
import Com.Example.Model.TestHttpDocumentDeserializationOutput qualified as TestHttpDocumentDeserializationOutput
import Com.Example.Model.TestHttpDocumentInput qualified as TestHttpDocumentInput
import Com.Example.Model.TestHttpHeadersInput qualified as TestHttpHeadersInput
import Com.Example.Model.TestHttpLabelsInput qualified as TestHttpLabelsInput
import Com.Example.Model.TestHttpPayloadDeserializationOutput qualified as TestHttpPayloadDeserializationOutput
import Com.Example.Model.TestHttpPayloadInput qualified as TestHttpPayloadInput
import Com.Example.Model.TestQueryInput qualified as TestQueryInput
import Com.Example.Model.TestReservedWordsInput qualified as TestReservedWordsInput
import Com.Example.Model.TestReservedWordsOutput qualified as TestReservedWordsOutput
import Control.Concurrent (forkIO)
import Control.Concurrent.MVar as MVar
import Control.Concurrent.STM qualified as Stm
import Control.Monad qualified
import Data.Aeson qualified as Aeson
import Data.ByteString qualified as BS
import Data.Function ((&))
import Data.Functor
import Data.Map qualified as Map
import Data.Maybe (fromJust)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Message (RequestInternal (..), State (..), assertEqRequest, defaultResponse)
import Message qualified as RI
import Network.HTTP.Client.TLS qualified as TLS
import Network.HTTP.Date (parseHTTPDate)
import Network.HTTP.Types qualified as HTTP
import Network.URI qualified as URI
import Network.Wai qualified as Wai
import Network.Wai.Handler.Warp qualified as Warp
import System.Exit (exitFailure)
import Test.HUnit qualified as HUnit
import Data.Either.Extra (fromRight')

port :: Int
port = 4321

createClient :: T.Text -> IO (Either T.Text Client.ExampleServiceClient)
createClient token = do
  manager <- TLS.newTlsManager
  let uriE = URI.parseURI $ "http://localhost:" ++ show port
  case uriE of
    Nothing -> pure $ Left "Invalid URI"
    Just uri -> pure $ Client.build $ do
      Client.setEndpointuri uri
      Client.setHttpmanager manager
      Client.setToken token

testClientCreation :: State -> HUnit.Test
testClientCreation _ = HUnit.TestCase $ do
  clientE <- createClient "test-token"
  case clientE of
    Left err -> HUnit.assertFailure $ "Failed to create client: " ++ T.unpack err
    Right _ -> return ()

tests :: State -> HUnit.Test
tests state =
  HUnit.TestList
    [ HUnit.TestLabel "Client Creation" $ testClientCreation state,
      HUnit.TestLabel "HttpLabels Operation" $ testHttpLabels state,
      HUnit.TestLabel "HttpQuery Operation" $ testHttpQuery state,
      HUnit.TestLabel "HttpHeaders Operation" $ testHttpHeaders state,
      HUnit.TestLabel "HttpPayload Operation" $ testHttpPayload state,
      HUnit.TestLabel "HttpDocument Operation" $ testHttpDocument state,
      HUnit.TestLabel "HttpPayloadDeserialization Operation" $ testHttpPayloadDeserialization state,
      HUnit.TestLabel "HttpDocumentDeserialization Operation" $ testHttpDocumentDeserialization state,
      HUnit.TestLabel "ReservedWords Operation" $ testReservedWords state
    ]

{- TODO Add HTTP semantic assertions for things like `content-type`.
 - These kind of assertions don't belong in the test-defintions. -}
app :: State -> Wai.Application
app state request responder = do
  body <- Wai.consumeRequestBodyStrict request <&> BS.toStrict
  response <-
    Stm.atomically $
      Stm.takeTMVar (res state)
        <* Stm.writeTMVar (req state) request
        <* Stm.writeTMVar (rBody state) body
  responder response

serverSettings :: MVar Bool -> Warp.Settings
serverSettings serverStarted =
  Warp.defaultSettings
    & Warp.setPort port
    & Warp.setBeforeMainLoop afterServerStart
  where
    afterServerStart =
      putStrLn "Server started. Signaling main thread."
        >> MVar.putMVar serverStarted True

-- TODO Add test for handling custom-status codes.
main :: IO ()
main = do
  req <- Stm.newEmptyTMVarIO @Wai.Request
  res <- Stm.newEmptyTMVarIO @Wai.Response
  rBody <- Stm.newEmptyTMVarIO @BS.ByteString

  client <-
    (createClient "test-token") >>= \case
      Left err -> do
        putStrLn $ "Error creating client: " ++ T.unpack err
        exitFailure
      Right c -> pure c

  serverStarted <- MVar.newEmptyMVar :: IO (MVar Bool)

  let state = State {req, res, rBody, client}
  putStrLn $ "Starting server on port " ++ show port

  _ <- forkIO $ Warp.runSettings (serverSettings serverStarted) (app state)
  _ <- takeMVar serverStarted
  putStrLn "Server started. Running PostMenu tests with HUnit..."

  counts <- HUnit.runTestTT $ tests state
  putStrLn $ "Tests run: " ++ show (HUnit.cases counts)
  putStrLn $ "Failures: " ++ show (HUnit.failures counts)
  putStrLn $ "Errors: " ++ show (HUnit.errors counts)

  -- Return exit code based on test results
  Control.Monad.when (HUnit.failures counts > 0 || HUnit.errors counts > 0) exitFailure

testHttpPayload :: State -> HUnit.Test
testHttpPayload state = HUnit.TestCase $ do
  let dateString = "Thu, 04 Jan 2024 14:45:00 GMT"
      identifierValue = 123
      stringHeaderValue = "test-header-value"
      prefixHeadersValue = Map.fromList [("custom", "prefix-value")]
      time = fromJust $ parseHTTPDate dateString

      coffeeItem = fromRight' $ CoffeeItem.build $ do
        CoffeeItem.setCoffeetype CoffeeType.LATTE
        CoffeeItem.setDescription "A smooth latte with silky foam"
        CoffeeItem.setCreatedat time

      expectedPayload = Aeson.toJSON coffeeItem
      expectedRequest =
        RequestInternal
          { RI.queryString = [],
            RI.pathInfo = ["payload", T.pack $ show identifierValue],
            RI.requestMethod = HTTP.methodPost,
            RI.requestHeaders =
              [ ("x-header-string", T.encodeUtf8 stringHeaderValue),
                ("x-prefix-custom", prefixHeadersValue Map.! "custom"),
                ("content-type", "application/json"),
                ("Authorization", "Bearer test-token")
              ]
          }

  _ <- Stm.atomically $ Stm.writeTMVar (res state) (Wai.responseLBS HTTP.created201 [] "{ \"message\": \"Success\" }")

  result <- TestHttpPayload.testHttpPayload (client state) $ do
    TestHttpPayloadInput.setPayload coffeeItem
    TestHttpPayloadInput.setIdentifier identifierValue
    TestHttpPayloadInput.setStringheader (Just stringHeaderValue)
    TestHttpPayloadInput.setPrefixheaders (Just prefixHeadersValue)

  actualReq <- Stm.atomically $ Stm.takeTMVar (req state)
  actualPayload <- Stm.atomically $ Stm.takeTMVar (rBody state)

  assertEqRequest expectedRequest actualReq

  case Aeson.decodeStrict actualPayload of
    Just actualJson -> do
      HUnit.assertEqual "Payload JSON should match" expectedPayload actualJson
    _ -> HUnit.assertFailure "Failed to parse JSON payload"

  case result of
    Left e ->
      HUnit.assertFailure $ "Error: " <> T.decodeUtf8 (Aeson.encode e)
    Right _ ->
      HUnit.assertBool "TestHttpPayload operation successful" True

testHttpQuery :: State -> HUnit.Test
testHttpQuery state = HUnit.TestCase $ do
  let dateString = "Tue, 02 Jan 2024 15:30:00 GMT"
      time = fromJust $ parseHTTPDate dateString
      pageValue = 42
      coffeeTypeValue = "LATTE"
      enabledValue = True
      tagsValue = ["tag1", "tag2", "tag3"]
      customQueryParams = Map.fromList [("custom1", "value1"), ("enabled", "false")]

      expectedRequest =
        RequestInternal
          { RI.queryString =
              [ ("query_literal", Just "some_query_literal_value"),
                ("custom1", Just "value1"),
                ("type", Just $ BS.pack coffeeTypeValue),
                ("page", Just $ BS.pack $ show pageValue),
                ("time", Just dateString),
                ("enabled", Just $ BS.pack $ map toLower (show enabledValue)),
                ("tags", Just "tag1"),
                ("tags", Just "tag2"),
                ("tags", Just "tag3")
              ],
            RI.pathInfo = ["query_params"],
            RI.requestMethod = HTTP.methodGet,
            RI.requestHeaders =
              [ ("Authorization", "Bearer test-token")
              ]
          }

  _ <- Stm.atomically $ Stm.writeTMVar (res state) defaultResponse

  result <- TestQuery.testQuery (client state) $ do
    TestQueryInput.setPage (Just pageValue)
    TestQueryInput.setCoffeetype (Just $ T.pack coffeeTypeValue)
    TestQueryInput.setEnabled (Just enabledValue)
    TestQueryInput.setTags (Just tagsValue)
    TestQueryInput.setTime (Just time)
    TestQueryInput.setMapqueryparams (Just customQueryParams)

  actualReq <- Stm.atomically $ Stm.takeTMVar (req state)
  assertEqRequest expectedRequest actualReq

  case result of
    Left _ -> pure ()
    Right _ ->
      HUnit.assertBool "TestQuery operation successful" True

testHttpPayloadDeserialization :: State -> HUnit.Test
testHttpPayloadDeserialization state = HUnit.TestCase $ do
  let coffeeTypeValue = "ESPRESSO"
      dateString = "Fri, 05 Jan 2024 16:20:00 GMT"

      expectedOutputHeader = "test-output-header"
      expectedOutputHeaderInt = 123
      expectedOutputHeaderBool = True
      expectedOutputHeaderList = ["item1", "item2", "item3"]
      expectedOutputPrefixHeaders = Map.fromList [("custom", "prefix-value"), ("another", "another-value")]
      expectedTimeValue = fromJust $ parseHTTPDate dateString
      expectedCoffeeItem = fromRight' $ CoffeeItem.build $ do
        CoffeeItem.setCoffeetype CoffeeType.LATTE
        CoffeeItem.setDescription "Test latte for deserialization"
        CoffeeItem.setCreatedat expectedTimeValue

      expectedOutput = fromRight' $ TestHttpPayloadDeserializationOutput.build $ do
        TestHttpPayloadDeserializationOutput.setOutputheader (Just $ T.pack expectedOutputHeader)
        TestHttpPayloadDeserializationOutput.setOutputheaderint (Just expectedOutputHeaderInt)
        TestHttpPayloadDeserializationOutput.setOutputheaderbool (Just expectedOutputHeaderBool)
        TestHttpPayloadDeserializationOutput.setOutputheaderlist (Just expectedOutputHeaderList)
        TestHttpPayloadDeserializationOutput.setTime (Just expectedTimeValue)
        TestHttpPayloadDeserializationOutput.setOutputprefixheaders (Just expectedOutputPrefixHeaders)
        TestHttpPayloadDeserializationOutput.setItem (Just expectedCoffeeItem)

      expectedReq =
        RequestInternal
          { RI.requestMethod = HTTP.methodGet,
            RI.pathInfo = ["payload_response"],
            RI.queryString = [("type", Just coffeeTypeValue)],
            RI.requestHeaders =
              [ ("Authorization", "Bearer test-token")
              ]
          }

      mockResponse =
        Wai.responseLBS
          HTTP.status200
          [ ("x-output-header", BS.pack expectedOutputHeader),
            ("x-output-header-int", BS.pack $ show expectedOutputHeaderInt),
            ("x-output-header-bool", BS.pack $ map toLower $ show expectedOutputHeaderBool),
            ("x-output-header-list", BS.pack $ T.unpack $ T.intercalate "," expectedOutputHeaderList),
            ("x-output-header-time", dateString),
            ("x-output-prefix-custom", BS.pack $ T.unpack $ expectedOutputPrefixHeaders Map.! "custom"),
            ("x-output-prefix-another", BS.pack $ T.unpack $ expectedOutputPrefixHeaders Map.! "another"),
            ("Content-Type", "application/json")
          ]
          (encode expectedCoffeeItem)

  _ <- Stm.atomically $ Stm.writeTMVar (res state) mockResponse

  result <- TestHttpPayloadDeserialization.testHttpPayloadDeserialization (client state) $ do
    TestHttpPayloadDeserializationInput.setCoffeetype (Just $ T.decodeUtf8 coffeeTypeValue)

  actualReq <- Stm.atomically $ Stm.takeTMVar (req state)

  assertEqRequest expectedReq actualReq
  case result of
    Left e ->
      HUnit.assertFailure $ "Error: " <> T.decodeUtf8 (encode e)
    Right output -> do
      HUnit.assertEqual "Output should match" expectedOutput output

testHttpLabels :: State -> HUnit.Test
testHttpLabels state = HUnit.TestCase $ do
  let dateString = "Mon, 01 Jan 2024 12:00:00 GMT"
      identifierValue = 42
      enabledValue = True
      nameValue = "test-name"

      time = fromJust $ parseHTTPDate dateString
      expectedRequest =
        RequestInternal
          { RI.queryString = [],
            RI.pathInfo =
              [ "path_params",
                T.pack $ show identifierValue,
                T.toLower (T.pack $ show enabledValue),
                nameValue,
                decodeUtf8 dateString
              ],
            RI.requestMethod = HTTP.methodGet,
            RI.requestHeaders =
              [ ("Authorization", "Bearer test-token")
              ]
          }

  _ <- Stm.atomically $ Stm.writeTMVar (res state) defaultResponse

  result <- TestHttpLabels.testHttpLabels (client state) $ do
    TestHttpLabelsInput.setIdentifier identifierValue
    TestHttpLabelsInput.setEnabled enabledValue
    TestHttpLabelsInput.setName nameValue
    TestHttpLabelsInput.setTime time

  actualReq <- Stm.atomically $ Stm.takeTMVar (req state)

  assertEqRequest expectedRequest actualReq
  case result of
    Left e ->
      HUnit.assertFailure $ "Error: " <> (T.decodeUtf8 $ encode e)
    Right _ ->
      HUnit.assertBool "TestHttpLabels operation successful" True

testHttpHeaders :: State -> HUnit.Test
testHttpHeaders state = HUnit.TestCase $ do
  let dateString = "Wed, 03 Jan 2024 09:15:00 GMT"
      intHeaderValue = 42
      stringHeaderValue = "test-string-value"
      boolHeaderValue = True
      listHeaderValue = ["value1", "value2", "value3"]
      prefixHeadersValue = Map.fromList [("custom1", "prefix-value1"), ("custom2", "prefix-value2")]
      time = fromJust $ parseHTTPDate dateString

      expectedRequest =
        RequestInternal
          { RI.queryString = [],
            RI.pathInfo = ["headers"],
            RI.requestMethod = HTTP.methodGet,
            RI.requestHeaders =
              [ ("x-header-bool", BS.pack $ map toLower $ show boolHeaderValue),
                ("x-header-int", BS.pack $ show intHeaderValue),
                ("x-header-list", BS.pack $ T.unpack $ T.intercalate "," listHeaderValue),
                ("x-header-time", dateString),
                ("x-header-string", BS.pack $ T.unpack stringHeaderValue),
                ("x-prefix-custom1", BS.pack $ T.unpack $ prefixHeadersValue Map.! "custom1"),
                ("x-prefix-custom2", BS.pack $ T.unpack $ prefixHeadersValue Map.! "custom2"),
                ("Authorization", "Bearer test-token")
              ]
          }

  _ <- Stm.atomically $ Stm.writeTMVar (res state) defaultResponse

  result <- TestHttpHeaders.testHttpHeaders (client state) $ do
    TestHttpHeadersInput.setIntheader (Just intHeaderValue)
    TestHttpHeadersInput.setStringheader (Just stringHeaderValue)
    TestHttpHeadersInput.setBoolheader (Just boolHeaderValue)
    TestHttpHeadersInput.setListheader (Just listHeaderValue)
    TestHttpHeadersInput.setTime (Just time)
    TestHttpHeadersInput.setPrefixheaders (Just prefixHeadersValue)

  actualReq <- Stm.atomically $ Stm.takeTMVar (req state)
  assertEqRequest expectedRequest actualReq

  case result of
    Left e ->
      HUnit.assertFailure $ "Error: " <> T.decodeUtf8 (Aeson.encode e)
    Right _ ->
      HUnit.assertBool "TestHttpHeaders operation successful" True

testHttpDocument :: State -> HUnit.Test
testHttpDocument state = HUnit.TestCase $ do
  let dateString = "Thu, 04 Jan 2024 14:45:00 GMT"
      identifierValue = 456
      stringHeaderValue = "test-document-header"
      prefixHeadersValue = Map.fromList [("doc", "document-prefix-value")]
      time = fromJust $ parseHTTPDate dateString

      coffeeItem = fromRight' $ CoffeeItem.build $ do
        CoffeeItem.setCoffeetype CoffeeType.ESPRESSO
        CoffeeItem.setDescription "Strong espresso shot"
        CoffeeItem.setCreatedat time
      customization = CoffeeCustomization.Milk MilkType.OAT

      expectedPayloadJson =
        Aeson.object
          [ "payload" Aeson..= coffeeItem,
            "customization" Aeson..= customization,
            "time" Aeson..= T.decodeUtf8 dateString
          ]

      expectedRequest =
        RequestInternal
          { RI.queryString = [],
            RI.pathInfo = ["document", T.pack $ show identifierValue],
            RI.requestMethod = HTTP.methodPost,
            RI.requestHeaders =
              [ ("x-header-string", BS.pack $ T.unpack stringHeaderValue),
                ("x-prefix-doc", BS.pack $ T.unpack $ prefixHeadersValue Map.! "doc"),
                ("content-type", "application/json"),
                ("Authorization", "Bearer test-token")
              ]
          }

  _ <- Stm.atomically $ Stm.writeTMVar (res state) defaultResponse

  result <- TestHttpDocument.testHttpDocument (client state) $ do
    TestHttpDocumentInput.setPayload (Just coffeeItem)
    TestHttpDocumentInput.setCustomization (Just customization)
    TestHttpDocumentInput.setTime (Just time)
    TestHttpDocumentInput.setIdentifier identifierValue
    TestHttpDocumentInput.setStringheader (Just stringHeaderValue)
    TestHttpDocumentInput.setPrefixheaders (Just prefixHeadersValue)

  -- Take the request and compare it
  actualReq <- Stm.atomically $ Stm.takeTMVar (req state)
  actualPayload <- Stm.atomically $ Stm.takeTMVar (rBody state)

  assertEqRequest expectedRequest actualReq
  case Aeson.decodeStrict actualPayload of
    Just actualJson -> do
      HUnit.assertEqual "Payload should match" expectedPayloadJson actualJson
    _ -> HUnit.assertFailure "Failed to parse JSON document"

  -- Verify operation result
  case result of
    Left e ->
      HUnit.assertFailure $ "Error: " <> T.decodeUtf8 (Aeson.encode e)
    Right _ ->
      HUnit.assertBool "TestHttpDocument operation successful" True

testHttpDocumentDeserialization :: State -> HUnit.Test
testHttpDocumentDeserialization state = HUnit.TestCase $ do
  let coffeeTypeValue = "POUR_OVER"
      dateString = "Fri, 05 Jan 2024 16:20:00 GMT"
      expectedTimeValue = fromJust $ parseHTTPDate dateString

      expectedOutputHeader = "test-document-output-header"
      expectedOutputHeaderInt = 789
      expectedOutputHeaderBool = False
      expectedOutputHeaderList = ["doc1", "doc2", "doc3"]
      expectedOutputPrefixHeaders = Map.fromList [("metadata", "doc-metadata"), ("version", "doc-v1")]

      expectedCoffeeItem = fromRight' $ CoffeeItem.build $ do
        CoffeeItem.setCoffeetype CoffeeType.POUR_OVER
        CoffeeItem.setDescription "Hand-poured coffee for document test"
        CoffeeItem.setCreatedat expectedTimeValue

      expectedCustomization = CoffeeCustomization.Temperature TemperaturePreference.HOT

      expectedRequest =
        RequestInternal
          { RI.queryString = [("type", Just coffeeTypeValue)],
            RI.pathInfo = ["document_response"],
            RI.requestMethod = HTTP.methodGet,
            RI.requestHeaders = [("Authorization", "Bearer test-token")]
          }

      mockResponse =
        Wai.responseLBS
          HTTP.status200
          [ ("x-output-header", BS.pack expectedOutputHeader),
            ("x-output-header-int", BS.pack $ show expectedOutputHeaderInt),
            ("x-output-header-bool", BS.pack $ map toLower $ show expectedOutputHeaderBool),
            ("x-output-header-list", BS.pack $ T.unpack $ T.intercalate "," expectedOutputHeaderList),
            ("x-output-prefix-metadata", BS.pack $ T.unpack $ expectedOutputPrefixHeaders Map.! "metadata"),
            ("x-output-prefix-version", BS.pack $ T.unpack $ expectedOutputPrefixHeaders Map.! "version"),
            ("x-output-header-time", dateString),
            ("Content-Type", "application/json")
          ]
          ( Aeson.object
              [ "item" Aeson..= expectedCoffeeItem,
                "customization" Aeson..= expectedCustomization,
                "time" Aeson..= T.decodeUtf8 dateString
              ]
              & Aeson.encode
          )

      expectedOutput = fromRight' $ TestHttpDocumentDeserializationOutput.build $ do
        TestHttpDocumentDeserializationOutput.setOutputheader (Just $ T.pack expectedOutputHeader)
        TestHttpDocumentDeserializationOutput.setOutputheaderint (Just expectedOutputHeaderInt)
        TestHttpDocumentDeserializationOutput.setOutputheaderbool (Just expectedOutputHeaderBool)
        TestHttpDocumentDeserializationOutput.setOutputheaderlist (Just expectedOutputHeaderList)
        TestHttpDocumentDeserializationOutput.setOutputprefixheaders (Just expectedOutputPrefixHeaders)
        TestHttpDocumentDeserializationOutput.setTime (Just expectedTimeValue)
        TestHttpDocumentDeserializationOutput.setItem (Just expectedCoffeeItem)
        TestHttpDocumentDeserializationOutput.setCustomization (Just expectedCustomization)

  _ <- Stm.atomically $ Stm.writeTMVar (res state) mockResponse

  result <- TestHttpDocumentDeserialization.testHttpDocumentDeserialization (client state) $ do
    TestHttpDocumentDeserializationInput.setCoffeetype (Just $ T.decodeUtf8 coffeeTypeValue)

  actualReq <- Stm.atomically $ Stm.takeTMVar (req state)

  assertEqRequest expectedRequest actualReq

  case result of
    Left e -> pure ()
    Right output -> do
      HUnit.assertEqual "Output should match" expectedOutput output

testReservedWords :: State -> HUnit.Test
testReservedWords state = HUnit.TestCase $ do
  let type' = "type"
      data' = "data"
      as' = "as"
      case' = "case"
      class' = "class"
      default' = "default"
      deriving' = "deriving"
      do' = "do"
      else' = "else"
      hiding' = "hiding"
      if' = "if"
      import' = "import"
      in' = "in"
      infix' = "infix"
      infixl' = "infixl"
      infixr' = "infixr"
      instance' = "instance"
      let' = "let"
      module' = "module"
      newtype' = "newtype"
      of' = "of"
      qualified' = "qualified"
      then' = "then"
      where' = "where"

      payload =
        Aeson.object
          [ "type" Aeson..= type',
            "data" Aeson..= data',
            "as" Aeson..= as',
            "case" Aeson..= case',
            "class" Aeson..= class',
            "default" Aeson..= default',
            "deriving" Aeson..= deriving',
            "do" Aeson..= do',
            "else" Aeson..= else',
            "hiding" Aeson..= hiding',
            "if" Aeson..= if',
            "import" Aeson..= import',
            "in" Aeson..= in',
            "infix" Aeson..= infix',
            "infixl" Aeson..= infixl',
            "infixr" Aeson..= infixr',
            "instance" Aeson..= instance',
            "let" Aeson..= let',
            "module" Aeson..= module',
            "newtype" Aeson..= newtype',
            "of" Aeson..= of',
            "qualified" Aeson..= qualified',
            "then" Aeson..= then',
            "where" Aeson..= where'
          ]

      expectedOutput = fromRight' $ TestReservedWordsOutput.build $ do
        TestReservedWordsOutput.setType' type'
        TestReservedWordsOutput.setData' data'
        TestReservedWordsOutput.setAs' as'
        TestReservedWordsOutput.setCase' case'
        TestReservedWordsOutput.setClass' class'
        TestReservedWordsOutput.setDefault' default'
        TestReservedWordsOutput.setDeriving' deriving'
        TestReservedWordsOutput.setDo' do'
        TestReservedWordsOutput.setElse' else'
        TestReservedWordsOutput.setHiding' hiding'
        TestReservedWordsOutput.setIf' if'
        TestReservedWordsOutput.setImport' import'
        TestReservedWordsOutput.setIn' in'
        TestReservedWordsOutput.setInfix' infix'
        TestReservedWordsOutput.setInfixl' infixl'
        TestReservedWordsOutput.setInfixr' infixr'
        TestReservedWordsOutput.setInstance' instance'
        TestReservedWordsOutput.setLet' let'
        TestReservedWordsOutput.setModule' module'
        TestReservedWordsOutput.setNewtype' newtype'
        TestReservedWordsOutput.setOf' of'
        TestReservedWordsOutput.setQualified' qualified'
        TestReservedWordsOutput.setThen' then'
        TestReservedWordsOutput.setWhere' where'

      mockResponse = Wai.responseLBS HTTP.status200 [] (Aeson.encode payload)

  _ <- Stm.atomically $ Stm.writeTMVar (res state) mockResponse

  result <- TestReservedWords.testReservedWords (client state) $ do
    TestReservedWordsInput.setType' type'
    TestReservedWordsInput.setData' data'
    TestReservedWordsInput.setAs' as'
    TestReservedWordsInput.setCase' case'
    TestReservedWordsInput.setClass' class'
    TestReservedWordsInput.setDefault' default'
    TestReservedWordsInput.setDeriving' deriving'
    TestReservedWordsInput.setDo' do'
    TestReservedWordsInput.setElse' else'
    TestReservedWordsInput.setHiding' hiding'
    TestReservedWordsInput.setIf' if'
    TestReservedWordsInput.setImport' import'
    TestReservedWordsInput.setIn' in'
    TestReservedWordsInput.setInfix' infix'
    TestReservedWordsInput.setInfixl' infixl'
    TestReservedWordsInput.setInfixr' infixr'
    TestReservedWordsInput.setInstance' instance'
    TestReservedWordsInput.setLet' let'
    TestReservedWordsInput.setModule' module'
    TestReservedWordsInput.setNewtype' newtype'
    TestReservedWordsInput.setOf' of'
    TestReservedWordsInput.setQualified' qualified'
    TestReservedWordsInput.setThen' then'
    TestReservedWordsInput.setWhere' where'

  actualPayload <- Stm.atomically $ Stm.takeTMVar (rBody state)
  case Aeson.decodeStrict actualPayload of
    Just actualJson -> do
      HUnit.assertEqual "Payload should match" payload actualJson
    _ -> HUnit.assertFailure "Failed to parse JSON document"

  case result of
    Left _ -> pure ()
    Right output -> do
      HUnit.assertEqual "Output should match" expectedOutput output
