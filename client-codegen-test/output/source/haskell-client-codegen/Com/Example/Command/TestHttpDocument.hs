module Com.Example.Command.TestHttpDocument (
    TestHttpDocumentError (..),
    testHttpDocument
) where
import qualified Com.Example.ExampleServiceClient
import qualified Com.Example.Model.InternalServerError
import qualified Com.Example.Model.TestHttpDocumentInput
import qualified Com.Example.Model.TestHttpDocumentOutput
import qualified Com.Example.Utility
import qualified Data.Aeson
import qualified Data.Text
import qualified GHC.Generics
import qualified GHC.Show

data TestHttpDocumentError =
    InternalServerError Com.Example.Model.InternalServerError.InternalServerError
    | BuilderError Data.Text.Text
    | DeSerializationError Data.Text.Text
    | UnexpectedError Data.Text.Text
    | UnexpectedStatus Data.Text.Text
       deriving (GHC.Generics.Generic, GHC.Show.Show)

instance Data.Aeson.ToJSON TestHttpDocumentError
instance Data.Aeson.FromJSON TestHttpDocumentError
instance Com.Example.Utility.OperationError TestHttpDocumentError where
    mkDeSerializationError = DeSerializationError
    mkUnexpectedError = UnexpectedError
    mkUnexpectedStatusError = UnexpectedStatus . Data.Text.pack . show

    getErrorParser status
        | status == (Com.Example.Utility.expectedStatus @Com.Example.Model.InternalServerError.InternalServerError) = Just (fmap InternalServerError (Com.Example.Utility.responseParser @Com.Example.Model.InternalServerError.InternalServerError))
        | otherwise = Nothing


testHttpDocument :: Com.Example.ExampleServiceClient.ExampleServiceClient -> Com.Example.Model.TestHttpDocumentInput.TestHttpDocumentInputBuilder () -> IO (Either TestHttpDocumentError Com.Example.Model.TestHttpDocumentOutput.TestHttpDocumentOutput)
testHttpDocument client builder =
    let endpoint = Com.Example.ExampleServiceClient.endpointUri client
        manager = Com.Example.ExampleServiceClient.httpManager client
        token = Com.Example.ExampleServiceClient.token client
        setAuth = Com.Example.Utility.serHeader "Authorization" ("Bearer " <> token)
    in Com.Example.Utility.runOperation endpoint manager setAuth (Com.Example.Model.TestHttpDocumentInput.build builder)

