module Com.Example.Command.TestReservedWords (
    TestReservedWordsError (..),
    testReservedWords
) where
import qualified Com.Example.ExampleServiceClient
import qualified Com.Example.Model.InternalServerError
import qualified Com.Example.Model.TestReservedWordsInput
import qualified Com.Example.Model.TestReservedWordsOutput
import qualified Com.Example.Utility
import qualified Data.Aeson
import qualified Data.Text
import qualified GHC.Generics
import qualified GHC.Show

data TestReservedWordsError =
    InternalServerError Com.Example.Model.InternalServerError.InternalServerError
    | BuilderError Data.Text.Text
    | DeSerializationError Data.Text.Text
    | UnexpectedError Data.Text.Text
    | UnexpectedStatus Data.Text.Text
       deriving (GHC.Generics.Generic, GHC.Show.Show)

instance Data.Aeson.ToJSON TestReservedWordsError
instance Data.Aeson.FromJSON TestReservedWordsError
instance Com.Example.Utility.OperationError TestReservedWordsError where
    mkDeSerializationError = DeSerializationError
    mkUnexpectedError = UnexpectedError
    mkUnexpectedStatusError = UnexpectedStatus . Data.Text.pack . show

    getErrorParser status
        | status == (Com.Example.Utility.expectedStatus @Com.Example.Model.InternalServerError.InternalServerError) = Just (fmap InternalServerError (Com.Example.Utility.responseParser @Com.Example.Model.InternalServerError.InternalServerError))
        | otherwise = Nothing


testReservedWords :: Com.Example.ExampleServiceClient.ExampleServiceClient -> Com.Example.Model.TestReservedWordsInput.TestReservedWordsInputBuilder () -> IO (Either TestReservedWordsError Com.Example.Model.TestReservedWordsOutput.TestReservedWordsOutput)
testReservedWords client builder =
    let endpoint = Com.Example.ExampleServiceClient.endpointUri client
        manager = Com.Example.ExampleServiceClient.httpManager client
        token = Com.Example.ExampleServiceClient.token client
        setAuth = Com.Example.Utility.serHeader "Authorization" ("Bearer " <> token)
    in Com.Example.Utility.runOperation endpoint manager setAuth (Com.Example.Model.TestReservedWordsInput.build builder)

