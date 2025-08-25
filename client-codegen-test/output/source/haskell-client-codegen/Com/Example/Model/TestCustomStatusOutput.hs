module Com.Example.Model.TestCustomStatusOutput (
    build,
    TestCustomStatusOutputBuilder,
    TestCustomStatusOutput
) where
import qualified Com.Example.Utility
import qualified Control.Applicative
import qualified Control.Monad
import qualified Data.Aeson
import qualified Data.Either
import qualified Data.Eq
import qualified Data.Functor
import qualified Data.Text
import qualified GHC.Generics
import qualified GHC.Show
import qualified Network.HTTP.Types

data TestCustomStatusOutput = TestCustomStatusOutput {
} deriving (
  GHC.Show.Show,
  Data.Eq.Eq,
  GHC.Generics.Generic
  )

instance Data.Aeson.ToJSON TestCustomStatusOutput where
    toJSON a = Data.Aeson.object [
        ]
    

instance Com.Example.Utility.SerializeBody TestCustomStatusOutput

instance Data.Aeson.FromJSON TestCustomStatusOutput where
    parseJSON = Data.Aeson.withObject "TestCustomStatusOutput" $ \_ -> pure $ TestCustomStatusOutput



data TestCustomStatusOutputBuilderState = TestCustomStatusOutputBuilderState {
} deriving (
  GHC.Generics.Generic
  )

defaultBuilderState :: TestCustomStatusOutputBuilderState
defaultBuilderState = TestCustomStatusOutputBuilderState {
}

newtype TestCustomStatusOutputBuilder a = TestCustomStatusOutputBuilder {
    runTestCustomStatusOutputBuilder :: TestCustomStatusOutputBuilderState -> (TestCustomStatusOutputBuilderState, a)
}

instance Data.Functor.Functor TestCustomStatusOutputBuilder where
    fmap f (TestCustomStatusOutputBuilder g) =
        TestCustomStatusOutputBuilder (\s -> let (s', a) = g s in (s', f a))

instance Control.Applicative.Applicative TestCustomStatusOutputBuilder where
    pure a = TestCustomStatusOutputBuilder (\s -> (s, a))
    (TestCustomStatusOutputBuilder f) <*> (TestCustomStatusOutputBuilder g) = TestCustomStatusOutputBuilder (\s ->
        let (s', h) = f s
            (s'', a) = g s'
        in (s'', h a))

instance Control.Monad.Monad TestCustomStatusOutputBuilder where
    (TestCustomStatusOutputBuilder f) >>= g = TestCustomStatusOutputBuilder (\s ->
        let (s', a) = f s
            (TestCustomStatusOutputBuilder h) = g a
        in h s')


build :: TestCustomStatusOutputBuilder () -> Data.Either.Either Data.Text.Text TestCustomStatusOutput
build builder = do
    let (st, _) = runTestCustomStatusOutputBuilder builder defaultBuilderState
    Data.Either.Right (TestCustomStatusOutput { 
    })


instance Com.Example.Utility.FromResponseParser TestCustomStatusOutput where
    expectedStatus = Network.HTTP.Types.status201
    responseParser = do
        
        
        pure $ TestCustomStatusOutput {
            
        }

