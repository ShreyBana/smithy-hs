module Com.Example.Model.TestCustomStatusInput (
    setType',
    build,
    TestCustomStatusInputBuilder,
    TestCustomStatusInput,
    type'
) where
import qualified Com.Example.Utility
import qualified Control.Applicative
import qualified Control.Monad
import qualified Data.Aeson
import qualified Data.Either
import qualified Data.Eq
import qualified Data.Functor
import qualified Data.Maybe
import qualified Data.Text
import qualified GHC.Generics
import qualified GHC.Show
import qualified Network.HTTP.Types.Method

data TestCustomStatusInput = TestCustomStatusInput {
    type' :: Data.Text.Text
} deriving (
  GHC.Show.Show,
  Data.Eq.Eq,
  GHC.Generics.Generic
  )

instance Data.Aeson.ToJSON TestCustomStatusInput where
    toJSON a = Data.Aeson.object [
        "type" Data.Aeson..= type' a
        ]
    

instance Com.Example.Utility.SerializeBody TestCustomStatusInput

instance Data.Aeson.FromJSON TestCustomStatusInput where
    parseJSON = Data.Aeson.withObject "TestCustomStatusInput" $ \v -> TestCustomStatusInput
        Data.Functor.<$> (v Data.Aeson..: "type")
    



data TestCustomStatusInputBuilderState = TestCustomStatusInputBuilderState {
    type'BuilderState :: Data.Maybe.Maybe Data.Text.Text
} deriving (
  GHC.Generics.Generic
  )

defaultBuilderState :: TestCustomStatusInputBuilderState
defaultBuilderState = TestCustomStatusInputBuilderState {
    type'BuilderState = Data.Maybe.Nothing
}

newtype TestCustomStatusInputBuilder a = TestCustomStatusInputBuilder {
    runTestCustomStatusInputBuilder :: TestCustomStatusInputBuilderState -> (TestCustomStatusInputBuilderState, a)
}

instance Data.Functor.Functor TestCustomStatusInputBuilder where
    fmap f (TestCustomStatusInputBuilder g) =
        TestCustomStatusInputBuilder (\s -> let (s', a) = g s in (s', f a))

instance Control.Applicative.Applicative TestCustomStatusInputBuilder where
    pure a = TestCustomStatusInputBuilder (\s -> (s, a))
    (TestCustomStatusInputBuilder f) <*> (TestCustomStatusInputBuilder g) = TestCustomStatusInputBuilder (\s ->
        let (s', h) = f s
            (s'', a) = g s'
        in (s'', h a))

instance Control.Monad.Monad TestCustomStatusInputBuilder where
    (TestCustomStatusInputBuilder f) >>= g = TestCustomStatusInputBuilder (\s ->
        let (s', a) = f s
            (TestCustomStatusInputBuilder h) = g a
        in h s')

setType' :: Data.Text.Text -> TestCustomStatusInputBuilder ()
setType' value =
   TestCustomStatusInputBuilder (\s -> (s { type'BuilderState = Data.Maybe.Just value }, ()))

build :: TestCustomStatusInputBuilder () -> Data.Either.Either Data.Text.Text TestCustomStatusInput
build builder = do
    let (st, _) = runTestCustomStatusInputBuilder builder defaultBuilderState
    type'' <- Data.Maybe.maybe (Data.Either.Left "Com.Example.Model.TestCustomStatusInput.TestCustomStatusInput.type' is a required property.") Data.Either.Right (type'BuilderState st)
    Data.Either.Right (TestCustomStatusInput { 
        type' = type''
    })


instance Com.Example.Utility.IntoRequestBuilder TestCustomStatusInput where
    intoRequestBuilder self = do
        Com.Example.Utility.setMethod Network.HTTP.Types.Method.methodPost
        Com.Example.Utility.setPath [
            "custom-status"
            ]
        
        
        Com.Example.Utility.serField "type" (type' self)

