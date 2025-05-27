package io.superposition.smithy.haskell.client.codegen

import io.superposition.smithy.haskell.client.codegen.language.Function
import io.superposition.smithy.haskell.client.codegen.language.Module
import software.amazon.smithy.codegen.core.Property
import software.amazon.smithy.codegen.core.Symbol
import software.amazon.smithy.codegen.core.SymbolDependency

object SymbolProperties {
    val IS_PRIMITIVE: Property<Boolean> = Property.named("is-primitive")
}

object HttpClient {
    val dependency = SymbolDependency.builder()
        .packageName("http-client")
        .version(CodegenUtils.depRange("0.5.14", "0.8"))
        .build()

    private val MODULE_NAME = "Network.HTTP.Client"

    val Manager: Symbol = Symbol.builder()
        .name("Manager")
        .namespace(MODULE_NAME, ".")
        .dependencies(dependency)
        .build()

    val ManagerSettings: Symbol = Manager.toBuilder()
        .name("ManagerSettings")
        .build()

    val Request: Symbol = Symbol.builder()
        .name("Request")
        .namespace(MODULE_NAME, ".")
        .dependencies(dependency)
        .build()

    val path = Function.newLibraryFunction("path", MODULE_NAME)
    val method = Function.newLibraryFunction("method", MODULE_NAME)
    val requestHeaders = Function.newLibraryFunction("requestHeaders", MODULE_NAME)
    val requestBody = Function.newLibraryFunction("requestBody", MODULE_NAME)
    val queryString = Function.newLibraryFunction("queryString", MODULE_NAME)
    val httpLbs = Function.newLibraryFunction("httpLbs", MODULE_NAME)
    val defaultManagerSettings = Function.newLibraryFunction("defaultManagerSettings", MODULE_NAME)
    val newManager = Function.newLibraryFunction("newManager", MODULE_NAME)
}

object NetworkUri {
    val dependency = SymbolDependency.builder()
        .packageName("network-uri")
        .version(CodegenUtils.depRange("2.6", "2.7"))
        .build()

    val URI: Symbol = Symbol.builder()
        .name("URI")
        .namespace("Network.URI", ".")
        .dependencies(dependency)
        .build()
}

object CaseInsensitive {
    val dependency = SymbolDependency.builder()
        .packageName("case-insensitive")
        .version(CodegenUtils.depRange("1.2.1", "1.3"))
        .build()

    val CI: Symbol = Symbol.builder()
        .name("CI")
        .namespace("Data.CaseInsensitive", ".")
        .dependencies(dependency)
        .build()
}

object Text {
    val dependency = SymbolDependency.builder()
        .packageName("text")
        .version(CodegenUtils.depRange("1.2.3", "2.1"))
        .build()

    val Text: Symbol = Symbol.builder()
        .name("Text")
        .namespace("Data.Text", ".")
        .dependencies(dependency)
        .build()

    val pack = Function.newLibraryFunction("pack", "Data.Text")
    val encodeUtf8 = Function.newLibraryFunction("encodeUtf8", "Data.Text.Encoding")
}

object Aeson {
    val dependency = SymbolDependency.builder()
        .packageName("aeson")
        .version(CodegenUtils.depRange("2.0.0", "2.2.0"))
        .build()

    val Aeson: Symbol = Symbol.builder()
        .name("Aeson")
        .namespace("Data.Aeson", ".")
        .dependencies(dependency)
        .build()

    val ToJSON: Symbol = Aeson.toBuilder()
        .name("ToJSON")
        .build()

    val JsonString: Symbol = Aeson.toBuilder()
        .name("String")
        .build()

    val object_ = Function.newLibraryFunction("object", "Data.Aeson")
    val parseEither = Function.newLibraryFunction("parseEither", "Data.Aeson.Types")
}

object ByteString {
    val dependency = SymbolDependency.builder()
        .packageName("bytestring")
        .version(CodegenUtils.depRange("0.10.12", "0.12.0"))
        .build()

    val ByteString: Symbol = Symbol.builder()
        .name("ByteString")
        .namespace("Data.ByteString", ".")
        .dependencies(dependency)
        .build()

    val LazyByteString: Symbol = Symbol.builder()
        .name("ByteString")
        .namespace("Data.ByteString.Lazy", ".")
        .dependencies(dependency)
        .build()
}

object Containers {
    val dependency = SymbolDependency.builder()
        .packageName("containers")
        .version(CodegenUtils.depRange("0.6.4", "0.7"))
        .build()

    val Map: Symbol = Symbol.builder()
        .name("Map")
        .namespace("Data.Map", ".")
        .dependencies(dependency)
        .build()
}

object HttpTypes {
    val dependency = SymbolDependency.builder()
        .packageName("http-types")
        .version(CodegenUtils.depRange("0.12.3", "0.13"))
        .build()

    val Query: Symbol = Symbol.builder()
        .name("Query")
        .namespace("Network.HTTP.Types.URI", ".")
        .dependencies(dependency)
        .build()

    private val METHOD_MODULE = "Network.HTTP.Types.Method"
    val methodPost = Function.newLibraryFunction("methodPost", METHOD_MODULE)
    val methodGet = Function.newLibraryFunction("methodGet", METHOD_MODULE)
    val methodPut = Function.newLibraryFunction("methodPut", METHOD_MODULE)
    val methodHead = Function.newLibraryFunction("methodHead", METHOD_MODULE)
    val methodDelete = Function.newLibraryFunction("methodDelete", METHOD_MODULE)
    val methodPatch = Function.newLibraryFunction("methodPatch", METHOD_MODULE)
    val methodOptions = Function.newLibraryFunction("methodOptions", METHOD_MODULE)
    val methodTrace = Function.newLibraryFunction("methodTrace", METHOD_MODULE)
    val methodConnect = Function.newLibraryFunction("methodConnect", METHOD_MODULE)
}

object Base {
    val BaseDep = SymbolDependency.builder()
        .packageName("base")
        .version(CodegenUtils.depRange("4.14", "4.19"))
        .build()

    val IO: Symbol = Symbol.builder()
        .name("IO")
        .build()

    val Maybe: Symbol = Symbol.builder()
        .name("Maybe")
        .namespace("Data.Maybe", ".")
        .build()

    val Monoid: Symbol = Symbol.builder()
        .name("Monoid")
        .namespace("Data.Monoid", ".")
        .build()
    val mempty = Function.newLibraryFunction("mempty", Module("Data.Monoid", BaseDep))

    val Either: Symbol = Symbol.builder()
        .name("Either")
        .namespace("Data.Either", ".")
        .build()

    val Functor: Symbol = Symbol.builder()
        .name("Functor")
        .namespace("Data.Functor", ".")
        .build()

    val Eq: Symbol = Symbol.builder()
        .name("Eq")
        .namespace("Data.Eq", ".")
        .build()

    val List: Symbol = Symbol.builder()
        .name("List")
        .namespace("Data.List", ".")
        .build()

    val Flip: Symbol = Symbol.builder()
        .name("&")
        .namespace("Data.Function", ".")
        .build()
    val And = Flip

    val first = Function.newLibraryFunction("first", "Data.Bifunctor")

    val Applicative: Symbol = Symbol.builder()
        .name("Applicative")
        .namespace("Control.Applicative", ".")
        .build()

    val Monad: Symbol = Symbol.builder()
        .name("Monad")
        .namespace("Control.Monad", ".")
        .build()

    val SomeException: Symbol = Symbol.builder()
        .name("SomeException")
        .namespace("Control.Exception", ".")
        .build()

    val Generic: Symbol = Symbol.builder()
        .name("Generic")
        .namespace("GHC.Generics", ".")
        .build()
}
