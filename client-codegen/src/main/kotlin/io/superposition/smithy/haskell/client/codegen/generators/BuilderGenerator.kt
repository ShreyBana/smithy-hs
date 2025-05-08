package io.superposition.smithy.haskell.client.codegen.generators

import io.superposition.smithy.haskell.client.codegen.HaskellWriter
import io.superposition.smithy.haskell.client.codegen.toMaybe
import software.amazon.smithy.codegen.core.Symbol
import software.amazon.smithy.codegen.core.SymbolProvider
import software.amazon.smithy.model.shapes.StructureShape

class BuilderGenerator(
    val shape: StructureShape,
    val symbolProvider: SymbolProvider,
    val writer: HaskellWriter
) : Runnable {
    private data class BuilderStateMember(val name: String, val symbol: Symbol)

    private val builderName = "${shape.id.name}Builder"
    private val stateName = "${builderName}State"
    private val builderStateMembers = shape.members().map {
        val name = "${symbolProvider.toMemberName(it)}BuilderState"
        val symbol = symbolProvider.toSymbol(it).toMaybe()
        return@map BuilderStateMember(name, symbol)
    }

    @Suppress("MaxLineLength")
    override fun run() {
        writer.pushState()
        writer.putContext("builderState", Runnable(::builderStateSection))
        writer.putContext("defaultBuilderState", Runnable(::defaultBuilderState))
        writer.write(
            """
           #{builderState:C}

           #{defaultBuilderState:C}

            newtype $builderName a =
                $builderName { run$builderName :: $stateName -> ($stateName, a) }

            instance #{functor:T} $builderName where
                fmap f ($builderName g) = $builderName ${'$'} \s -> let (s', a) = g s in (s', f a)

            instance #{applicative:T} $builderName where
                pure a = $builderName ${'$'} \s -> (s, a)
                ($builderName f) <*> ($builderName g) = $builderName ${'$'} \s ->
                    let (s', fab) = f s
                        (s'', a) = g s'
                    in (s'', fab a)

            instance #{monad:T} $builderName where
                ($builderName g) >>= f = $builderName ${'$'} \s ->
                    let (s', a) = g s
                        $builderName h = f a
                    in h s'
            """.trimIndent()
        )
        writer.popState()
    }

    private fun builderStateSection() {
        writer.openBlock("data $stateName = $stateName {", "}") {
            builderStateMembers.map {
                writer.write("${it.name} :: #T,", it.symbol)
            }
        }
    }

    private fun defaultBuilderState() {
        writer.openBlock("default$stateName = $stateName {", "}") {
            builderStateMembers.map {
                writer.write("${it.name} = Nothing,")
            }
        }
    }

    /**
     * Generates a builder struct/monad for the provided Smithy shape.
     */
//    private fun generateBuilderMonad(writer: HaskellWriter, shape: StructureShape) {
//        val shapeName = shape.id.name
//        val builderName = "${shapeName}Builder"
//        val stateName = "${shapeName}State"
//        val symbolProvider = writer.symbolProvider

//        // Generate the builder state data type with Maybe fields
//        val stateFields = shape.members().joinToString("\n") { member ->
//            val memberName = symbolProvider.toMemberName(member)
//            val memberType = symbolProvider.toSymbol(member).toMaybe()
//            "  _${memberName}State :: #T,".also { writer.importSymbol(memberType) }
//        }

//        writer.write("""

//            -- Builder state for ${shapeName}
//            data $stateName = $stateName {
//            $stateFields
//            }
//        """.trimIndent())

//        // Generate empty state
//        val emptyStateFields = shape.members().joinToString("\n") { member ->
//            val memberName = symbolProvider.toMemberName(member)
//            "  _${memberName}State = Nothing,"
//        }

//        writer.write("""
//            empty${stateName} :: $stateName
//            empty${stateName} = $stateName {
//            $emptyStateFields
//            }
//        """.trimIndent())

//        // Generate the builder monad type and instances
//        writer.write("""

//            -- Builder monad for ${shapeName}
//            newtype $builderName a = $builderName { run$builderName :: $stateName -> ($stateName, a) }

//            instance Functor $builderName where
//              fmap f (${builderName} g) = $builderName $ \\s -> let (s', a) = g s in (s', f a)

//            instance Applicative $builderName where
//              pure a = $builderName $ \\s -> (s, a)
//              (${builderName} f) <*> (${builderName} g) = $builderName $ \\s ->
//                let (s', fab) = f s
//                    (s'', a) = g s'
//                in (s'', fab a)

//            instance Monad $builderName where
//              (${builderName} g) >>= f = $builderName $ \\s ->
//                let (s', a) = g s
//                    ${builderName} h = f a
//                in h s'
//        """.trimIndent())

//        // Generate builder execution function
//        writer.write("""

//            build$shapeName :: $builderName a -> Either String $shapeName
//            build$shapeName ($builderName f) =
//              let (state, _) = f empty${stateName}
//              in fromBuilderState state
//        """.trimIndent())

//        // Generate validation function
//        val validationChecks = shape.members().joinToString("\n") { member ->
//            val memberName = symbolProvider.toMemberName(member)
//            "  ${memberName}Val <- maybe (Left \"Missing required field: ${memberName}\") Right (_${memberName}State state)"
//        }

//        val structFields = shape.members().joinToString("\n") { member ->
//            val memberName = symbolProvider.toMemberName(member)
//            "    ${memberName} = ${memberName}Val,"
//        }

//        writer.write("""

//            fromBuilderState :: $stateName -> Either String $shapeName
//            fromBuilderState state = do
//            $validationChecks
//              return $shapeName {
//            $structFields
//              }
//        """.trimIndent())

//        // Generate setter functions
//        for (member in shape.members()) {
//            val memberName = symbolProvider.toMemberName(member)
//            val memberType = symbolProvider.toSymbol(member)
//            writer.write("""

//                set$memberName :: #T -> $builderName ()
//                set$memberName val = $builderName $ \\s -> (s { _${memberName}State = Just val }, ())
//            """.trimIndent(), memberType)
//        }
//    }
}
