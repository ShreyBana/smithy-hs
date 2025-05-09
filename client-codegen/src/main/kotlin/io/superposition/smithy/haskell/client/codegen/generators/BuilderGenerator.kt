package io.superposition.smithy.haskell.client.codegen.generators

import io.superposition.smithy.haskell.client.codegen.HaskellWriter
import io.superposition.smithy.haskell.client.codegen.toMaybe
import software.amazon.smithy.codegen.core.Symbol
import software.amazon.smithy.codegen.core.SymbolProvider
import software.amazon.smithy.model.shapes.MemberShape
import software.amazon.smithy.model.shapes.StructureShape
import software.amazon.smithy.utils.CaseUtils

class BuilderGenerator(
    val shape: StructureShape,
    val symbol: Symbol,
    val symbolProvider: SymbolProvider,
    val writer: HaskellWriter
) : Runnable {
    private data class BuilderStateMember(
        val name: String,
        val symbol: Symbol,
        val member: MemberShape
    )

    private val builderName = "${shape.id.name}Builder"
    private val stateName = "${builderName}State"
    private val builderStateMembers = shape.members().map {
        val name = "${symbolProvider.toMemberName(it)}BuilderState"
        val symbol = symbolProvider.toSymbol(it).toMaybe()
        return@map BuilderStateMember(name, symbol, it)
    }

    @Suppress("MaxLineLength")
    override fun run() {
        writer.pushState()
        writer.putContext("builderState", Runnable(::builderStateSection))
        writer.putContext("defaultBuilderState", Runnable(::defaultBuilderState))
        writer.putContext("builderSetters", Runnable(::builderSetters))
        writer.putContext("builderGetters", Runnable(::builderGetters))
        writer.putContext("buildInput", Runnable(::buildInput))
        writer.write(
            """
           #{builderState:C}

           #{defaultBuilderState:C}

           newtype $builderName a = $builderName {
               run$builderName :: $stateName -> ($stateName, a)
           }

           instance #{functor:T} $builderName where
               fmap f ($builderName g) =
                   $builderName (\s -> let (s', a) = g s in (s', f a))

           instance #{applicative:T} $builderName where
               pure a = $builderName (\s -> (s, a))
               ($builderName f) <*> ($builderName g) = $builderName (\s ->
                   let (s', h) = f s
                       (s'', a) = g s'
                   in (s'', h a))

           instance #{monad:T} $builderName where
               ($builderName f) >>= g = $builderName (\s ->
                   let (s', a) = f s
                       ($builderName h) = g a
                   in h s')

           #{builderSetters:C}
           #{builderGetters:C}
           #{buildInput:C}
            """.trimIndent()
        )
        writer.popState()
    }

    private fun builderStateSection() {
        writer.openBlock("data $stateName = $stateName {", "}") {
            builderStateMembers.forEach {
                writer.write("${it.name} :: #T,", it.symbol)
            }
        }
    }

    private fun defaultBuilderState() {
        val fn = "defaultBuilderState"
        writer.write("$fn :: $stateName")
        writer.openBlock("$fn = $stateName {", "}") {
            builderStateMembers.forEach {
                writer.write("${it.name} = Nothing,")
            }
        }
    }

    private fun builderSetters() {
        builderStateMembers.forEach {
            val fn = setterName(it.member.memberName)
            // TODO Use a builder-symbol if the field is a structure.
            writer.write(
                """
                $fn :: #T -> $builderName ()
                $fn value =
                    $builderName (\s -> (s { ${it.name} = (Just value) }, ()))

                """.trimIndent(),
                symbolProvider.toSymbol(it.member)
            )
        }
    }

    private fun builderGetters() {
        builderStateMembers.forEach {
            val fn = getterName(it.member.memberName)
            // TODO Run the builder if's a builder in state.
            writer.write(
                """
                $fn :: $builderName a -> #T
                $fn ($builderName f) = f default$stateName

                """.trimIndent(),
                symbolProvider.toSymbol(it.member).toMaybe()
            )
        }
    }

    private fun buildInput() {
        val fn = "build${shape.id.name}"
        val mNames = builderStateMembers.map { it.member.memberName }
        writer.write("$fn :: $builderName () -> #{either:T} #{text:T} ${shape.id.name}")
        writer.openBlock("$fn builder = do", "") {
            mNames.forEachIndexed { i, mn ->
                val gfn = getterName(mn)
                // TODO Check for nullability/@required.
                // NOTE We're already using maybe's in this mod, so no worries of import here.
                writer.write(
                    """
                    $mn' <- maybe (Left e$i) Right ($gfn builder)
                    """.trimIndent()
                )
            }
            writer.openBlock("Right (#T { ", "})", symbol) {
                mNames.forEach {
                    writer.write("$it = $it'")
                }
            }
            writer.openBlock("where", "") {
                mNames.forEachIndexed { i, mn ->
                    writer.write("e$i = \"$symbol.$mn is a required property.\"")
                }
            }
        }
    }

    companion object {
        fun getterName(fieldName: String) = CaseUtils.toCamelCase("get $fieldName")
        fun setterName(fieldName: String) = CaseUtils.toCamelCase("set $fieldName")
    }
}
