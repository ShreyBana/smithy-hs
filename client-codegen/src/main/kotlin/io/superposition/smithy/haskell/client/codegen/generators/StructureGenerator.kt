@file:Suppress("FINITE_BOUNDS_VIOLATION_IN_JAVA")

package io.superposition.smithy.haskell.client.codegen.generators

import io.superposition.smithy.haskell.client.codegen.HaskellContext
import io.superposition.smithy.haskell.client.codegen.HaskellSettings
import io.superposition.smithy.haskell.client.codegen.HaskellWriter
import software.amazon.smithy.codegen.core.SymbolProvider
import software.amazon.smithy.codegen.core.directed.ShapeDirective
import software.amazon.smithy.model.shapes.StructureShape
import java.util.function.Consumer

@Suppress("MaxLineLength")
class StructureGenerator<T : ShapeDirective<StructureShape, HaskellContext, HaskellSettings>> : Consumer<T> {

    override fun accept(directive: T) {
        // Generate structure code
        val symbolProvider = directive.symbolProvider()
        val shape = directive.shape()
        val symbol = symbolProvider.toSymbol(shape)
        directive.context().writerDelegator().useShapeWriter(shape) { writer ->
            writer.write("#C", writer.consumer(DataSection(shape, symbolProvider)::accept))
            writer.write("#C", BuilderGenerator(shape, symbol, symbolProvider, writer))
        }
    }

    class DataSection(val shape: StructureShape, val symbolProvider: SymbolProvider) {
        fun accept(writer: HaskellWriter) {
            val symbol = symbolProvider.toSymbol(shape)
            writer.openBlock("data #T = #T {", "}", symbol, symbol) {
                shape.members().map {
                    val mName = symbolProvider.toMemberName(it)
                    val mSymbol = symbolProvider.toSymbol(it)
                    writer.write("$mName :: #T,", mSymbol)
                }
            }
        }
    }
}
