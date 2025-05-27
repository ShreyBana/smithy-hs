package io.superposition.smithy.haskell.client.codegen.language

import software.amazon.smithy.codegen.core.SymbolDependency

data class Module(val name: String, val dependency: SymbolDependency)
