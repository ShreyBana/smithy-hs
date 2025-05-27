package io.superposition.smithy.haskell.client.codegen.language

data class Function(val name: String, val module: String) {
    companion object {
        val LIBRARY_FUNCTIONS = HashSet<Function>()

        fun newLibraryFunction(name: String, module: String): Function {
            val function = Function(name, module)
            LIBRARY_FUNCTIONS.add(function)
            return function
        }
    }
}
