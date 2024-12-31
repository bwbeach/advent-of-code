package net.beachfamily.aoc

import java.io.File

fun main() {
    val input = readInput("y2024d24")
    println(part1(input))
    println(part2(input))
}

fun part1(s: String) : Long {
    val problem = parse(s, ::ident)
    val values = evaluateAll(problem)
    val zees = values.keys.filter { it.startsWith("z") }
    return zees
        .sorted()
        .reversed()
        .map { values[it]!! }
        .fold(0) { acc, i -> (acc shl 1) or i }
}

fun part2(s: String) : String {
    val problem = parse(s, ::swapForPart2)
    writeDot(problem)

    for (i in 0 .. 44) {
        val name = "z%02d".format(i)
        val stdName = StandardName.forDefinition(problem[name]!!, problem)
        if (stdName == null || stdName.flavor != Flavor.ZEE || stdName.index != i) {
            println("BROKEN: $name")
            break
        }
    }

    return swaps
        .flatMap { listOf(it.first, it.second) }
        .sorted()
        .joinToString(",")
}

enum class Operator {
    AND,
    OR,
    XOR,
}

sealed class Definition(
    val name: String,
) {
    class Const(name: String, val value: Long) : Definition(name)
    class Operation(
        name: String,
        val op: Operator,
        val lhs: String,
        val rhs: String,
    ) : Definition(name)
}

typealias Problem = Map<String, Definition>

fun parse(s: String, swapper: (String) -> String): Problem {
    val (a, b) = lines(s).asSequence().splitBy { it.isBlank() }.asPair()
    val constants = a.map { parseConstant(it) }
    val operations = b.map { parseOperation(it, swapper) }

    return (constants + operations)
        .map { it.name to it }
        .toMap()
}

fun parseConstant(s: String): Definition {
    val (a, b) = words(s.replace(":", " ")).asPair()
    return Definition.Const(a, b.toLong())
}

fun parseOperation(s: String, swapper: (String) -> String): Definition {
    val (lhs, opName, rhs, arrow, name) = words(s)
    require(arrow == "->")
    val op = when (opName) {
        "AND" -> Operator.AND
        "OR" -> Operator.OR
        "XOR" -> Operator.XOR
        else -> throw IllegalArgumentException("Unknown operator: $opName")
    }
    return Definition.Operation(swapper(name), op, lhs, rhs)
}

fun evaluateAll(problem : Problem): Map<String, Long> {
    val memos: MutableMap<String, Long> = mutableMapOf()

    fun evaluate(name: String): Long =
        if (name in memos) {
            memos[name]!!
        } else {
            val definition = problem[name]!!
            val value =
                when (definition) {
                    is Definition.Const ->
                        definition.value
                    is Definition.Operation -> {
                        val lhs = evaluate(definition.lhs)
                        val rhs = evaluate(definition.rhs)
                        when (definition.op) {
                            Operator.AND -> lhs and rhs
                            Operator.OR -> lhs or rhs
                            Operator.XOR -> lhs xor rhs
                        }
                    }
                }
            memos.put(name, value)
            value
        }

    for (name in problem.keys) {
        evaluate(name)
    }

    return memos.toMap()
}

fun writeDot(problem: Problem) {
    File("day24.dot").printWriter().use { out ->
        val stdNames = problem.values
            .filterIsInstance<Definition.Operation>()
            .map { it.name to StandardName.forDefinition(it, problem) }
            .filter { it.second != null }
            .map { it.first to it.second!!.toString().lowercase()}
            .toMap()

        fun std(s: String): String = stdNames[s] ?: s

        out.println("digraph Adder {")
        for ((orig, standard) in stdNames.entries) {
            out.println("  $standard [label=\"$standard\\n$orig\"];")
        }
        for ((name, defn) in problem.entries) {
            when (defn) {
                is Definition.Operation -> {
                    val uniqueName = "${defn.op}_${defn.lhs}_${defn.rhs}"
                    out.println("  $uniqueName [label=\"${defn.op}\"];")
                    out.println("  ${std(defn.lhs)} -> ${uniqueName};")
                    out.println("  ${std(defn.rhs)} -> ${uniqueName};")
                    out.println("  ${uniqueName} -> ${std(name)};")
                }
                is Definition.Const -> {}
            }
        }
        out.println("}")
    }
}

enum class Flavor {
    X,
    Y,
    ZEE,
    ONE,   // Xnn xor Ynn
    BOTH,  // Xnn and Ynn
    CARRY, // carry out of level nn
    OAC,   // ONE(n) and CARRY(n-1)
}

data class StandardName(
    val flavor: Flavor,
    val index: Int,
) {
    companion object {
        fun forDefinition(defn: Definition, problem: Problem): StandardName? =
            when (defn) {
                is Definition.Const -> {
                    val flavor = when (defn.name.substring(0, 1)) {
                        "x" -> Flavor.X
                        "y" -> Flavor.Y
                        else -> throw IllegalArgumentException("Bad const: ${defn}")
                    }
                    val index = defn.name.substring(1).toInt()
                    StandardName(flavor, index)
                }

                is Definition.Operation -> {
                    val lhs = forDefinition(problem[defn.lhs]!!, problem)
                    val rhs = forDefinition(problem[defn.rhs]!!, problem)
                    if (lhs == null || rhs == null) {
                        null
                    } else {
                        when (defn.op) {
                            Operator.XOR -> {
                                val oneIndex = matchSameIndex(lhs, rhs, Flavor.X, Flavor.Y)
                                val zeeIndex = matchPlusOne(lhs, rhs, Flavor.CARRY, Flavor.ONE)
                                if (oneIndex != null) {
                                    StandardName(
                                        if (oneIndex == 0) Flavor.ZEE else Flavor.ONE,
                                        oneIndex
                                    )
                                } else if (zeeIndex != null) {
                                    StandardName(Flavor.ZEE, zeeIndex)
                                } else {
                                    null
                                }
                            }
                            Operator.AND -> {
                                val oneIndex = matchSameIndex(lhs, rhs, Flavor.X, Flavor.Y)
                                val oacIndex = matchPlusOne(lhs, rhs, Flavor.CARRY, Flavor.ONE)
                                if (oneIndex != null) {
                                    StandardName(
                                        if (oneIndex == 0) Flavor.CARRY else Flavor.BOTH,
                                        oneIndex
                                    )
                                } else if (oacIndex != null) {
                                    StandardName(Flavor.OAC, oacIndex)
                                } else {
                                    null
                                }
                            }
                            Operator.OR -> {
                                val carryIndex = matchSameIndex(lhs, rhs, Flavor.OAC, Flavor.BOTH)
                                if (carryIndex != null) {
                                    StandardName(Flavor.CARRY, carryIndex)
                                } else {
                                    null
                                }
                            }
                            else ->
                                null
                        }
                    }
                }
            }

        fun matchSameIndex(
            lhs: StandardName,
            rhs: StandardName,
            a: Flavor,
            b: Flavor
        ): Int? {
            if (lhs.flavor == a && rhs.flavor == b && lhs.index == rhs.index) {
                return lhs.index
            } else if (rhs.flavor == a && lhs.flavor == b && lhs.index == rhs.index) {
                return rhs.index
            } else {
                return null
            }
        }

        fun matchPlusOne(
            lhs: StandardName,
            rhs: StandardName,
            a: Flavor,
            b: Flavor
        ): Int? {
            if (lhs.flavor == a && rhs.flavor == b && rhs.index == lhs.index + 1) {
                return rhs.index
            } else if (rhs.flavor == a && lhs.flavor == b && lhs.index == rhs.index + 1) {
                return lhs.index
            } else {
                return null
            }
        }
    }

    override fun toString(): String {
        val formattedIndex = "%02d".format(index)
        return "${flavor}${formattedIndex}"
    }
}

fun <T> ident(x: T): T = x

val swaps = listOf(
    "jst" to "z05",
    "mcm" to "gdf",
    "dnt" to "z15",
    "z30" to "gwc",
)

fun swapForPart2(s: String): String {
    for ((a, b) in swaps) {
        if (s == a) return b
        if (s == b) return a
    }
    return s
}
