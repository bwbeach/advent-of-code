package net.beachfamily.aoc

fun main() {
    val input = readInput("y2024d24")
    println(part1(input))
    println(part2(input))
}

fun part1(s: String) : Long {
    val problem = parse(s)
    val values = evaluateAll(problem)
    val zees = values.keys.filter { it.startsWith("z") }
    return zees
        .sorted()
        .reversed()
        .map { values[it]!! }
        .fold(0) { acc, i -> (acc shl 1) or i }
}

fun part2(s: String) : Int {
    return s.length
}

enum class Operator {
    AND,
    OR,
    XOR,
}

sealed class Definition() {
    class Const(val value: Long) : Definition()
    class Operation(
        val op: Operator,
        val lhs: String,
        val rhs: String,
    ) : Definition()
}

typealias Problem = Map<String, Definition>

fun parse(s: String): Problem {
    val (a, b) = lines(s).asSequence().splitBy { it.isBlank() }.asPair()
    val constants = a.map { parseConstant(it) }
    val operations = b.map { parseOperation(it) }

    return (constants + operations).toMap()
}

fun parseConstant(s: String): Pair<String, Definition> {
    val (a, b) = words(s.replace(":", " ")).asPair()
    return a to Definition.Const(b.toLong())
}

fun parseOperation(s: String): Pair<String, Definition> {
    val (lhs, opName, rhs, arrow, name) = words(s)
    val op = when (opName) {
        "AND" -> Operator.AND
        "OR" -> Operator.OR
        "XOR" -> Operator.XOR
        else -> throw IllegalArgumentException("Unknown operator: $opName")
    }
    return name to Definition.Operation(op, lhs, rhs)
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
