package net.beachfamily.aoc

data class Equation(
    val testValue: Long,
    val numbers: Sequence<Long>,
)

fun main() {
    val input = readInput("y2024d07")
    println(part1(input))
    println(part2(input))
}

fun part1(s: String) : Long =
    parse(s)
        .filter(::equationWorks)
        .map { it.testValue }
        .sum()

fun part2(s: String) : Int {
    return s.length
}

fun parse(s: String): Sequence<Equation> =
    lines(s).map { parseEquation(it) }.asSequence()

fun parseEquation(s: String): Equation {
    val (a, b) = s.split(":")
    return Equation(
        a.toLong(),
        words(b).map { it.toLong() }.asSequence()
    )
}

fun equationWorks(e: Equation): Boolean {
    return allValues(e.numbers).contains(e.testValue)
}

fun allValues(numbers: Sequence<Long>): Sequence<Long> {
    val (head, tail) = numbers.headAndTail()
    val ops = sequenceOf<(Long, Long) -> Long>(
        { a, b -> a + b },
        { a, b -> a * b },
    )
    return tail.fold(sequenceOf(head), { a, b -> tryAllOps(a, ops, b) })
}

/**
 * Returns a sequence of the results of combining each of the
 * values in the LHS input sequence with the value on the RHS
 * using each of the provided operators.
 */
fun <T> tryAllOps(
    lhs: Sequence<T>,
    ops: Sequence<(T, T) -> T>,
    rhs: T
) =
    lhs.flatMap { lhsVal -> ops.map { op -> op(lhsVal, rhs) } }