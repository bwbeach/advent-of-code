package net.beachfamily.aoc

import com.google.common.collect.ImmutableMultimap

data class Input (
    val constraints: Sequence<Pair<Int, Int>>,
    val books: Sequence<Sequence<Int>>
)

fun main() {
    val input = readInput("y2024d05")
    println(part1(input))
    println(part2(input))
}

fun part1(s: String) : Int {
    val input = parse(s)
    val mapToBefore = input.constraints.map { it.swap() }.toSetMultimap()
    return input.books.filter { isBookOk(it, mapToBefore) }.sumOf { it.toList().middle() }
}

fun part2(s: String) : Int {
    val input = parse(s)
    val mapToBefore = input.constraints.map { it.swap() }.toSetMultimap()
    return input.books
        .filter { ! isBookOk(it, mapToBefore) }
        .map { it.toList().sortedWith(Comparator { a, b -> comparePages(a, b, mapToBefore) }) }
        .sumOf { it.toList().middle() }
}

fun comparePages(a: Int, b: Int, mapToBefore: ImmutableMultimap<Int, Int>) : Int {
    val beforeA = mapToBefore[a]
    if (beforeA != null && b in beforeA) {
        return -1
    }
    val beforeB = mapToBefore[b]
    if (beforeB != null && a in beforeB) {
        return 1
    }
    throw IllegalArgumentException("No mapping for $a or $b")
}

fun isBookOk(book: Sequence<Int>, mapToBefore: ImmutableMultimap<Int, Int>) : Boolean {
    val isDisallowed = mutableSetOf<Int>()
    for (i in book) {
        if (i in isDisallowed) {
            return false
        }
        if (mapToBefore.containsKey(i)) {
            isDisallowed.addAll(mapToBefore[i])
        }
    }
    return true
}

fun <T> List<T>.middle() : T {
    require(size % 2 == 1) { "Size must be odd" }
    return this[size / 2]
}

fun parse(s: String) : Input {
    val (lines1, lines2) = lines(s)
        .asSequence()
        .splitBy { it.isBlank() }
        .asPair()
    return Input(
        lines1.map { parseConstraint(it) },
        lines2.map { parseBook(it) }
    )
}

fun <A, B> Pair<A, B>.swap() : Pair<B, A> {
    return second to first
}

fun parseConstraint(s: String) : Pair<Int, Int> {
    val (a, b) = s.split("|").map { it.toInt() }
    return a to b
}

fun parseBook(s: String) : Sequence<Int> {
    return s.split(",").map { it.toInt() }.asSequence()
}

