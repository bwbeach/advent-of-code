package net.beachfamily.aoc

data class State(
    val items: CircularList<Int>,
    val skipLength: Int
) {
    override fun toString(): String {
        return "State(items=$items, skip=$skipLength)"
    }
}

fun initialState(size: Int) =
    State(CircularList((0 ..< size).toList(), 0), 0)

fun oneStep(s0: State, len: Int): State {
    val items0 = s0.items
    val items1 = items0.reverseN(len)
    val items2 = items1.advance(len + s0.skipLength)
    return State(items2, s0.skipLength + 1)
}

fun makePart2Lengths(input: String): List<Int> {
    return input.trim().map { it.code } + listOf(17, 31, 73, 47, 23)
}

fun main() {
    val input = readInput("y2017d10").trim()
    println(part1(256, input))
    println(part2(input))
}

fun part1(size: Int, input: String) : Int {
    val lengths = words(input.replace(',', ' ')).map { it.toInt() }
    val finalState = process(size, lengths.asSequence())
    return finalState.items.items[0] * finalState.items.items[1]
}

fun part2(input: String) : String {
    val lengths = makePart2Lengths(input)
    val repeatedLengths = sequence { repeat(64) { yieldAll(lengths) } }
    val finalState = process(256, repeatedLengths)
    val dense = denseHash(finalState.items.items)
    val final = dense.joinToString("") { it.toString(16).padStart(2, '0') }
    return final
}

fun process(size: Int, lengths: Sequence<Int>): State {
    return lengths.fold(initialState(size)) { s, len -> oneStep(s, len) }
}

/**
 * Turns a list of 256 ints into a list of 16 ints by xor-ing groups of 16 together
 */
fun denseHash(sparseHash: List<Int>): List<Int> {
    require(sparseHash.size == 256)
    return sparseHash.chunked(16).map { it.reduce { a, b -> a xor b } }
}
