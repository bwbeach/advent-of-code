package net.beachfamily.aoc

data class State(
    val items: CircularList<Int>,
    val skipLength: Int
) {
    override fun toString(): String {
        return "State(items=$items, skip=$skipLength)"
    }
}

fun initialState() = State(CircularList((0 ..< 256).toList(), 0), 0)

fun oneStep(s0: State, len: Int): State {
    val items0 = s0.items
    val items1 = items0.reverseN(len)
    val items2 = items1.advance(len + s0.skipLength)
    return State(items2, s0.skipLength + 1)
}

fun main() {
    val input = readInput("y2017d10")
    val lengths = words(input.replace(',', ' ')).map { it.toInt() }
    println(part1(lengths))
    println(part2(input))
}

fun part1(lengths: List<Int>) : Int {
    val finalState = lengths.fold(initialState()) { s, len -> oneStep(s, len) }
    return finalState.items[0] * finalState.items[1]
}

fun part2(s: String) : Int {
    return s.length
}

