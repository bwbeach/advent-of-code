package net.beachfamily.aoc

sealed class Item {
    data class Garbage(val text: String) : Item()
    data class Group(val items: List<Item>) : Item()
}

/**
 * Parses one item of garbage.  Assumes leading '<' has already been consumed.
 */
fun parseGarbage(input: Iterator<Char>): Item {
    val result = StringBuilder()
    while (true) {
        val c = input.next()
        if (c == '!') {
            input.next()
        } else if (c == '>') {
            return Item.Garbage(result.toString())
        } else {
            result.append(c)
        }
    }
}

/**
 * Parse one group.  Assumes the leading '{' has already been consumed.
 */
fun parseGroup(input: Iterator<Char>): Item {
    val items = mutableListOf<Item>()
    while (true) {
        val c = input.next()
        if (c == '}') {
            return Item.Group(items)
        } else if (c == '<') {
            items.add(parseGarbage(input))
        } else if (c == '{') {
            items.add(parseGroup(input))
        }
        val t = input.next()
        if (t == '}') {
            return Item.Group(items)
        } else {
            if (t != ',') {
                println("AHA!")
            }
            require(t == ',')
        }
    }
}

fun countGroups(item: Item): Int {
    when (item) {
        is Item.Garbage -> return 0
        is Item.Group -> return 1 + item.items.map { countGroups(it) }.sum()
    }
}

fun score(item: Item, depth: Int = 1): Int {
    when (item) {
        is Item.Garbage -> return 0
        is Item.Group -> return depth + item.items.map { score(it, depth + 1) }.sum()
    }
}

fun score2(item: Item): Int {
    when (item) {
        is Item.Garbage -> return item.text.length
        is Item.Group -> return item.items.map { score2(it) }.sum()
    }
}

fun main() {
    val input = readInput("y2017d09").trim()
    println(part1(input))
    println(part1b(input))
    println(part2(input))
    println(part2b(input))
}

fun part1(s: String) : Int {
    val iter = s.iterator()
    require(iter.next() == '{')
    val item = parseGroup(iter)
    return score(item)
}

fun part2(s: String) : Int {
    val iter = s.iterator()
    require(iter.next() == '{')
    val item = parseGroup(iter)
    return score2(item)
}

enum class State {
    GROUP,
    GARBAGE,
    DELETE,
}

data class FullState(
    val state: State,
    val depth: Int,
    val score: Int,
    val score2: Int,
) {
    fun next(c: Char): FullState {
        return when (state) {
            State.GROUP ->
                when (c) {
                    '{' -> FullState(State.GROUP, depth + 1, score + depth + 1, score2)
                    '}' -> {
                        require(0 < depth)
                        FullState(State.GROUP, depth - 1, score, score2)
                    }
                    ',' -> this
                    '<' -> FullState(State.GARBAGE, depth, score, score2)
                    else -> throw IllegalArgumentException("Invalid char '$c' in state $this")
                }
            State.GARBAGE -> {
                when (c) {
                    '!' -> FullState(State.DELETE, depth, score, score2)
                    '>' -> FullState(State.GROUP, depth, score, score2)
                    else -> FullState(State.GARBAGE, depth, score, score2 + 1)
                }
            }
            State.DELETE ->
                FullState(State.GARBAGE, depth, score, score2)
        }
    }
}

fun initialState(): FullState = FullState(State.GROUP, 0, 0, 0)

fun part1b(s: String): Int {
    var state = initialState()
    for (c in s) {
        state = state.next(c)
    }
    return state.score
}

fun part2b(s: String): Int {
    var state = initialState()
    for (c in s) {
        state = state.next(c)
    }
    return state.score2
}