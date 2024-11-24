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
                    '{' -> copy(state = State.GROUP, depth = depth + 1, score = score + depth + 1)
                    '}' -> {
                        require(0 < depth)
                        copy(depth = depth - 1)
                    }
                    ',' -> this
                    '<' -> copy(state = State.GARBAGE)
                    else -> throw IllegalArgumentException("Invalid char '$c' in state $this")
                }
            State.GARBAGE -> {
                when (c) {
                    '!' -> copy(state = State.DELETE)
                    '>' -> copy(state = State.GROUP)
                    else -> copy(score2 = score2 + 1)
                }
            }
            State.DELETE ->
                copy(state = State.GARBAGE)
        }
    }
}

fun initialState(): FullState = FullState(State.GROUP, 0, 0, 0)

fun part1b(s: String): Int {
    return finalState(s).score
}

private fun finalState(s: String): FullState {
    return s.fold(initialState()) { state, c -> state.next(c) }
}

fun part2b(s: String): Int {
    kreturn finalState(s).score2
}