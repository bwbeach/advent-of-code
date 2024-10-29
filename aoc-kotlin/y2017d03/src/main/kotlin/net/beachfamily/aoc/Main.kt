package net.beachfamily.aoc

import kotlin.math.abs

fun main() {
    val input = readInput("y2017d03")
    val address = singleItem(words(input).asSequence()).toInt()
    
    println(part1(address))
    println(part2(address))
}

fun part1(a: Int) : Int {
    val (x, y) = xAndY(a)
    return abs(x) + abs(y)
}

fun part2(a: Int) : Int {
    val grid : MutableMap<Pair<Int, Int>, Int> = HashMap()
    val origin = Pair(0, 0)
    grid[origin] = 1

    val addresses = generateSequence(2) { it + 1 }
    for (n in addresses) {
        val p = xAndY(n)
        val value = neighbors(p).mapNotNull { grid[it] }.sum()
        grid[p] = value
        if (a < value) {
            return value
        }
    }

    throw RuntimeException("Infinite sequence ended")
}

/**
 * Returns all the neighbors of the given point, including diagonals.
 */
fun neighbors(p: Pair<Int, Int>) : Sequence<Pair<Int, Int>> {
    val (x, y) = p
    return sequenceOf(
        Pair(x - 1, y - 1),
        Pair(x - 1, y),
        Pair(x - 1, y + 1),
        Pair(x, y - 1),
        Pair(x, y + 1),
        Pair(x + 1, y - 1),
        Pair(x + 1, y),
        Pair(x + 1, y + 1),
    )
}

/**
 * Returns the (x, y) coordinates of the given address.
 */
fun xAndY(a: Int) : Pair<Int, Int> {
    if (a == 1) {
        return Pair(0, 0)
    } else {
        val inner = sizeOfInner(a)
        val first = inner * inner + 1
        val edge = inner + 2
        return Pair(x(a, first, edge), y(a, first, edge))
    }
}

fun x(a: Int, first: Int, edge: Int) : Int {
    // `n` is how many squares, counterclockwise, we are around the outer
    // square, with 0 at the bottom right.
    val n = a - first + 1
    // `r` is the "radius" of the square.  The range of both x and
    // y is [-r, r].
    val r = edge / 2  // edge is odd; rounds down
    // The number of new squares on each edge is the edge length minus one,
    // because the corners count only once, not on both edges they participate
    // in.
    val edgeDelta = edge - 1

    return when (n / edgeDelta) {
        0 -> r
        1 -> edgeDelta * 2 - n - r
        2 -> -r
        3 -> n - edgeDelta * 3 - r
        4 -> r
        else -> throw IllegalArgumentException("Invalid address $a when first=$first and edge=$edge")
    }
}

fun y(a: Int, first: Int, edge: Int) : Int {
    // `n` is how many squares, counterclockwise, we are around the outer
    // square, with 0 at the bottom right.
    val n = a - first + 1
    // `r` is the "radius" of the square.  The range of both x and
    // y is [-r, r].
    val r = edge / 2  // edge is odd; rounds down
    // The number of new squares on each edge is the edge length minus one,
    // because the corners count only once, not on both edges they participate
    // in.
    val edgeDelta = edge - 1

    return when (n / edgeDelta) {
        0 -> n - r
        1 -> r
        2 -> edgeDelta * 3 - n - r
        3 -> -r
        4 -> -r
        else -> throw IllegalArgumentException("Invalid address $a when first=$first and edge=$edge")
    }
}

/**
 * Returns the largest odd n, such that n*n < a
 */
fun sizeOfInner(a: Int) : Int {
    var n = 1
    while ((n + 2) * (n + 2) < a) {
        n += 2
    }
    return n
}