package net.beachfamily.aoc

fun main() {
    val input = readInput("y2024d15")
    println(part1(input))
    println(part2(input))
}

fun part1(s: String) : Int {
    val (state, instructions) = parse(s)
    for (c in instructions) {
        state.push(c)
    }
    return state.score1()
}

fun part2(s: String) : Int {
    return s.length
}

fun parse(s: String): Pair<State, String> {
    val (a, b) = lines(s).asSequence().splitBy { it.isEmpty() }.asPair()
    val grid = gridOfChar(a.joinToString("\n"))
    val instructions = b.joinToString("").filter { !it.isWhitespace()}
    return Pair(State.create(grid), instructions)
}

class State(
    val xmin: Int,
    val xmax: Int,
    val ymin: Int,
    val ymax: Int,
    // A map containing what's where
    val contents: MutableMap<Point, Char>,
    // Where is the robot?
    var robotPos: Point,
) {
    companion object {
        fun create(grid: Grid<Char>): State =
            State(
                grid.xmin,
                grid.xmax,
                grid.ymin,
                grid.ymax,
                grid.data
                    .toMutableMap(),
                grid.data.entries
                    .filter { it.value == '@'}
                    .map { it.key }
                    .first()
            )
    }

    fun push(instr: Char) {
        val moves = setToMove(instr)
        if (moves != null) {
            val dir = instructionToDir(instr)
            // pick up all the things from their old locations, leaving them blank
            for ((p, c) in moves) {
                contents[p] = '.'
            }
            // drop them in their new locations
            for ((p, c) in moves) {
                require(contents[p + dir] == '.')
                contents[p + dir] = c
            }
            robotPos += instructionToDir(instr)
        }
    }

    /**
     * Returns the set of locations of things that will move or null.
     *
     * Starts with the robot and includes everything it will push.
     * Checks that there is empty space to move into.  If anything
     * that wants to move doesn't have space, returns null.
     */
    fun setToMove(instr: Char): Map<Point, Char>? {
        val dir = instructionToDir(instr)
        return buildMoves(findThingsThatMove(robotPos, dir))
    }

    /**
     * Given a sequence of Point?, returns null if any are null, and a
     * set of them all otherwise.
     */
    fun buildMoves(seq: Sequence<Point?>): Map<Point, Char>? {
        val result = mutableMapOf<Point, Char>()
        for (p in seq) {
            if (p == null) {
                return null
            } else {
                result[p] = contents[p]!!
            }
        }
        return result.toMap()
    }

    /**
     * Finds all of the things that move when the given thing (if there is one)
     * at the given pos moves in the given direction.  Produces null if the
     * thing at the given position cannot move.
     */
    fun findThingsThatMove(pos: Point, dir: Point): Sequence<Point?> =
        sequence {
            when (contents[pos]) {
                '#' -> yield(null)
                '.' -> {}
                '@', 'O' -> {
                    yield(pos)
                    yieldAll(findThingsThatMove(pos + dir, dir))
                }
            }
        }

    fun score1(): Int {
        return contents.entries
            .filter { it.value == 'O' }
            .map { it.key.gps() }
            .sum()
    }

    override fun toString(): String =
        buildString {
            for (y in ymin..ymax) {
                for (x in xmin..xmax) {
                    append(contents[Point(x, y)])
                }
                appendLine()
            }
        }
}

fun instructionToDir(c: Char): Point =
    when (c) {
        '<' -> Point(-1, 0)
        '>' -> Point(1, 0)
        '^' -> Point(0, -1)
        'v' -> Point(0, 1)
        else -> throw IllegalArgumentException("Bad instruction: $c")
    }

fun Point.gps(): Int {
    return x + y * 100
}