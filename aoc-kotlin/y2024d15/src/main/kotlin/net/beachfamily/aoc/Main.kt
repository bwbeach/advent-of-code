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

//    fun push(instr: Char) = pushFrom(robotPos, instructionToDir(instr))

    fun pushFrom(pos: Point, dir: Point) {
        val next = pos + dir
        val what = contents[next]!!

        // can't push into a wall
        if (what == '#') {
            return
        }

        // try pushing non-empty
        if (what != '.') {
            pushFrom(next, dir)
        }

        // if there was already a space, or we just made one, we can
        // push what's in this spot
        if (contents[next] == '.') {
            contents[next] = contents[pos]!!
            contents[pos] = '.'
            if (pos == robotPos) {
                robotPos = next
            }
        }
    }

    fun push(instr: Char) {
        val moves = planMove(instr)
        if (moves != null) {
            for (m in moves) {
                require(contents[m.dst] == '.')
                contents[m.dst] = contents[m.src]!!
                contents[m.src] = '.'
            }
            robotPos += instructionToDir(instr)
        }
    }

    fun planMove(instr: Char): List<Move>? {
        val dir = instructionToDir(instr)
        return planMoveFrom(robotPos, dir)
    }

    fun planMoveFrom(pos: Point, dir: Point): List<Move>? {
        val what = contents[pos]!!
        return when (what) {
            '#' -> null
            '.' -> listOf()
            '@', 'O' -> {
                val rest = planMoveFrom(pos + dir, dir)
                when (rest) {
                    null -> null
                    else -> rest + listOf(Move(pos, pos + dir))
                }
            }
            else -> throw IllegalArgumentException("Bad state: $what")
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

data class Move(
    val src: Point,
    val dst: Point,
)

fun Point.gps(): Int {
    return x + y * 100
}