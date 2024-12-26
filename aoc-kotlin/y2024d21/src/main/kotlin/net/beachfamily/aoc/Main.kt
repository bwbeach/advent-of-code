package net.beachfamily.aoc

const val NUMBER_PAD =
"""
+---+---+---+
| 7 | 8 | 9 |
+---+---+---+
| 4 | 5 | 6 |
+---+---+---+
| 1 | 2 | 3 |
+---+---+---+
    | 0 | A |
    +---+---+
"""

const val DIR_PAD =
"""
    +---+---+
    | ^ | A |
+---+---+---+
| < | v | > |
+---+---+---+
"""

fun main() {
    val input = readInput("y2024d21")
    println(part1(input))
    println(part2(input))
}

fun part1(s: String) : Int {
    val numberPad = Keypad.parse(NUMBER_PAD)
    val dirPad = Keypad.parse(DIR_PAD)

    var result = 0
    for (line in lines(s)) {
        val encoded = shortestEncoding(line, numberPad, 2, dirPad)
        println("$line\n$encoded")
        require(encoded.decode(dirPad).decode(dirPad).decode(numberPad) == line)
        val numericPart = line.substring(0, line.length - 1).toInt()
        println("${encoded.length} * $numericPart")
        result += numericPart * encoded.length
    }
    return result
}

fun part2(s: String) : Int {
    return s.length
}

/**
 * Returns the shortest encoding for the given string with the given keypad,
 * plus n more directional keypads.
 */
fun shortestEncoding(
    // String to encode
    s: String,
    // Keypad to use
    keypad: Keypad,
    // How many more encodings?
    plusN: Int,
    // What keypad to use for more encodings?
    plusKeypad: Keypad,
): String =
    s.splitAfterA()
        .map { shortestEncodingOfOne(it, keypad, plusN, plusKeypad) }
        .joinToString("")

/**
 * Returns the shortest encoding for the given string with the given keypad,
 * plus n more directional keypads.
 *
 * Requires that the string have a single "A" that appears at the end.
 */
fun shortestEncodingOfOne(
    // String to encode
    s: String,
    // Keypad to use
    keypad: Keypad,
    // How many more encodings?
    plusN: Int,
    // What keypad to use for more encodings?
    plusKeypad: Keypad,
): String =
    s.allEncodings(keypad)
        .map {
            if (plusN == 0)
                it
            else
                shortestEncoding(it, plusKeypad, plusN - 1, plusKeypad)
        }
        .minBy { it.length }

/**
 * A keypad that can be used to encode a string.
 */
data class Keypad(
    val charToPos: Map<Char, Point>,
    val posToChar: Map<Point, Char>,
) {
    companion object {
        fun parse(s: String): Keypad {
            val grid = gridOfChar(s)
            val charToPos = mutableMapOf<Char, Point>()
            val posToChar = mutableMapOf<Point, Char>()
            for (x in 2 until grid.xmax step 4) {
                for (y in 1 until grid.ymax step 2) {
                    val c = grid[Point(x, y)]!!
                    if (c != ' ') {
                        val p = Point(x / 4, y / 2)
                        charToPos[c] = p
                        posToChar[p] = c
                    }
                }
            }
            return Keypad(charToPos.toMap(), posToChar.toMap())
        }
    }
}

/**
 * Makes a list of all substrings that end in "A"
 */
fun String.splitAfterA(): Sequence<String> =
    sequence {
        val s = this@splitAfterA
        var start = 0
        while (start < s.length) {
            val aPos = s.indexOf('A', start)
            require(0 < aPos)
            yield(s.substring(start, aPos + 1))
            start = aPos + 1
        }
    }

/**
 * Returns all possible encodings for a string with the given keypad.
 * 
 * Assumes that the position starts at "A"
 */
fun String.allEncodings(keypad: Keypad): Sequence<String> {
    val allChoices = sequence {
        var pos = keypad.charToPos['A']!!
        for (c in this@allEncodings) {
            val forChar = allEncodingsForChar(c, pos, keypad)
            yield(forChar)
            pos = keypad.charToPos[c]!!
        }
    }
    return crossProduct(allChoices).map { it.joinToString("") }
}

/**
 * Returns all the possible encodings for a single character,
 * starting at the given position.
 */
fun allEncodingsForChar(c: Char, pos: Point, keypad: Keypad): Sequence<String> =
    sequence {
        val target = keypad.charToPos[c]!!
        val (dx, dy) = target - pos
        val h =
            if (dx < 0)
                "<".repeat(-dx)
            else if (dx > 0)
                ">".repeat(dx)
            else
                ""

        val v =
            if (dy < 0)
                "^".repeat(-dy)
            else if (dy > 0)
                "v".repeat(dy)
            else
                ""

        if (move(pos, h + v, keypad.posToChar) == target) {
            yield(h + v + "A")
        }

        if (move(pos, v + h, keypad.posToChar) == target && h.isNotEmpty() && v.isNotEmpty()) {
            yield(v + h + "A")
        }
    }

/**
 * Given a sequence of sequences, returns a sequence of all combinations
 * choosing one item from each sequence.
 */
fun <T> crossProduct(listOfChoices: Sequence<Sequence<T>>): Sequence<Sequence<T>> =
    if (!listOfChoices.iterator().hasNext()) {
        sequenceOf(sequenceOf())
    } else {
        sequence {
            val (first, rest) = listOfChoices.headAndTail()
            val restCrossProduct = crossProduct(rest)
            for (f in first) {
                for (rcp in restCrossProduct) {
                    yield(
                        sequence {
                            yield(f)
                            yieldAll(rcp)
                        }
                    )
                }
            }
        }
    }

/**
 * Encode robot commands to produce the given sequence on the given keypad.
 */
fun String.encode(keypad: Map<Char, Point>, vFirst: Boolean = false): String =
    buildString {
        val inverted = keypad.entries.associateBy({ it.value }) { it.key }.toMap()
        require(this@encode.last() == 'A')
        var pos = keypad['A']!!
        for (c in this@encode) {
            val target = keypad[c]!!
            val (dx, dy) = (target - pos)
            val h =
                if (dx < 0)
                    "<".repeat(-dx)
                else if (dx > 0)
                    ">".repeat(dx)
                else
                    ""
            val v =
                if (dy < 0)
                    "^".repeat(-dy)
                else if (dy > 0)
                    "v".repeat(dy)
                else
                    ""
            if (vFirst && move(pos, v + h, inverted) == target) {
                append(v)
                append(h)
            }
            else if (move(pos, h + v, inverted) == target) {
                append(h)
                append(v)
            }
            else if (move(pos, v + h, inverted) == target) {
                append(v)
                append(h)
            }
            append("A")
            pos = target
        }
    }

/**
 * Play the given commands on the given keypad.
 */
fun String.decode(keypad: Keypad): String =
    buildString {
        val posToChar = keypad.posToChar
        var pos = keypad.charToPos['A']!!
        for (c in this@decode) {
            when (c) {
                '<' -> pos -= Point(1, 0)
                '>' -> pos += Point(1, 0)
                '^' -> pos -= Point(0, 1)
                'v' -> pos += Point(0, 1)
                'A' -> append(posToChar[pos]!!)
            }
            require(posToChar[pos] != null, {"Navigated to blank"})
        }
    }

fun move(startPos: Point, commands: String, inverted: Map<Point, Char>): Point? {
    var pos = startPos
    for (c in commands) {
        when (c) {
            '<' -> pos -= Point(1, 0)
            '>' -> pos += Point(1, 0)
            '^' -> pos -= Point(0, 1)
            'v' -> pos += Point(0, 1)
        }
        if (inverted[pos] == null) {
            return null
        }
    }
    return pos
}

