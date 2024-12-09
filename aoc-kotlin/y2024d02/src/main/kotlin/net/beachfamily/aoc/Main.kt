package net.beachfamily.aoc

fun main() {
    val input = readInput("y2024d02")
    println(part1(input))
    println(part2(input))
}

fun part1(s: String): Int {
    return seqOfSeqOfInt(s).filter { isOkPart1(it) }.count()
}

fun isOkPart1(seq: Sequence<Int>) : Boolean {
    val deltas = seq.zipWithNext { a, b -> b - a }
    return deltas.all { it in 1..3 } || deltas.all { -3 <= it && it <= -1 }
}

fun part2(s: String) : Int {
    return seqOfSeqOfInt(s).filter { isOkPart2(it) }.count()
}

fun isOkPart2(seq: Sequence<Int>) : Boolean {
    val options = sequence {
        yield(seq)
        yieldAll(seq.allPossibleDrops())
    }
    return options.any { isOkPart1(it) }
}

fun <T> Sequence<T>.allPossibleDrops(): Sequence<Sequence<T>> =
    sequence {
        val seq: Sequence<T> = this@allPossibleDrops
        for (i in 0 until this@allPossibleDrops.count()) {
            yield(seq.dropNth(i))
        }
    }

fun <T> Sequence<T>.dropNth(n: Int): Sequence<T> =
    sequence {
        for ((i, v) in this@dropNth.withIndex()) {
            if (i != n) {
                yield(v)
            }
        }
    }


