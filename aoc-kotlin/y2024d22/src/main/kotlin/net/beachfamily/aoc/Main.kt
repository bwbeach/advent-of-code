package net.beachfamily.aoc

fun main() {
    val input = readInput("y2024d22")
    println(part1(input))
    println(part2(input))
}

fun part1(s: String) : Long =
    lines(s)
        .map { it.toLong() }
        .map { firstTwoThousandSteps(it) }
        .sumOf { it.last() }

fun part2(s: String) : Long =
    lines(s)
        .asSequence()
        .map { it.toLong() }
        .map { patternToPrice(it) }
        .sum()
        .values
        .max()

fun Sequence<Map<String, Long>>.sum(): Map<String, Long> {
    val result = mutableMapOf<String, Long>()
    for (m in this) {
        for ((k, v) in m.entries) {
            result[k] = result.getOrDefault(k, 0) + v
        }
    }
    return result
}

/**
 * Builds a map from pattern to the first price that appears with that pattern.
 */
fun patternToPrice(initialSecret: Long) =
    firstTwoThousandSteps(initialSecret).patternsAndPrices().toMap()

/**
 * Returns the price for each pattern the first time it is seen.
 *
 * If a pattern occurs a second (or more) time, it's ignored.
 */
fun Sequence<Long>.patternsAndPrices() : Sequence<Pair<String, Long>> =
    sequence {
        val seen = mutableSetOf<String>()
        var a: Long? = null
        var b: Long? = null
        var c: Long? = null
        var d: Long? = null
        var e: Long? = null

        for (x in this@patternsAndPrices) {
            val price: Long = x.mod(10).toLong()
            a = b
            b = c
            c = d
            d = e
            e = price
            if (a != null) {
                val pattern = "${b!! - a} ${c!! - b} ${d!! - c} ${e - d}"
                if (pattern !in seen) {
                    seen.add(pattern)
                    yield(pattern to price)
                }
            }
        }
    }

fun firstTwoThousandSteps(start: Long): Sequence<Long> =
    generateSequence(start) { it.nextSecret() }.take(2001)

fun Long.nextSecret(): Long {
    val a = this.mix(this.shl(6))
    val b = a.mix(a.shr(5))
    return b.mix(b.shl(11))
}

fun Long.mix(b: Long): Long =
    (this.xor(b)).and(0xFFFFFF)

