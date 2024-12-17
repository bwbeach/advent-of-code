package net.beachfamily.aoc

import java.util.*

const val EMPTY = -1

fun main() {
    val input = readInput("y2024d09")
    println(part1(input))
    println(part2(input))
}

fun part1(s: String) : Long {
    val disk = parse1(s)

    var i = 0
    var j = disk.size - 1
    while (i < j) {
        if (disk[i] != EMPTY) {
            i += 1
        } else if (disk[j] == EMPTY) {
            j -= 1
        } else {
            disk[i] = disk[j]
            disk[j] = EMPTY
            i += 1
            j -= 1
        }
    }

    return disk.withIndex()
        .filter { it.value != EMPTY }
        .map { it.value.toLong() * it.index.toLong() }
        .sum()
}

fun part2(s: String) : Long {
    val (gaps, files) = parse2(s)
    val toProcess = files.toList().reversed()
    for (f in toProcess) {
        val gap = findGap(gaps, f)
        if (gap != null) {
            println("move $f to $gap")
            gaps.remove(gap)
            gaps.add(Gap(gap.size - f.size, gap.pos + f.size))
            files.remove(f)
            files.add(File(gap.pos, f.size, f.label))
        }
    }

    return files
        .map { checksumFile(it) }
        .sum()
}

fun findGap(gaps: TreeSet<Gap>, file: File): Gap? {
    return sequence {
        for (size in file.size..9) {
            val candidate = gaps.ceiling(Gap(size, -1))
            if (candidate != null && candidate.size == size && candidate.pos < file.pos) {
                yield(candidate)
            }
        }
    }.minByOrNull() { it.pos }
}

fun checksumFile(file: File): Long =
    (0 until file.size)
        .sumOf { i ->
            (i + file.pos).toLong() * file.label.toLong()
        }

fun parse1(input: String): MutableList<Int> {
    val result = mutableListOf<Int>()
    for ((i, c) in input.withIndex()) {
        val len = Character.getNumericValue(c)
        val label = if (i % 2 == 0)  i / 2 else EMPTY
        repeat(len) { result.add(label) }
    }
    return result
}

data class Gap (
    val size: Int,
    val pos: Int,
) : Comparable<Gap> {
    override fun compareTo(other: Gap): Int {
        if (size == other.size) {
            return pos.compareTo(other.pos)
        } else {
            return size.compareTo(other.size)
        }
    }
}

data class File(
    val pos: Int,
    val size: Int,
    val label: Int,
) : Comparable<File> {
    override fun compareTo(other: File): Int {
        return pos.compareTo(other.pos)
    }
}

data class State(
    val gaps: TreeSet<Gap>,
    val files: TreeSet<File>,
)

fun parse2(input: String): State {
    val gaps = TreeSet<Gap>()
    val files = TreeSet<File>()
    var pos = 0
    for ((i, c) in input.withIndex()) {
        val size = Character.getNumericValue(c)
        if (i % 2 == 0) {
            val label = i / 2
            println("file: $pos $size $label")
            files.add(File(pos, size, label))
        } else {
            gaps.add(Gap(size, pos))
        }
        pos += size
    }
    return State(gaps, files)
}

//fun pretty(disk: List<Int>) =
//    disk
//        .map { if (it == EMPTY) "." else "${it}"}
//        .joinToString(separator = "")

