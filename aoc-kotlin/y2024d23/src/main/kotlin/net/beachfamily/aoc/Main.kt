package net.beachfamily.aoc

import com.google.common.collect.HashMultimap
import com.google.common.collect.Multimap

fun main() {
    val input = readInput("y2024d23")
    println(part1(input))
    println(part2(input))
}

fun part1(s: String) : Int {
    val problem = parse(s)
    return allTriples(problem)
        .filter { it.any { c -> c.startsWith("t") } }
        .count()
}

fun part2(s: String) : String {
    val problem = parse(s)
    return allFullyConnected(problem)
        .maxBy { it.size }
        .toList()
        .sorted()
        .joinToString(",")
}

data class Problem(
    // The names of all computers on the network.
    val computers: Set<String>,
    // Connections between computers.  There are two entries for each connection,
    // one in each direction.
    val connections: Multimap<String, String>,
)

fun parse(s: String): Problem {
    val links = lines(s).map { it.split("-") }.map { Pair(it[0], it[1]) }
    val connections: Multimap<String, String> = HashMultimap.create()
    for ((a, b) in links) {
        connections.put(a, b)
        connections.put(b, a)
    }
    return Problem(connections.keys().toSet(), connections)
}

fun allTriples(problem: Problem): Sequence<List<String>> =
    sequence {
        val computers = problem.computers.toList().sorted()
        for (i in 0 ..< computers.size) {
            val a = computers[i]
            for (j in i + 1 ..< computers.size) {
                val b = computers[j]
                if (problem.connections.containsEntry(a, b)) {
                    for (k in j + 1 ..< computers.size) {
                        val c = computers[k]
                        if (problem.connections.containsEntry(b, c) && problem.connections.containsEntry(a, c)) {
                            yield(listOf(a, b, c))
                        }
                    }
                }
            }
        }
    }

fun allFullyConnected(problem: Problem): Sequence<Set<String>> {
    val computers = problem.computers.toList().sorted()
    val connections = problem.connections

    fun search(soFar: Set<String>, start: Int): Sequence<Set<String>> =
        sequence {
            if (3 <= soFar.size) {
                yield(soFar)
            }
            for (i in (start..<computers.size)) {
                val c = computers[i]
                if (soFar.all { connections.containsEntry(it, c) }) {
                    yieldAll(search(soFar + setOf(c), i + 1))
                }
            }
        }

    return search(setOf(), 0)
}
