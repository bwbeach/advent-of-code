package net.beachfamily.aoc

data class Node(
    val name: String,
    val weight: Int,
    val upperNeighbors: List<String>
)

/**
 * Parse one input line to create a `Node`.
 *
 * Written by AI Assistant.  Prompt was something like:
 * Parse a string to create a node.  String contains name, weight,
 * and a list of neighbors.  Examples are: ...
 */
fun parseLine(line: String): Node {
    val regex = Regex("""(\w+) \((\d+)\)( -> (.+))?""")
    val matchResult = regex.matchEntire(line)

    return if (matchResult != null) {
        val (name, weight, _, neighbors) = matchResult.destructured
        val upperNeighbors = neighbors.split(", ").filter { it.isNotBlank() }
        Node(name, weight.toInt(), upperNeighbors)
    } else {
        throw IllegalArgumentException("Line format is incorrect: $line")
    }
}

fun main() {
    val input = readInput("y2017d07")
    val nodes = lines(input).map { parseLine(it) }
    println(part1(nodes))
    println(part2(input))
}

fun part1(nodes: List<Node>) : String {
    val allNames = nodes.map { it.name }.toSet()
    val allUpper = nodes.map { it.upperNeighbors }.flatten().toSet()
    val allRoots = allNames - allUpper
    return singleItem(allRoots.asSequence())
}

fun part2(s: String) : Int {
    return s.length
}

