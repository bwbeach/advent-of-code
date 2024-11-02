package net.beachfamily.aoc

data class RawNode(
    val name: String,
    val weight: Int,
    val upperNeighbors: List<String>
)

data class Node(
    val name: String,
    val weight: Int,
    val upperNeighbors: List<Node>
) {
    val totalWeight: Int by lazy {
        weight + upperNeighbors.sumOf { it.totalWeight }
    }

    val isBalanced: Boolean by lazy {
        if (upperNeighbors.size == 0) {
            true
        } else {
            val firstWeight = upperNeighbors[0].totalWeight
            upperNeighbors.all { it.totalWeight == firstWeight }
        }
    }
}

/**
 * Parse one input line to create a `Node`.
 *
 * Written by AI Assistant.  Prompt was something like:
 * Parse a string to create a node.  String contains name, weight,
 * and a list of neighbors.  Examples are: ...
 */
fun parseLine(line: String): RawNode {
    val regex = Regex("""(\w+) \((\d+)\)( -> (.+))?""")
    val matchResult = regex.matchEntire(line)

    return if (matchResult != null) {
        val (name, weight, _, neighbors) = matchResult.destructured
        val upperNeighbors = neighbors.split(", ").filter { it.isNotBlank() }
        RawNode(name, weight.toInt(), upperNeighbors)
    } else {
        throw IllegalArgumentException("Line format is incorrect: $line")
    }
}

fun main() {
    val input = readInput("y2017d07")
    val rawNodes = lines(input).map { parseLine(it) }
    println(part1(rawNodes))
    println(part2(rawNodes))
}

fun part1(rawNodes: List<RawNode>) : String {
    val allNames = rawNodes.map { it.name }.toSet()
    val allUpper = rawNodes.map { it.upperNeighbors }.flatten().toSet()
    val allRoots = allNames - allUpper
    return singleItem(allRoots.asSequence())
}

fun part2(rawNodes: List<RawNode>) : Int {
    val nameToRawNode = rawNodes.associateBy { it.name }
    val rootName = part1(rawNodes)
    val root = makeNode(rootName, nameToRawNode)
    return rawNodes.size
}

fun makeNode(name: String, rawNodes: Map<String, RawNode>): Node {
    val rawNode = rawNodes[name]!!
    val upperNeighbors = rawNode.upperNeighbors.map { makeNode(it, rawNodes) }
    return Node(name, rawNode.weight, upperNeighbors)
}

/**
 * Returns the mode, the most common value, in the given Sequence.
 */
fun <T> mode(items: Sequence<T>): T {
    return items.groupBy { it }.maxBy { it.value.size }.key
}