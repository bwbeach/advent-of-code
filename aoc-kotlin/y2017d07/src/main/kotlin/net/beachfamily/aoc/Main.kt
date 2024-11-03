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

    private val isBalanced: Boolean by lazy {
        if (upperNeighbors.isEmpty()) {
            true
        } else {
            val firstWeight = upperNeighbors[0].totalWeight
            upperNeighbors.all { it.totalWeight == firstWeight }
        }
    }

    /**
     * The node we're looking for in part1 is the one that is not balanced,
     * but whose upper neighbors are all balanced.
     */
    val isTheOne: Boolean by lazy {
        ! isBalanced && upperNeighbors.all { it.isBalanced }
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
    val nameToNode = makeNodes(nameToRawNode)
    val nodes = nameToNode.values
    val theOne = singleItem(nodes.asSequence().filter { it.isTheOne })
    require(2 < theOne.upperNeighbors.size)
    val goodWeight = mode(theOne.upperNeighbors.asSequence().map {it.totalWeight})
    val badNode = singleItem(theOne.upperNeighbors.asSequence().filter( { it.totalWeight != goodWeight }))
    return badNode.weight + (goodWeight - badNode.totalWeight)
}

fun makeNodes(rawNodes: Map<String, RawNode>): Map<String, Node> {
    val workingMap = mutableMapOf<String, Node>()
    for (entry in rawNodes) {
        getOrMakeNode(entry.key, workingMap, rawNodes)
    }
    return workingMap
}

fun getOrMakeNode(name: String, workingMap: MutableMap<String, Node>, rawNodes: Map<String, RawNode>): Node {
    return workingMap.computeIfAbsent(
        name,
        {
            val raw = rawNodes[it]!!
            Node(name, raw.weight, raw.upperNeighbors.map { getOrMakeNode(it, workingMap, rawNodes) })
        }
    )
}

/**
 * Returns the mode, the most common value, in the given Sequence.
 */
fun <T> mode(items: Sequence<T>): T {
    return items.groupBy { it }.maxBy { it.value.size }.key
}