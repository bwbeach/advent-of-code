package net.beachfamily.aoc

import java.nio.file.Files
import java.nio.file.Paths

fun readInput(name: String) : String {
    val dir = Paths.get("/Users/brianb/sandbox/advent-of-code/problems")  // TODO: use env var
    val path = dir.resolve(name)
    return Files.readAllBytes(path).decodeToString()
}


/**
 * Returns a Sequence of all possible pairs of items chosen from the given list.
 * Each pair contains two distinct items from the list, in the same order they
 * appear in the list.
 */
fun <T> allPairs(list: List<T>): Sequence<Pair<T, T>> = sequence {
    for (i in list.indices) {
        for (j in (i + 1) ..< list.size) {
            yield(Pair(list[i], list[j]))
        }
    }
}

/**
 * Same as `allPairs`, but every pair is returned in both orders.
 */
fun <T> allPairsBothOrders(list: List<T>): Sequence<Pair<T, T>> = sequence {
    for ((first, second) in allPairs(list)) {
        yield(Pair(first, second))
        yield(Pair(second, first))
    }
}

/**
 * If the input Sequence contains exactly one item, returns that item.
 * In all other cases, throws an exception.
 */
fun <T> singleItem(sequence: Sequence<T>): T {
    val iterator = sequence.iterator()
    if (!iterator.hasNext()) throw IllegalArgumentException("Sequence is empty")
    val singleItem = iterator.next()
    if (iterator.hasNext()) throw IllegalArgumentException("Sequence contains more than one item")
    return singleItem
}

/**
 * Splits the input string at newlines and returns a list of all non-blank lines.
 *
 * @param s Input string
 * @return A list of non-blank lines from the input string.
 */
fun lines(s: String) : List<String> =
    s.split('\n').filter { it.isNotBlank() }

/**
 * Splits the input string into words, where words are defined as sequences of non-whitespace
 * characters separated by sequences of whitespace characters. Only non-blank words are retained.
 * Words include any non-whitespace characters, including punctuation.
 *
 * @param s Input string to be processed.
 * @return A list of non-blank words from the input string.
 */
fun words(s: String) : List<String> =
    s.split(Regex("\\s+")).filter { it.isNotBlank() }

/**
 * Transposes a sequence of sequences.
 * 
 * The first item in the returned sequence is a sequence of the first item
 * in each of the input sequences.  The length of the output sequence is
 * the minimum of the lengths of the input sequences.
 */
fun <T> transpose(input: Sequence<Sequence<T>>): Sequence<Sequence<T>> = sequence {
    // Get iterators for each sequence in the input
    val iterators = input.map { it.iterator() }.toList()

    // Loop until any of the iterators has no next element
    while (iterators.all { it.hasNext() }) {
        // Yield a sequence from the next elements of each iterator
        yield(iterators.map { it.next() }.asSequence())
    }
}