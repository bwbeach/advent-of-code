package net.beachfamily.aoc

/**
 * A circular list of things of type T, with one position designated as "current".
 *
 * The list is directional.  From the current position, there is "forward" and "backward".
 */
data class CircularList<T>(
    /**
     * The items in the list.  Forward is increasing indices.
     */
    val items : List<T>,

    /**
     * The item that is current.
     *
     * Invariant: 0 <= current < items.size()
     */
    val current : Int,
){

    /**
     * Two lists are equal if items at the same indices are equal, even though
     * the representation in `items` may be shifted.
     */
    override fun equals(other: Any?): Boolean {
        if (this === other) return true
        if (other !is CircularList<*>) return false
        if (items.size != other.items.size) return false

        return (0 ..< items.size).all {
            this[it] == other[it]
        }
    }

    /**
     * Override hashing to match equals().
     *
     * Values are combined using the standard combiner, starting at current and
     * going forward around the list.
     */
    override fun hashCode(): Int {
        return (0 ..< items.size).fold(0) { hash, index ->
            31 * hash + get(index).hashCode()
        }
    }
    
    /**
     * Get the element at the "index" relative to current position.
     */
    operator fun get(index: Int): T {
        val circularIndex = (current + index).mod(items.size)
        return items[circularIndex]
    }

    /**
     * Set the element at the "index" relative to current position.
     */
    fun set(index: Int, value: T) : CircularList<T> {
        val itemIndex = (current + index).mod(items.size)
        val newItems = items.mapIndexed { i, item ->
            if (i == itemIndex) value else item
        }
        return CircularList(newItems, current)
    }

    /**
     * Advances the current position by N.
     */
    fun advance(n: Int) : CircularList<T> =
        CircularList(
            items,
            (current + n).mod(items.size)
        )

    /**
     * Reverses N items, starting with the current one.
     */
    fun reverseN(n: Int) : CircularList<T> =
        CircularList(
            (0 ..< items.size).map { i ->
                if (i < n) {
                    get(n - i - 1)
                } else {
                    get(i)
                }
            },
            0
        )

    /**
     * Display the current state using a format similar to the one in the problem description.
     *
     * We always put the current item first.
     */
    override fun toString(): String {
        return (0..< items.size)
            .map { index ->
                val v = get(index)
                if (index == 0) "[$v]" else "$v"
            }
            .joinToString(" ")
    }
}