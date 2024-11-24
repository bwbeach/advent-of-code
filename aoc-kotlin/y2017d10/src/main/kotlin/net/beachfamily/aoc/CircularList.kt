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
            items.mapIndexed { i, v ->
                val indexFromCurrent = (i - current).mod(items.size)
                if (indexFromCurrent < n) {
                    get(n - indexFromCurrent - 1)
                } else {
                    v
                }
            },
            current
        )

    /**
     * Display the current state using a format similar to the one in the problem description.
     *
     * We always put the current item first.
     */
    override fun toString(): String {
        return items.mapIndexed { i, v ->
                if (i == current) "[$v]" else "$v"
            }
            .joinToString(" ")
    }
}