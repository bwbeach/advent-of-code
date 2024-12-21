package net.beachfamily.aoc

import org.junit.jupiter.api.Test
import kotlin.test.assertEquals

class MainTest {

    fun myTest1() =
        """
        #####
        #  E#
        #   #
        #S  #
        #####
        """.trimIndent()

    fun myTest2() =
        """
        #######
        #    E#
        # #####
        #     #
        #     #
        #S    #
        #######
        """.trimIndent()

    fun testInput() =
        """
        ###############
        #.......#....E#
        #.#.###.#.###.#
        #.....#.#...#.#
        #.###.#####.#.#
        #.#.#.......#.#
        #.#.#####.###.#
        #...........#.#
        ###.#.#####.#.#
        #...#.....#.#.#
        #.#.#.###.#.#.#
        #.....#...#.#.#
        #.###.#.#.#.#.#
        #S..#.....#...#
        ###############
        """.trimIndent()

    fun testInput2() =
        """
        #################
        #...#...#...#..E#
        #.#.#.#.#.#.#.#.#
        #.#.#.#...#...#.#
        #.#.#.#.###.#.#.#
        #...#.#.#.....#.#
        #.#.#.#.#.#####.#
        #.#...#.#.#.....#
        #.#.#####.#.###.#
        #.#.#.......#...#
        #.#.###.#####.###
        #.#.#...#.....#.#
        #.#.#.#####.###.#
        #.#.#.........#.#
        #.#.#.#########.#
        #S#.............#
        #################
        """.trimIndent()

    @Test
    fun `examples from part1 problem statement`() {
        assertEquals(1004, part1(myTest1()))
        assertEquals(2008, part1(myTest2()))
        assertEquals(7036, part1(testInput()))
        assertEquals(11048, part1(testInput2()))
    }

    @Test
    fun `examples from part2 problem statement`() {
        assertEquals(45, part2(testInput()))
//        assertEquals(64, part2(testInput2()))
    }
}