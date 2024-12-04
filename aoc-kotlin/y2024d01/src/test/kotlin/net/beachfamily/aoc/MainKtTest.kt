package net.beachfamily.aoc

import org.junit.jupiter.api.Assertions.*
import kotlin.test.Test

class MainKtTest {

     @Test
     fun testPart1() =
         assertEquals(
             11,
             part1(
                 """
                 3   4
                 4   3
                 2   5
                 1   3
                 3   9
                 3   3   
                 """.trimIndent()
             )

         )
}