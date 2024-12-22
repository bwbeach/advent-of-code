package net.beachfamily.aoc

import java.math.BigInteger

fun main() {
    val input = readInput("y2024d17")
    println(part1(input))
    println(part2(input))
}

fun part1(s: String) : String =
    Computer.parse(s).run().map { it.toString() }.joinToString(",")

fun part2(s: String) : Int {
    return s.length
}

class Computer(
    var a: BigInteger,
    var b: BigInteger,
    var c: BigInteger,
    val program: List<Int>,
    var pc: Int = 0,
) {
    companion object {
        fun create(a: Int, b: Int, c: Int, program: List<Int>) = Computer(
            a.toBigInteger(),
            b.toBigInteger(),
            c.toBigInteger(),
            program,
        )

        /**
         * Parses an input problem, creating a ready-to run Computer object.
         *
         * Input looks like this:
         * 
         * Register A: 729
         * Register B: 0
         * Register C: 0
         *
         * Program: 0,1,5,4,3,0
         */
        fun parse(input: String): Computer {
            val lines = input.lines().filter { !it.isBlank() }.map { it.trim() }
            val a = lines[0].substringAfter(":").trim().toBigInteger()
            val b = lines[1].substringAfter(":").trim().toBigInteger()
            val c = lines[2].substringAfter(":").trim().toBigInteger()
            val program = lines[3].substringAfter(":").trim().split(",").map { it.toInt() }
            return Computer(a, b, c, program)
        }
    }

    fun run(): List<BigInteger> {
        val result = mutableListOf<BigInteger>()
        while (pc < program.size) {
            require(0 <= pc)
            when (program[pc]) {
                0 -> {
                    // adv
                    val exp = comboOperand()
                    require(exp < Int.MAX_VALUE.toBigInteger())
                    a = a / 2.toBigInteger().pow(exp.toInt())
                }
                1 -> {
                    // bxl
                    b = b.xor(literalOperand())
                }
                2 -> {
                    // bst
                    b = comboOperand().mod(8.toBigInteger())
                }
                3 -> {
                    // jnz
                    if (a != 0.toBigInteger()) {
                        pc = program[pc+1] - 2
                    }
                }
                4 -> {
                    // bxc
                    b = b.xor(c)
                }
                5 -> {
                    // out
                    result.add(comboOperand().mod(8.toBigInteger()))
                }
                6 -> {
                    // bdv
                    val exp = comboOperand()
                    require(exp < Int.MAX_VALUE.toBigInteger())
                    b = a / 2.toBigInteger().pow(exp.toInt())
                }
                7 -> {
                    // cdv
                    val exp = comboOperand()
                    require(exp < Int.MAX_VALUE.toBigInteger())
                    c = a / 2.toBigInteger().pow(exp.toInt())
                }
                else -> throw IllegalArgumentException("Unknown opcode: ${program[pc]}")
            }
            pc += 2
        }
        return result.toList()
    }

    fun comboOperand(): BigInteger =
        when (program[pc+1]) {
            0 -> 0.toBigInteger()
            1 -> 1.toBigInteger()
            2 -> 2.toBigInteger()
            3 -> 3.toBigInteger()
            4 -> a
            5 -> b
            6 -> c
            else -> throw IllegalArgumentException("Unknown combo operand: ${program[pc+1]}")
        }

    fun literalOperand(): BigInteger =
        program[pc+1].toBigInteger()
}

