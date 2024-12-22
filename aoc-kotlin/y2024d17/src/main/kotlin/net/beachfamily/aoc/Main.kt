package net.beachfamily.aoc

import java.math.BigInteger

fun main() {
    val input = readInput("y2024d17")
    println(part1(input))
    println(part2(input))
}

fun part1(s: String) : String =
    Computer.parse(s).run().map { it.toString() }.joinToString(",")

fun part2(s: String) : BigInteger {
    val computer = Computer.parse(s)
    printProgram(computer.program)
    var a = 0.toBigInteger()
    for (n in computer.program.reversed()) {
        a = findNext(computer, a * 8.toBigInteger(), n)
    }
    return a
}

fun findNext(computer: Computer, a: BigInteger, n: Int): BigInteger {
    for (i in 0 ..< 8) {
        val x = a + i.toBigInteger()
        val c = Computer(
            x,
            computer.b,
            computer.c,
            computer.program
        )
        if (c.run().first() == n.toBigInteger()) {
            return x
        }
    }
    throw IllegalArgumentException("findNext failed for $a, $n")
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

    fun run(): Sequence<BigInteger>  =
        sequence {
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
                            pc = program[pc + 1] - 2
                        }
                    }

                    4 -> {
                        // bxc
                        b = b.xor(c)
                    }

                    5 -> {
                        // out
                        yield(comboOperand().mod(8.toBigInteger()))
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

fun printProgram(program: List<Int>) {
    val opcodes = listOf("adv", "bxl", "bst", "jnz", "bxc", "out", "bdv", "cdv")
    val operandTypes = listOf("c", "l", "c", "l", "n", "c", "c", "c")
    for (i in 0..< program.size step 2) {
        val opcode = program[i]
        val opcodeStr = opcodes[opcode]
        val operand = program[i+1]
        val operandStr = when(operandTypes[opcode]) {
            "n" -> ""
            "l" -> "$operand"
            "c" -> when (operand) {
                0, 1, 2, 3 -> "$operand"
                4 -> "a"
                5 -> "b"
                6 -> "c"
                else -> throw IllegalArgumentException("Unknown combo operand: $operand")
            }
            else -> throw IllegalArgumentException("Unknown operand type: ${operandTypes[opcode]}")
        }
        println("$i: $opcodeStr $operandStr")
    }
}

