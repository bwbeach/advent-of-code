package net.beachfamily.aoc

fun main() {
    val input = readInput("y2017d05")
    println(part1(parse(input)))
    println(part2(parse(input)))
}

fun parse(s: String) : Program {
    val instructions = words(s).map{  it.toInt() }.toMutableList()
    return Program(instructions, 0)
} 

fun part1(p: Program) : Int {
    while (!p.isDone()) {
        p.step()
    }
    return p.count
}

fun part2(p: Program) : Int {
    while (!p.isDone()) {
        p.step2()
    }
    return p.count
}


data class Program(
    val instructions: MutableList<Int>,
    var pc: Int,
    var count: Int = 0
) {
    fun step() {
        val delta = instructions[pc]
        instructions[pc] = delta + 1
        pc += delta
        count += 1
    }

    fun step2() {
        val delta = instructions[pc]
        if (delta < 3) {
            instructions[pc] = delta + 1
        } else {
            instructions[pc] = delta - 1
        }
        pc += delta
        count += 1
    }
    
    fun isDone() : Boolean = 
        pc < 0 || pc >= instructions.size
}

