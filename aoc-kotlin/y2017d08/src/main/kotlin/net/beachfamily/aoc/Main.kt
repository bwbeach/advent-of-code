package net.beachfamily.aoc

enum class ComparisonOperator(val symbol: String) {
    LT("<"),
    LE("<="),
    EQ("=="),
    GE(">="),
    GT(">"),
    NE("!=");

    companion object {
        fun fromString(symbol: String): ComparisonOperator? =
            values().find { it.symbol == symbol }
    }

    override fun toString(): String = symbol
}

enum class Direction(
    val multiplier: Int
) {
    INC(1),
    DEC(-1);
}

typealias Memory = Map<String, Int>

sealed class IntExpr {
    abstract fun eval(memory: Memory): Int
    data class Register(val name: String) : IntExpr() {
        override fun eval(memory: Memory): Int = memory[name] ?: 0
    }
    data class Constant(val value: Int) : IntExpr() {
        override fun eval(memory: Memory): Int = value
    }
}

data class BoolExpr (
    val lhs: IntExpr,
    val op: ComparisonOperator,
    val rhs: IntExpr
) {
    fun eval(memory: Memory): Boolean {
        val lhsValue = lhs.eval(memory)
        val rhsValue = rhs.eval(memory)
        return when (op) {
            ComparisonOperator.LT -> lhsValue < rhsValue
            ComparisonOperator.LE -> lhsValue <= rhsValue
            ComparisonOperator.EQ -> lhsValue == rhsValue
            ComparisonOperator.GE -> lhsValue >= rhsValue
            ComparisonOperator.GT -> lhsValue > rhsValue
            ComparisonOperator.NE -> lhsValue != rhsValue
        }
    }
}

data class Instruction (
    val register: String,
    val direction: Direction,
    val value: Int,
    val test: BoolExpr
) {
    companion object {
        fun parse(line: String): Instruction {
            val parts = words(line)
            val register = parts[0]
            val direction = Direction.valueOf(parts[1].uppercase())
            val value = parts[2].toInt()
            val lhs = parsePrimary(parts[4])
            val op = ComparisonOperator.fromString(parts[5])!!
            val rhs = parsePrimary(parts[6])
            val test = BoolExpr(lhs, op, rhs)

            return Instruction(register, direction, value, test)
        }

        private fun parsePrimary(text: String) =
            if (text.toIntOrNull() != null) {
                IntExpr.Constant(text.toInt())
            } else {
                IntExpr.Register(text)
            }
    }

    fun execute(memory: MutableMap<String, Int>) {
        if (test.eval(memory)) {
            val prevValue = memory[register] ?: 0
            val offset = direction.multiplier * value
            memory[register] = prevValue + offset
        }
    }
}

fun main() {
    val input = readInput("y2017d08")
    val program = lines(input).map { Instruction.parse(it) }
    println(part1(program))
    println(part2(program))
}

fun part1(program: List<Instruction>) : Int {
    val memory = mutableMapOf<String, Int>()
    program.forEach { it.execute(memory) }
    return memory.values.max()
}

fun part2(program: List<Instruction>) : Int {
    val memory = mutableMapOf<String, Int>()
    return program.map { it.execute(memory); memory.values.max() }.max()
}

