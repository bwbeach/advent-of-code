package net.beachfamily.aoc

import java.nio.file.Files
import java.nio.file.Paths

fun readInput(name: String) : String {
    val dir = Paths.get("/Users/brianb/sandbox/advent-of-code/problems")  // TODO: use env var
    var path = dir.resolve(name)
    return Files.readAllBytes(path).decodeToString()
}
