package net.beachfamily.aoc

import java.io.File
import kotlin.system.exitProcess

/**
 * Creates a subproject for a new problem.
 *
 * Command-line argument is the name of the new subproject.
 *
 * Adds the subproject to the file `settings.gradle.kts`
 * in the current directory by adding an `include` line.
 *
 * Copies the sub-project directory `blank` to a new directory
 * named for the new sub-project.
 */
fun main(args: Array<String>) {

    // Check command-line arguments
    if (args.size != 1) {
        println("args.size: ${args.size}")
        println("Usage: newDay <newProjectName>")
        exitProcess(1)
    }

    // Get the names of files we're dealing with
    val newProjectName = args[0]
    val settingsFile = File("settings.gradle.kts")
    val blankProjectDir = File("blank")
    val newProjectDir = File(newProjectName)

    // Check if the new project directory already exists
    if (newProjectDir.exists()) {
        println("The directory for the new sub-project already exists.")
        exitProcess(1)
    }

    // Add the include line to the settings.gradle.kts file
    addIncludeLineToProject(settingsFile, newProjectName)

    // Copy the blank project directory to the new project directory
//    try {
//        blankProjectDir.copyRecursively(newProjectDir)
//        println("New sub-project '$newProjectName' created successfully.")
//    } catch (e: IOException) {
//        println("An error occurred while creating the new sub-project: ${e.message}")
//        System.exit(1)
//    }

    // Success
    exitProcess(0)
}

/**
 * Adds a new include line to the gradle settings file `settingsFile`, maintaining
 * all include lines at the end of the settings, and in alphabetical order.
 */
private fun addIncludeLineToProject(settingsFile: File, newProjectName: String) {
    // Read the original contents of the file
    val originalLines = settingsFile.readLines()

    // Separate the include lines from the other lines
    val (includeLines, nonIncludeLines) = originalLines.partition { it.startsWith("include(\"") }

    // Add the new include line and sort the include lines
    val updatedIncludeLines = buildList {
        addAll(includeLines)
        add("include(\"$newProjectName\")")
        sort()
    }

    // Make the new file contents
    val newContents = buildList {
        addAll(nonIncludeLines)
        addAll(updatedIncludeLines)
    }

    // Write back to the settings file
    settingsFile.writeText(newContents.joinToString(separator= "\n", postfix= "\n"))
}

