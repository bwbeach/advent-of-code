package net.beachfamily.aoc

import java.io.File
import java.io.IOException

/**
 * Creates a sub-project for a new problem.
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
        System.exit(1)
    }

    // Get the names of files we're dealing with
    val newProjectName = args[0]
    val settingsFile = File("xxx.settings.gradle.kts")
    val blankProjectDir = File("blank")
    val newProjectDir = File(newProjectName)

    // Check if the new project directory already exists
    if (newProjectDir.exists()) {
        println("The directory for the new sub-project already exists.")
        System.exit(1)
    }

    // Add the include line to the settings.gradle.kts file
    settingsFile.appendText("\ninclude(\"$newProjectName\")")

    // Copy the blank project directory to the new project directory
//    try {
//        blankProjectDir.copyRecursively(newProjectDir)
//        println("New sub-project '$newProjectName' created successfully.")
//    } catch (e: IOException) {
//        println("An error occurred while creating the new sub-project: ${e.message}")
//        System.exit(1)
//    }

    // Success
    System.exit(0)
}

