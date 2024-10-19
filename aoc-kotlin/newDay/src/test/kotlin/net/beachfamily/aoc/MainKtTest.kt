package net.beachfamily.aoc

import com.google.common.jimfs.Configuration
import com.google.common.jimfs.Jimfs
import org.junit.jupiter.api.Assertions.*
import java.nio.file.FileSystem
import java.nio.file.Files
import kotlin.test.Test

class MainKtTest {

    @Test
    fun testAddIncludeLineToProject() {

        val fileSystem: FileSystem = Jimfs.newFileSystem(Configuration.unix())
        val root = fileSystem.getPath("/aoc-kotlin")
        Files.createDirectories(root)
        
        val settingsFile = root.resolve("settings.gradle.kts")

        val originalText =
            """
            |before
            |include("a")
            |include("z")
            |after
            |""".trimMargin()
        Files.write(settingsFile, originalText.toByteArray())

        addIncludeLineToProject(settingsFile, "newProject")

        val expectedText =
            """
            |before
            |after
            |include("a")
            |include("newProject")
            |include("z")
            |""".trimMargin()

        assertEquals(
            expectedText,
            Files.readAllBytes(settingsFile).decodeToString()
        )
    }
}