// commandLineTool1/build.gradle.kts
plugins {
    application  // Application plugin to create an executable
    kotlin("jvm") version "2.0.20"  // Kotlin JVM plugin
}

application {
    mainClass.set("net.beachfamily.aoc.MainKt")  // Main class for command-line tool (note the "Kt" suffix)
}

dependencies {
    implementation(kotlin("stdlib"))
    testImplementation(kotlin("test"))
    implementation(project(":testLibrary"))
}

repositories {
    mavenCentral()
}

tasks.test {
    useJUnitPlatform()
}
