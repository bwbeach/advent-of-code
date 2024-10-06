// commandLineTool1/build.gradle.kts
plugins {
    application  // Application plugin to create an executable
    kotlin("jvm") version "2.0.20"  // Kotlin JVM plugin
}

application {
    mainClass.set("com.example.MainKt")  // Main class for command-line tool (note the "Kt" suffix)
}

dependencies {
    implementation(project(":testLibrary"))
}

repositories {
    mavenCentral()
}
