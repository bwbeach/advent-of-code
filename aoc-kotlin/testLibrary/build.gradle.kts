// testLibrary/build.gradle.kts
plugins {
    kotlin("jvm") version "2.0.20"  // Use Kotlin JVM plugin
}

dependencies {
    implementation(kotlin("stdlib"))
    testImplementation(kotlin("test"))
}

repositories {
    mavenCentral()
}

tasks.test {
    useJUnitPlatform()
}
