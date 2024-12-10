plugins {
    kotlin("jvm") version "2.0.20"  // Use Kotlin JVM plugin
}

dependencies {
    implementation(kotlin("stdlib"))
    implementation("com.google.guava:guava:33.3.1-jre")
    testImplementation(kotlin("test"))
}

tasks.test {
    useJUnitPlatform()
}
