plugins {
    application
    kotlin("jvm") version "2.0.20"
}

application {
    mainClass.set("net.beachfamily.aoc.MainKt")
}

dependencies {
    implementation(kotlin("stdlib"))
    testImplementation(kotlin("test"))
    testImplementation("org.junit.jupiter:junit-jupiter-api:5.9.2")
}

tasks.test {
    useJUnitPlatform()
}
