plugins {
    application
    kotlin("jvm") version "2.0.20"
}

application {
    mainClass.set("net.beachfamily.aoc.MainKt")
}

dependencies {
    implementation(kotlin("stdlib"))
    implementation(project(":aocLib"))
    implementation("com.google.guava:guava:33.3.1-jre")
    testImplementation(kotlin("test"))
    testImplementation("org.junit.jupiter:junit-jupiter-api:5.9.2")
}

tasks.test {
    useJUnitPlatform()
}
