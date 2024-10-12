// Base build file for multi-project Gradle build.

plugins {
    base  // Base plugin to aggregate tasks
}

// This that apply to all sub-projects
subprojects {
//    apply(plugin = "org.jetbrains.kotlin.jvm")  // Applies Kotlin plugin to all subprojects

    repositories {
        mavenCentral()
    }

//    dependencies {
//        // Common test dependencies for all subprojects
//        testImplementation(kotlin("test"))
//    }

//    tasks.withType<org.jetbrains.kotlin.gradle.tasks.KotlinCompile> {
//        compilerOptions {
//            jvmTarget.set(JvmTarget.JVM_21)
//            languageVersion.set(KotlinVersion.KOTLIN_2_2)
//            apiVersion.set(KotlinVersion.KOTLIN_2_2)
//        }
//    }
}

