import org.jetbrains.kotlin.gradle.plugin.KotlinSourceSet
import org.jetbrains.kotlin.gradle.plugin.mpp.KotlinNativeTargetWithHostTests

plugins {
    kotlin("multiplatform") version "1.6.10"
}

group = "me.ben"
version = "1.0-SNAPSHOT"

repositories {
    mavenCentral()
}

kotlin {
    jvm {
        compilations.all {
            kotlinOptions.jvmTarget = "11"
        }
        testRuns.forEach {
            it.executionTask.configure { this.useJUnit() }
        }
    }

    js(IR) {
        browser {
            commonWebpackConfig {
                cssSupport.enabled = false
            }
            testRuns.forEach {
                it.executionTask.configure {
                    this.useMocha {
                        // https://mochajs.org/#-timeout-ms-t-ms
                        /** [org.jetbrains.kotlin.gradle.targets.js.testing.mocha.KotlinMocha] */
                        this.timeout = "20s"
                    }
                }
            }
        }
        nodejs {
            testRuns.forEach {
                it.executionTask.configure {
                    this.useMocha {
                        this.timeout = "20s"
                    }
                }
            }
        }
    }

    val hostOs: String = System.getProperty("os.name")
    val isMingwX64: Boolean = hostOs.startsWith("Windows")
    val nativeTarget: KotlinNativeTargetWithHostTests = when {
        hostOs == "Mac OS X" -> macosX64("native")
        hostOs == "Linux" -> linuxX64("native")
        isMingwX64 -> mingwX64("native")
        else -> throw GradleException("Host OS is not supported in Kotlin/Native.")
    }

    sourceSets {
        val commonMain: KotlinSourceSet by getting
        val commonTest by getting {
            dependencies {
                implementation(kotlin("test"))
            }
        }
        val jvmMain by getting
        val jvmTest by getting
        val jsMain by getting
        val jsTest by getting
        val nativeMain by getting
        val nativeTest by getting
    }
}
