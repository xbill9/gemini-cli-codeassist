plugins {
    kotlin("jvm") version "2.3.0"
    application
}

group = "com.example"
version = "1.0-SNAPSHOT"

repositories {
    mavenCentral()
}

dependencies {
    implementation("io.modelcontextprotocol:kotlin-sdk-jvm:0.8.1")
    implementation("org.jetbrains.kotlinx:kotlinx-coroutines-core:1.9.0")
    implementation("org.slf4j:slf4j-simple:2.0.9")
    implementation("io.ktor:ktor-server-core:3.0.0")
    implementation("io.ktor:ktor-server-netty:3.0.0")
    implementation("io.ktor:ktor-server-sse:3.0.0")
    implementation("io.ktor:ktor-serialization-kotlinx-json:3.0.0")
    implementation("io.ktor:ktor-server-content-negotiation:3.0.0")
}

kotlin {
    jvmToolchain(25)
}

application {
    mainClass.set("com.example.mcp.server.MainKt")
}

tasks.test {
    useJUnitPlatform()
}