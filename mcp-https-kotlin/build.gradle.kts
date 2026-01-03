plugins {
    alias(libs.plugins.kotlin.jvm)
    application
}

group = "com.example"
version = "1.0-SNAPSHOT"

repositories {
    mavenCentral()
}

dependencies {
    implementation(libs.mcp.sdk)
    implementation(libs.coroutines.core)
    implementation(libs.slf4j.simple)
    implementation(libs.ktor.server.core)
    implementation(libs.ktor.server.netty)
    implementation(libs.ktor.server.sse)
    implementation(libs.ktor.serialization.json)
    implementation(libs.ktor.server.content.negotiation)
    implementation(libs.ktor.server.cors)
    testImplementation(libs.ktor.server.test.host)
    testImplementation(libs.kotlin.test)
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