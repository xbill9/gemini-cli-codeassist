// swift-tools-version: 6.0
import PackageDescription

let package = Package(
    name: "mcp-https-swift",
    platforms: [
        .macOS(.v13),
    ],
    dependencies: [
        .package(url: "https://github.com/modelcontextprotocol/swift-sdk", from: "0.10.2"),
        .package(url: "https://github.com/swift-server/swift-service-lifecycle.git", from: "2.6.0"),
        .package(url: "https://github.com/apple/swift-log.git", from: "1.6.0"),
        .package(url: "https://github.com/hummingbird-project/hummingbird.git", from: "2.0.0"),
    ],
    targets: [
        .executableTarget(
            name: "mcp-https-swift",
            dependencies: [
                .product(name: "MCP", package: "swift-sdk"),
                .product(name: "ServiceLifecycle", package: "swift-service-lifecycle"),
                .product(name: "Logging", package: "swift-log"),
                .product(name: "Hummingbird", package: "hummingbird"),
            ],
            swiftSettings: [
                .enableUpcomingFeature("StrictConcurrency")
            ]
        ),
        .testTarget(
            name: "mcp-https-swiftTests",
            dependencies: ["mcp-https-swift"]
        ),
    ]
)
