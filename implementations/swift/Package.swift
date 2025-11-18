// swift-tools-version: 5.9
// The swift-tools-version declares the minimum version of Swift required to build this package.

import PackageDescription

let package = Package(
    name: "FF3",
    platforms: [
        .macOS(.v10_15),
        .iOS(.v13),
        .tvOS(.v13),
        .watchOS(.v6)
    ],
    products: [
        // Products define the executables and libraries a package produces, making them visible to other packages.
        .library(
            name: "FF3",
            targets: ["FF3"]),
        .executable(
            name: "FF3CLI",
            targets: ["FF3CLI"]),
        .executable(
            name: "FF3Validate",
            targets: ["FF3Validate"]),
        .executable(
            name: "FF3Benchmark",
            targets: ["FF3Benchmark"]),
        .executable(
            name: "FF3StressTest",
            targets: ["FF3StressTest"])
    ],
    dependencies: [
        // Dependencies declare other packages that this package depends on.
        .package(url: "https://github.com/krzyzanowskim/CryptoSwift.git", "1.8.0"..<"2.0.0"),
        .package(url: "https://github.com/attaswift/BigInt.git", "5.0.0"..<"6.0.0"),
    ],
    targets: [
        // Targets are the basic building blocks of a package, defining a module or a test suite.
        .target(
            name: "FF3",
            dependencies: [
                .product(name: "BigInt", package: "BigInt"),
                // CryptoSwift only needed on non-Apple platforms
                .product(name: "CryptoSwift", package: "CryptoSwift", condition: .when(platforms: [.linux, .windows, .android, .wasi, .openbsd])),
            ]),
        .executableTarget(
            name: "FF3CLI",
            dependencies: ["FF3"]),
        .executableTarget(
            name: "FF3Validate",
            dependencies: ["FF3"]),
        .executableTarget(
            name: "FF3Benchmark",
            dependencies: ["FF3"]),
        .executableTarget(
            name: "FF3StressTest",
            dependencies: ["FF3"]),
        .testTarget(
            name: "FF3Tests",
            dependencies: ["FF3"]),
    ]
)