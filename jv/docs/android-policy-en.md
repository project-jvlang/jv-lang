# Android SDK Non-Support Policy

[日本語](android-policy.md) | **English**

## Overview

**The jv language officially does not support Android SDK.**

This document explains why jv language does not target Android SDK development, future support conditions, and recommended alternatives.

## Current Status

- **Android 14 (API 34)**: Supports Java 17 as the core library
- **jv Language Requirements**: Java 25 LTS (records, pattern matching, virtual threads, sealed classes)
- **Compatibility Gap**: 8 version difference between Java 17 and Java 25 makes jv's core features unavailable

## Reasons for Non-Support

### 1. Java 25 Not Supported

Android SDK (as of API 34) supports only up to Java 17, making Java 25 features required by jv unavailable:

- Virtual Threads
- Enhanced pattern matching
- Latest record features
- Full sealed class support

### 2. Language Specification Constraints

Android-specific DEX constraints and Desugaring limitations prevent Java 25 features from working:

- DEX format doesn't support Java 25 class file format (version 69)
- Desugaring supports only limited features

### 3. Maintenance Cost

Supporting Android-specific build systems requires enormous effort:

- AGP (Android Gradle Plugin)
- D8/R8 compiler
- Desugaring system
- Android Studio toolchain

### 4. Philosophical Mismatch

jv language advocates "zero magic, Java 25 native," which contradicts Android's constrained environment.

### 5. API Compatibility

Android SDK's Java API is Java 8-based (with some Java 17 features via Desugaring), lacking most Java 25 standard library APIs.

## Why `--target 21` Doesn't Work on Android

Even compiling jv with `--target 21` won't work on Android for these reasons:

### 1. Desugaring Limitations

Android Gradle's Desugaring supports **only some features** up to Java 17; many Java 21 features are unsupported.

### 2. API Constraints

Android SDK's Java API is Java 8-based, missing Java 21/25 standard library APIs:

- Only some Java 17 features added
- Java 21/25-specific APIs unavailable

### 3. Runtime Constraints

ART (Android Runtime) doesn't support Java 21/25 bytecode semantics:

- Class file versions 65-69 won't run
- Java 25 bytecode instruction set not recognized

### 4. Build System Mismatch

jv's build chain is javac-based, but Android requires D8/R8 DEX conversion with incompatible bytecode formats.

### 5. Virtual Threads Unavailable

jv's `spawn {}` syntax depends on Java 25 Virtual Threads, completely unavailable on Android.

## Future Support Conditions

Android SDK support will be considered only if **all** these conditions are met:

### 1. Android SDK Fully Supports Java 25

Java 25 features (records, pattern matching, virtual threads) must work completely on Android.

### 2. DEX Format Supports Java 25 Bytecode

D8/R8 must fully support Java 25 class file format (version 69).

### 3. Android Studio Toolchain Support

AGP (Android Gradle Plugin), Lint, R8, etc. must fully support Java 25.

### 4. ART (Android Runtime) Support

Native support for Java 25 bytecode semantics and standard library.

### 5. Strong Community Demand

Concrete demand for Android support confirmed from actual user base.

## Recommended Alternatives

We recommend these established languages for Android development:

### Kotlin (Recommended)

- Official Android support language
- Null safety and modern syntax
- Jetpack Compose integration
- Active community and ecosystem

### Java 17

- Official support in Android 14 (API 34)
- Some modern features like records available
- Stable track record and toolchain

### Jetpack Compose

- Declarative UI framework
- Modern Android development with Kotlin

## Conclusion

**The jv language specializes in standard JVM environments: server-side, desktop, CLI, and data processing.**

For Android SDK development, we strongly recommend using Kotlin or Java 17. These languages provide proven stability on the Android platform with rich toolchain and library support.

## Related Documentation

- [Language Specification](language-spec-en.md) - Formal jv specification
- [Java Target Matrix](java-target-matrix.md) - Java 21/25 feature support matrix
- [Java Interop](java-interop-en.md) - Working with Java libraries
