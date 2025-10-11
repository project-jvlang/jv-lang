# Android SDK非サポートポリシー

**日本語** | [English](android-policy-en.md)

## 概要

**jv言語はAndroid SDKを公式サポート対象外とします。**

この文書では、jv言語がAndroid SDK開発を対象としない理由、将来的な対応条件、および推奨される代替手段について説明します。

## 現状認識

- **Android 14（API 34）**: Java 17をコアライブラリとしてサポート
- **jv言語の要件**: Java 25 LTS（record、パターンマッチング、仮想スレッド、sealed class）
- **互換性ギャップ**: Java 17とJava 25の間には8バージョンの差があり、jv言語の核心機能が利用不可能

## 非サポートの理由

### 1. Java 25非対応

Android SDK（API 34時点）は最新でもJava 17までのサポートであり、jv言語が依存するJava 25機能が使用できません:

- 仮想スレッド（Virtual Threads）
- 強化されたパターンマッチング
- 最新のrecord機能
- sealed class の完全サポート

### 2. 言語仕様の制約

Android独自のDEX制約とDesugaring制限により、Java 25の最新機能が動作しません:

- DEX形式はJava 25のクラスファイル形式（バージョン69）に未対応
- Desugaringは限定的な機能のみサポート

### 3. メンテナンスコスト

Android特有のビルドシステムへの対応が膨大な工数を必要とします:

- AGP（Android Gradle Plugin）
- D8/R8コンパイラ
- Desugaringシステム
- Android Studioツールチェーン

### 4. 言語哲学との不一致

jv言語は「ゼロマジック、Java 25ネイティブ」を掲げており、制約が多いAndroid環境は設計思想に反します。

### 5. API互換性

Android SDKのJava APIはJava 8ベース（一部Java 17機能をDesugaringでサポート）であり、Java 25標準ライブラリの多くが存在しません。

## なぜ`--target 21`でもAndroidで動作しないのか

仮にjv言語を`--target 21`でコンパイルしても、以下の理由でAndroidでは動作しません:

### 1. Desugaring限界

Android GradleのDesugaringはJava 17までの**一部機能のみ**をサポートしており、Java 21の多くの機能は未対応です。

### 2. API制約

Android SDKのJava APIはJava 8ベースであり、Java 21/25の標準ライブラリAPIが存在しません:

- Java 17の一部機能が追加されているのみ
- Java 21/25固有のAPIは利用不可

### 3. 実行時制約

ART（Android Runtime）がJava 21/25のバイトコードセマンティクスに未対応:

- クラスファイルバージョン65-69は動作不可
- Java 25のバイトコード命令セットが認識されない

### 4. ビルドシステム不整合

jv言語のビルドチェーンはjavacベースですが、AndroidはD8/R8によるDEX変換が必須であり、バイトコード形式に互換性がありません。

### 5. 仮想スレッド不可

jv言語の`spawn {}`構文はJava 25のVirtual Threadsに依存しており、Androidでは完全に動作不可能です。

## 将来的な対応条件

以下の条件が**すべて**満たされた場合、Android SDK対応を検討します:

### 1. Android SDKがJava 25を完全サポート

recordやパターンマッチング、仮想スレッドなどのJava 25機能がAndroidで完全動作可能になること。

### 2. DEX形式がJava 25バイトコードをサポート

D8/R8がJava 25のクラスファイル形式（バージョン69）を完全にサポートすること。

### 3. Android Studioツールチェーンの対応

AGP（Android Gradle Plugin）、Lint、R8等がJava 25に完全対応すること。

### 4. ART（Android Runtime）の対応

Java 25のバイトコードセマンティクスと標準ライブラリをネイティブサポートすること。

### 5. コミュニティからの強い要望

実際のユーザーベースからAndroid対応への具体的な需要が確認されること。

## 推奨される代替手段

Android開発には以下の既存言語を推奨します:

### Kotlin（推奨）

- Android公式サポート言語
- null安全性とモダン構文を提供
- Jetpack Composeとの統合
- 活発なコミュニティとエコシステム

### Java 17

- Android 14（API 34）で公式サポート
- record等の一部モダン機能が利用可能
- 安定した実績とツールチェーン

### Jetpack Compose

- 宣言的UIフレームワーク
- Kotlinとの組み合わせでモダンなAndroid開発を実現

## 結論

**jv言語はサーバーサイド、デスクトップ、CLI、データ処理などの標準JVM環境に特化します。**

Android SDK開発には、KotlinまたはJava 17の使用を強く推奨します。これらの言語は、Androidプラットフォームで実証済みの安定性と、豊富なツールチェーン・ライブラリサポートを提供します。

## 関連ドキュメント

- [言語仕様](language-spec.md) - jvの形式的仕様
- [Javaターゲットマトリクス](java-target-matrix.md) - Java 21/25の機能対応表
- [Java連携](java-interop.md) - Javaライブラリとの連携方法
