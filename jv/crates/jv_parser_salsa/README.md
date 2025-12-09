# jv_parser_salsa

Salsa ベースのインクリメンタルパーサーパイプラインの実装クレート。
Rowan パイプラインと並行に開発し、LSP とビルドのレイテンシ改善を狙う。

## ベンチマーク環境

ベースライン計測は以下の環境で実施する。

- CPU: AMD Ryzen 5 3500U (4C/8T, 2.1GHz base) `lscpu` reported under Microsoft hypervisor
- メモリ: 50.9 GiB (MemTotal: 53461184 kB)
- OS: Ubuntu 24.04.3 LTS (noble) on x86_64
- 仮想化: Hyper-V (Virtualization type: full)

同一環境でウォーム/コールド条件を揃えて計測する。条件詳細は `benches/README.md` を参照。
