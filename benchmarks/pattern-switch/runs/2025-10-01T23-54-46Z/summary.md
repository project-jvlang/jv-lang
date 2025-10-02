# Pattern Switch Benchmark Summary
- Timestamp: 2025-10-01T23-54-46Z (UTC)
- Command: cargo bench --manifest-path jv/Cargo.toml --bench pattern_switch
- Raw log: benchmarks/pattern-switch/runs/2025-10-01T23-54-46Z/raw.log

| Scenario | Median | Budget | Status | Change | Single-run |
|----------|--------|--------|--------|--------|------------|
| java21_sealed_depth10 | 39.467 us | 100.000 ms | PASS | -1.2743% [-3.7648%, +1.3202%] (p=0.34 > 0.05)<br>No change in performance detected.<br>Found 6 outliers among 100 measurements (6.00%)<br>6 (6.00%) high mild | 89.000 us |
| java21_when_arms_100 | 118.24 us | 10.000 ms | PASS | +0.9274% [-1.6033%, +3.5101%] (p=0.49 > 0.05)<br>No change in performance detected.<br>Found 5 outliers among 100 measurements (5.00%)<br>4 (4.00%) high mild<br>1 (1.00%) high severe | 152.000 us |
| java21_when_arms_1000 | 1.1688 ms | 50.000 ms | PASS | -3.5600% [-6.6388%, -0.7285%] (p=0.02 < 0.05)<br>Change within noise threshold.<br>Found 3 outliers among 100 measurements (3.00%)<br>2 (2.00%) high mild<br>1 (1.00%) high severe | 1.252 ms |
| java21_when_arms_500 | 589.24 us | 25.000 ms | PASS | -2.0174% [-4.6885%, +0.6382%] (p=0.15 > 0.05)<br>No change in performance detected.<br>Found 4 outliers among 100 measurements (4.00%)<br>3 (3.00%) high mild<br>1 (1.00%) high severe | 620.000 us |
| java21_when_arms_5000 | 7.6656 ms | 250.000 ms | PASS | +3.1399% [+0.6898%, +5.5565%] (p=0.01 < 0.05)<br>Change within noise threshold.<br>Found 6 outliers among 100 measurements (6.00%)<br>5 (5.00%) high mild<br>1 (1.00%) high severe | 7.977 ms |
| java25_sealed_depth10 | 26.622 us | 100.000 ms | PASS | -4.6020% [-7.1296%, -2.0410%] (p=0.00 < 0.05)<br>Performance has improved.<br>Found 6 outliers among 100 measurements (6.00%)<br>5 (5.00%) high mild<br>1 (1.00%) high severe | 57.000 us |
| java25_when_arms_100 | 39.119 us | 10.000 ms | PASS | +1.4049% [-0.8741%, +3.6509%] (p=0.22 > 0.05)<br>No change in performance detected.<br>Found 6 outliers among 100 measurements (6.00%)<br>6 (6.00%) high mild | 78.000 us |
| java25_when_arms_1000 | 378.79 us | 50.000 ms | PASS | -2.3289% [-4.9564%, +0.1834%] (p=0.08 > 0.05)<br>No change in performance detected.<br>Found 4 outliers among 100 measurements (4.00%)<br>4 (4.00%) high mild | 368.000 us |
| java25_when_arms_500 | 195.15 us | 25.000 ms | PASS | +1.3997% [-0.5167%, +3.4662%] (p=0.16 > 0.05)<br>No change in performance detected.<br>Found 5 outliers among 100 measurements (5.00%)<br>4 (4.00%) high mild<br>1 (1.00%) high severe | 202.000 us |
| java25_when_arms_5000 | 1.9770 ms | 250.000 ms | PASS | -2.3709% [-4.8193%, +0.0135%] (p=0.06 > 0.05)<br>No change in performance detected.<br>Found 4 outliers among 100 measurements (4.00%)<br>4 (4.00%) high mild | 1.991 ms |
