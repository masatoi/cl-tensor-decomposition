# テンソル分解レポート機能拡張 TODO

LLMによる自然言語レポート生成を強化するための追加指標実装計画。

---

## 現在実装済みの指標

| 指標 | 関数 | 説明 |
|------|------|------|
| prob | `normalize-factor-matrices` | P(カテゴリ\|因子) - 正規化された因子行列 |
| lift | `compute-lift-matrices` | prob / 周辺確率 - 平均との比較 |
| lambda | `compute-lambda-vector` | 因子の全体重み（各モード列和の積） |
| coverage | `compute-coverage` | 各因子がデータ全体の何%を説明するか |
| coherence | `compute-coherence-vectors` | 1 - H/log(n) - エントロピーベースの集中度 |
| KL divergence | `sparse-kl-divergence` | 再構成誤差（モデル全体の適合度） |
| factor similarity | `compute-factor-similarity-matrix` | 因子間のコサイン類似度行列 |
| similar pairs | `extract-similar-factor-pairs` | 閾値以上の類似因子ペア抽出 |
| redundancy score | `compute-factor-redundancy-score` | 因子の冗長度スコア |
| KL contribution | `compute-factor-kl-contributions` | 因子別のKL寄与度（重要度） |
| normalized contribution | `normalize-contributions` | 正規化された寄与度（合計1） |
| contribution ranking | `rank-factors-by-contribution` | 寄与度順の因子ランキング |
| responsibilities | `compute-observation-responsibilities` | 観測→因子の帰属確率行列 |
| responsibility stats | `compute-responsibility-stats` | 帰属確率の統計量 |
| ambiguous observations | `find-ambiguous-observations` | 曖昧な帰属を持つ観測の検出 |
| exclusivity | `compute-factor-exclusivity` | 因子の排他性/重複度 |
| residuals | `compute-observation-residuals` | 観測別の再構成残差 |
| residual stats | `compute-residual-stats` | 残差の統計量 |
| high residual obs | `find-high-residual-observations` | 高残差の観測を検出 |

---

## 追加実装チェックリスト

### 優先度: 高

- [x] **因子間類似度行列 (Factor Similarity Matrix)** ✅ 2024-12-27 完了

  **計算内容**: 因子ベクトル間のコサイン類似度を計算し、R×Rの類似度行列を生成。

  ```
  similarity(r1, r2) = Σ_i (a_i,r1 * a_i,r2) / (||a_r1|| * ||a_r2||)
  ```

  **なぜ必要か**:
  - LLMが「Factor 3と5は類似したパターンを持つ」と説明できる
  - 冗長な因子の検出（類似度 > 0.8 なら統合検討）
  - 因子のクラスタリング・グルーピングの根拠
  - ランク選択の妥当性評価（高類似因子が多い = ランク過大）

  **出力形式**:
  ```lisp
  (:factor_similarities ((0 1 0.85) (0 2 0.12) ...))
  ```

- [x] **因子別KL寄与度 (Factor KL Contribution)** ✅ 2025-12-27 完了

  **計算内容**: 各因子を除外した時のKLダイバージェンス増加量。

  ```
  contribution(r) = KL(X, X^_{-r}) - KL(X, X^)
  ```

  **なぜ必要か**:
  - lambdaは「量」を測るが、KL寄与は「情報量の重要性」を測る
  - 「Factor 2はデータの説明に最も重要」という根拠
  - 因子の削除可能性の判断（寄与が小さい因子は省略可）
  - coverage（量的シェア）とKL寄与（質的重要度）の乖離が示唆を生む

  **出力形式**:
  ```lisp
  (:kl_contributions #(0.15 0.32 0.08 ...))
  ```

- [x] **観測→因子の帰属確率 (Observation Responsibilities)** ✅ 2025-12-27 完了

  **計算内容**: 各観測データポイントがどの因子に帰属するかの事後確率。

  ```
  responsibility(n, r) = λ_r * Π_m a_{x_n^m, r} / Σ_r' λ_r' * Π_m a_{x_n^m, r'}
  ```

  **なぜ必要か**:
  - 個別レコードの説明: 「この購買はFactor 3に87%帰属」
  - セグメント割当の確信度評価
  - 曖昧なケース（複数因子に分散）の検出
  - coverage計算時に内部で使用しているが、現在は集計後のみ出力

  **出力形式**:
  ```lisp
  ;; 全観測は大きすぎるため、サマリ統計を出力
  (:responsibility_stats
    (:mean_max_responsibility 0.72)  ; 最大帰属確率の平均
    (:ambiguous_rate 0.15))          ; max < 0.5 の観測割合
  ```

---

### 優先度: 中

- [ ] **モード間条件付き依存 (Cross-Mode Association)**

  **計算内容**: 1つの因子内で、異なるモードのラベル間の共起強度。

  ```
  association(mode1, label1, mode2, label2 | factor r)
    = P(label1|r) * P(label2|r) の積と独立仮定との比較
  ```

  **なぜ必要か**:
  - 「東京×グルメの結びつきが強い」の数値的根拠
  - 因子内でのラベル組み合わせパターンの解釈
  - マーケティング施策の組み合わせ提案
  - テンソル分解は各モードを独立に扱うが、元データでの共起とのギャップを検出

  **出力形式**:
  ```lisp
  (:top_associations
    (((:mode "genre" :label "グルメ")
      (:mode "prefecture" :label "東京都")
      :strength 0.85)
     ...))
  ```

- [x] **因子の排他性/重複度 (Factor Exclusivity)** ✅ 2025-12-27 完了

  **計算内容**: 観測が単一因子に帰属する度合いの分布。

  ```
  exclusivity = mean(max_r responsibility(n, r))
  overlap = 1 - exclusivity
  ```

  **なぜ必要か**:
  - 因子が「純粋なセグメント」か「混合パターン」かを判定
  - 高exclusivity: クリアなセグメンテーション
  - 低exclusivity: ソフトクラスタリング的解釈が必要
  - LLMへの指示: 「このモデルはハードセグメントとして解釈可能」

  **出力形式**:
  ```lisp
  (:exclusivity 0.73)
  (:overlap 0.27)
  ```

- [x] **再構成誤差の観測別分解 (Per-Observation Residuals)** ✅ 2025-12-27 完了

  **計算内容**: 各観測の再構成誤差（KL寄与または絶対誤差）。

  ```
  residual(n) = x_n * log(x_n / x^_n) - x_n + x^_n
  ```

  **なぜ必要か**:
  - モデルで説明できない異常パターンの検出
  - 外れ値スコアとしての活用
  - 「このデータポイントは典型的でない」という警告
  - モデル改善のヒント（残差が大きいパターンの分析）

  **出力形式**:
  ```lisp
  (:residual_stats
    (:mean 0.05)
    (:std 0.12)
    (:p95 0.23)
    (:outlier_count 42))  ; 残差 > mean + 3*std
  ```

---

### 優先度: 低

- [ ] **スパース性指標 (Sparsity Metrics)**

  **計算内容**: Gini係数またはHoyerスパース性。

  ```
  hoyer(x) = (sqrt(n) - L1(x)/L2(x)) / (sqrt(n) - 1)
  gini(x) = Σ_i (2i - n - 1) * x_sorted[i] / (n * Σ x)
  ```

  **なぜ必要か**:
  - coherence（エントロピーベース）より厳密なスパース性評価
  - 分布の「尖り具合」を異なる観点で測定
  - coherenceが中程度の時の補助指標

  **備考**: coherenceで多くの場合は十分。高精度な比較研究時に有用。

  **出力形式**:
  ```lisp
  (:sparsity (:gini 0.65) (:hoyer 0.48))
  ```

- [ ] **ランク安定性 (Rank Stability)**

  **計算内容**: 異なる初期値での因子の一致度。

  ```
  stability = mean(max_r' similarity(factor_r, factor'_r'))
  ```

  **なぜ必要か**:
  - 結果の信頼性・再現性の評価
  - 不安定な因子への警告
  - ランク選択の妥当性検証

  **備考**: 計算コストが高い（複数回分解が必要）。

  **出力形式**:
  ```lisp
  (:stability_scores #(0.95 0.88 0.72 ...))  ; 因子ごと
  (:mean_stability 0.85)
  ```

- [ ] **時間的変化追跡 (Temporal Drift)**

  **計算内容**: 2時点の分解結果間の因子マッチングと変化量。

  **なぜ必要か**:
  - 「前月と比べてFactor 3のシェアが増加」の根拠
  - セグメント構造の変化検出
  - 施策効果の評価

  **備考**: 2つのfactor-matrix-vectorを入力とする比較関数として実装。

  **出力形式**:
  ```lisp
  (:temporal_comparison
    (:matched_factors ((0 2) (1 0) ...))  ; 新旧の対応
    (:shift_magnitudes #(0.05 0.12 ...)))
  ```

---

## 実装ノート

### 共通設計方針

1. **既存の`generate-factor-cards`への統合**
   - 新指標は`build-card-alist`に追加フィールドとして組み込む
   - オプショナル引数で計算有無を制御（デフォルトOFF for 計算コスト高いもの）

2. **JSON出力との互換性**
   - すべての新指標はalist形式で、既存のJSON出力パイプラインを通過可能に

3. **LLMプロンプト生成関数の追加**
   - 新指標を活用した`generate-llm-prompt`関数を最終的に実装
   - 指標の意味を自然言語で説明するテンプレート付き

### ファイル構成案

```
src/
  core.lisp           # 既存: 分解アルゴリズム
  reporting.lisp      # 既存: レポート生成
  diagnostics.lisp    # 新規: 診断指標（因子間類似度、KL寄与等）
  llm-prompt.lisp     # 新規: LLMプロンプト生成
```

---

## 進捗記録

| 日付 | 完了項目 | 備考 |
|------|----------|------|
| 2024-12-27 | 因子間類似度行列 | `diagnostics.lisp`新規作成、4関数実装、7テスト追加 |
| 2025-12-27 | 因子別KL寄与度 | 5関数実装（sdot-excluding-factor, compute-factor-kl-contributions, normalize-contributions, kl-contributions->alist, rank-factors-by-contribution）、5テスト追加 |
| 2025-12-27 | 観測→因子の帰属確率 | 4関数実装（compute-observation-responsibilities, compute-responsibility-stats, responsibility-stats->alist, find-ambiguous-observations）、5テスト追加 |
| 2025-12-27 | 因子の排他性/重複度 | 2関数実装（compute-factor-exclusivity, factor-exclusivity->alist）、3テスト追加 |
| 2025-12-27 | 再構成誤差の観測別分解 | 4関数実装（compute-observation-residuals, compute-residual-stats, residual-stats->alist, find-high-residual-observations）、4テスト追加 |
| 2025-12-27 | 診断指標をgenerate-factor-cardsに統合 | `reporting.lisp`の`generate-factor-cards`と`build-card-alist`を拡張。`:include-diagnostics t`でモデルレベル診断（kl_divergence, factor_similarity, exclusivity, overlap, responsibility_stats, residual_stats, kl_contributions）と因子レベル診断（kl_contribution, contribution_rank）を出力。後方互換性維持。1テスト追加 |
| 2025-12-27 | レビュー指摘に基づくバグ修正 | (1) `compute-observation-responsibilities`: λ二重スケーリング問題修正（非正規化因子で`sdot`と整合性確保）(2) `compute-observation-residuals`: x=0時のNaN問題修正（KL定義に従い`x-hat`を返す）(3) `normalize-contributions`: 負値クランプとepsilon閾値追加 (4) `generate-factor-cards`: epsilon引数を全診断関数に渡すよう修正 |
| 2025-12-27 | 回帰テスト追加 | `residuals-handle-zero-counts-without-nan`（x=0時のNaN防止確認）、`normalize-contributions-clamps-negative-values`（負値クランプ動作確認）の2テスト追加 |
