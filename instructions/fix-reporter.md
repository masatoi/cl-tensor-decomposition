examples/coupon-purchase-prediction.lisp を参考にして、テンソル分解の結果得られた因子行列からレポートを作るための一般的な方法を実装せよ。

**[SYSTEM / ROLE]**
非負 CP 分解で得た因子行列と、各特徴の離散化メタデータから、**“シナリオカード”形式**のレポートを自動生成します。
事実に忠実で簡潔に書き、**入力で与えられていない数値は絶対に作らない**こと。比喩や誇張は避け、根拠（確率・リフト等）を明記します。
出力は**決められた JSON 仕様**に厳密準拠してください。

---

**[CONTEXT]**

* 入力は以下を前提とする：

  1. **CP 分解結果**：モード n=1..N の因子行列 `A^(n)`（非負）、ランク R、（あれば）成分重み `λ`
  2. **観測の疎テンソル情報**：非ゼロ観測のインデックスとカウント `count(x)`（責務計算に使用）
  3. **特徴メタデータ**：各モードの**ラベル名**・**ビン境界/上位カテゴリ**・**離散化戦略（等頻度/等幅/上位N+その他など）**
  4. **母集団周辺分布** `q_n(i)`（ない場合は観測から推定）
* モード例（順不同）：`purchase / sex / age / user_pref / genre / price_rate / coupon_pref`
* 「none」等の欠損カテゴリは**別扱い**で注記する。

---

**[GOAL]**
従来の「各因子×各軸の**上位10件羅列**」から、次の 2 生成物に**刷新**する。

1. **因子カード JSON（factor_cards.json）**：因子の確率プロファイル・相対的特徴（lift）・カバレッジ等を持つ要約
2. **自然言語レポート（report.md）**：各因子の**タイトル／一文要約／根拠／施策／注意点**を “シナリオカード”として記述

---

**[DEFINITIONS / METRICS]**

1. **列正規化（確率化）**：各モードの各因子列を L1 正規化
   　`p_n(i|r) = A^(n)[i,r] / Σ_i A^(n)[i,r]`
   　（強さは `λ_r` に保持。`λ` が無い場合は列和を `λ` として吸収してもよい）
2. **母集団周辺分布**：`q_n(i)` を観測から周辺化して確率化（0 は ε=1e-8 で平滑化）
3. **リフト**：`lift_n(i,r) = p_n(i|r) / q_n(i)`
   　※小カテゴリのばらつきが大きい場合は**情報事前付き log-odds**も可（ただし式は出力に書かない）
4. **責務に基づくカバレッジ**（soft assignment）：
   　観測 `x = (i1,…,iN)` の成分スコア `s_r = λ_r * Π_n A^(n)[i_n, r]`
   　`p(r|x) = s_r / Σ_k s_k`
   　`coverage_r.count = Σ_x count(x) * p(r|x)`，`coverage_r.share = coverage_r.count / Σ_x count(x)`
5. **純度/コヒーレンス**：各モードの `p_n(·|r)` のエントロピー正規化など（0〜1）。
6. **80%最小被覆集合**：各モードの `p_n(·|r)` を降順で加算し 0.8 到達までのラベル集合。
7. **購入バイアス**（例）：`p_purchase = p_purchase_mode(purchase|r)` を抜粋して記載。

---

**[TASKS]**
**A. 因子カード JSON の生成**（ランク r=0..R-1 それぞれ）

* 必須フィールド（厳守）：

```json
{
  "factor_id": <int>,
  "lambda": <float>,                      // 因子強度
  "coverage": {"count_est": <float>, "share": <float>},
  "purchase_bias": {"purchase": <float>, "not_purchase": <float>},
  "salient": {
    "<mode_name>": [
      {"value":"<label or range>", "prob": <float>, "lift": <float>}
      // 上位 80% 最小被覆集合まで
    ],
    "…": []
  },
  "negatives": { "<mode_name>": [{"value":"<label>","prob":<float>}], "…": []},
  "coherence": {"<mode_name>": <float>, "…": <float>},
  "notes": {"missing_labels_ignored": <true|false>, "discretization": {"<mode>": "<strategy or bins>" }}
}
```

---

**[ACCEPTANCE CRITERIA]**

* JSON は**有効な JSON**で、全因子分を配列にして `factor_cards.json` に収める。
* `Σ_r coverage_r.share` は**1.0±0.02** 以内（誤差は数値安定性の範囲）。
* **「none（欠損）」は“salient”に含めない**が、`notes.missing_labels_ignored=true` を明記。
* レポートの数値は**JSON からの値のみ**を引用。小数は最大 3 桁。
* タイトルはビジネス利用向けに**簡潔・差別化**（例：「関西×美容×中〜高割引」「東京×グルメ×半額台」）。
* スタイル：断定ではなく**示唆**（例：「〜が示唆される」「〜が有効と考えられる」）。
