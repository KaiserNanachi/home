'use strict';
(function (global) {
  function pick(rng, list) {
    return list[Math.floor(rng() * list.length)];
  }
  function round(value, digits) {
    const scale = Math.pow(10, digits);
    return Math.round(value * scale) / scale;
  }

  const GENERATORS = [
    function bayes(rng) {
      const base = pick(rng, [1, 2, 5, 10]); // %
      const sens = pick(rng, [80, 85, 90, 95]); // %
      const fp = pick(rng, [2, 5, 8, 10]); // %
      const b = base / 100, s = sens / 100, f = fp / 100;
      const ans = round(100 * (b * s) / (b * s + (1 - b) * f), 2);
      return {
        id: 'drill-bayes', chapter: 'prob', title: 'ベイズの定理（陽性適中率）',
        prompt: `有病率 ${base}%、感度 ${sens}%、偽陽性率 ${fp}% の検査で陽性となったとき、実際に疾患がある確率を百分率（%）で小数第2位まで入力してください。`,
        answer: ans, tolerance: 0.02,
        hint: 'P(D|+) = P(+|D)P(D) / {P(+|D)P(D) + P(+|Dᶜ)P(Dᶜ)}',
        explanation: `分子=${(b * s).toFixed(4)}、分母=${(b * s + (1 - b) * f).toFixed(4)} より ${ans}%。`
      };
    },
    function binomial(rng) {
      const n = pick(rng, [20, 40, 50, 80, 100]);
      const p = pick(rng, [0.1, 0.2, 0.25, 0.4, 0.5]);
      const v = round(n * p * (1 - p), 2);
      return {
        id: 'drill-binomial', chapter: 'dist', title: '二項分布の分散',
        prompt: `独立な ${n} 回のベルヌーイ試行（各回の成功確率 p=${p}）における成功回数 X の分散 Var(X) を入力してください。`,
        answer: v, tolerance: 0.01,
        hint: 'Var(X) = np(1−p)',
        explanation: `${n} × ${p} × ${(1 - p).toFixed(2)} = ${v}。`
      };
    },
    function ciSampleSize(rng) {
      const e = pick(rng, [0.02, 0.04, 0.05, 0.1]);
      const n = Math.ceil(Math.pow(1.96 / e, 2) * 0.25);
      return {
        id: 'drill-ci-n', chapter: 'ci', title: '母比率信頼区間の必要標本数',
        prompt: `母比率 p の両側95%信頼区間の誤差幅（半幅）を ${e} 以下にするための保守的な最小標本数 n（z=1.96、p(1−p)≤0.25、整数）を入力してください。`,
        answer: n, tolerance: 0,
        hint: 'n ≥ (1.96 / e)² × 0.25 を切り上げる',
        explanation: `(1.96/${e})² × 0.25 = ${(Math.pow(1.96 / e, 2) * 0.25).toFixed(2)} → 切り上げて ${n}。`
      };
    },
    function zStat(rng) {
      const mu0 = pick(rng, [50, 100, 200]);
      const sigma = pick(rng, [4, 8, 10, 12]);
      const n = pick(rng, [16, 25, 64, 100]);
      const delta = pick(rng, [1, 2, 3, 4]);
      const xbar = mu0 + delta;
      const z = round(delta / (sigma / Math.sqrt(n)), 3);
      return {
        id: 'drill-z-stat', chapter: 'test', title: '母分散既知の z 検定統計量',
        prompt: `H₀: μ=${mu0}、σ=${sigma} 既知、標本数 n=${n} で標本平均 X̄=${xbar} を得た。検定統計量 Z を小数第3位まで入力してください。`,
        answer: z, tolerance: 0.005,
        hint: 'Z = (X̄ − μ₀) / (σ / √n)',
        explanation: `SE = ${sigma}/√${n} = ${(sigma / Math.sqrt(n)).toFixed(3)}、Z = (${xbar}−${mu0})/SE = ${z}。`
      };
    },
    function oddsRatio(rng) {
      const a = pick(rng, [20, 30, 40, 60]);
      const b = pick(rng, [10, 20, 30, 40]);
      const c = pick(rng, [10, 15, 20, 25]);
      const d = pick(rng, [20, 30, 40, 50]);
      const or = round((a * d) / (b * c), 3);
      return {
        id: 'drill-or', chapter: 'categorical', title: '2×2分割表のオッズ比',
        prompt: `2×2分割表が 第1行(a=${a}, b=${b})、第2行(c=${c}, d=${d}) のとき、オッズ比 OR = ad/(bc) を小数第3位まで入力してください。`,
        answer: or, tolerance: 0.005,
        hint: '対角の積 ad を bc で割る',
        explanation: `(${a}×${d}) / (${b}×${c}) = ${a * d} / ${b * c} = ${or}。`
      };
    },
    function olsVariance(rng) {
      const p = pick(rng, [2, 3, 4, 5]);
      const df = pick(rng, [10, 15, 20, 25]);
      const n = df + p;
      const s2 = pick(rng, [2, 3, 4, 5, 6]);
      const sse = s2 * df;
      return {
        id: 'drill-ols-s2', chapter: 'regression', title: '回帰誤差分散の不偏推定量',
        prompt: `観測数 n=${n}、切片を含む説明変数・係数の総数 p=${p} の線形回帰で残差平方和 SSE=${sse} であった。誤差分散の通常の不偏推定値 s² を入力してください。`,
        answer: s2, tolerance: 1e-6,
        hint: 's² = SSE / (n − p)',
        explanation: `自由度 n−p = ${n}−${p} = ${df}、s² = ${sse}/${df} = ${s2}。`
      };
    },
    function logisticOdds(rng) {
      const beta = pick(rng, [0.2, 0.4, 0.5, 0.7, 1.0]);
      const ratio = round(Math.exp(beta), 3);
      return {
        id: 'drill-logistic', chapter: 'glm', title: 'ロジスティック回帰のオッズ比',
        prompt: `ロジスティック回帰で説明変数 x の回帰係数が β=${beta} であった。他の変数を固定して x が 1 増えたとき、オッズは何倍になるか小数第3位まで入力してください。`,
        answer: ratio, tolerance: 0.005,
        hint: 'オッズ比は exp(β)',
        explanation: `exp(${beta}) ≈ ${ratio} 倍。`
      };
    },
    function anovaF(rng) {
      const k = pick(rng, [3, 4, 5]);
      const dfB = k - 1;
      const dfW = pick(rng, [12, 16, 20, 24]);
      const N = dfW + k;
      const msw = pick(rng, [2, 3, 4, 5]);
      const f = pick(rng, [2, 3, 4, 5, 6]);
      const msb = msw * f;
      const ssb = msb * dfB;
      const ssw = msw * dfW;
      const sst = ssb + ssw;
      return {
        id: 'drill-anova-f', chapter: 'anova', title: 'ANOVA表穴埋め（F統計量）',
        prompt: `${k}群・総観測数 N=${N} の一元配置分散分析で、総平方和 SST=${sst}、群内平方和 SSW=${ssw} であった。F統計量を入力してください。`,
        answer: f, tolerance: 1e-6,
        hint: 'SSB = SST − SSW、MSB = SSB/(k−1)、MSW = SSW/(N−k)、F = MSB/MSW',
        explanation: `SSB=${ssb}(df=${dfB})→MSB=${msb}、MSW=${ssw}/${dfW}=${msw}、F=${msb}/${msw}=${f}。`
      };
    },
    function neyman(rng) {
      const n1 = pick(rng, [100, 200, 300]);
      const s1 = pick(rng, [10, 20]);
      const n2 = pick(rng, [100, 200]);
      const s2 = pick(rng, [30, 40, 60]);
      const total = pick(rng, [50, 80, 100, 120]);
      const w1 = n1 * s1, w2 = n2 * s2;
      const alloc2 = round(total * w2 / (w1 + w2), 1);
      return {
        id: 'drill-neyman', chapter: 'sampling', title: '層化抽出のネイマン最適配分',
        prompt: `2層の母集団（第1層 N₁=${n1}, S₁=${s1}／第2層 N₂=${n2}, S₂=${s2}）から総標本数 n=${total} をネイマン配分するとき、第2層の標本数 n₂ を小数第1位まで入力してください。`,
        answer: alloc2, tolerance: 0.15,
        hint: 'n₂ = n × (N₂S₂) / (N₁S₁ + N₂S₂)',
        explanation: `N₁S₁=${w1}, N₂S₂=${w2} より ${total} × ${w2}/${w1 + w2} = ${alloc2}。`
      };
    },
    function pcaContribution(rng) {
      const rho = pick(rng, [0.3, 0.5, 0.6, 0.7, 0.8, 0.9]);
      const contrib = round(100 * (1 + rho) / 2, 1);
      return {
        id: 'drill-pca', chapter: 'multivariate', title: '2変数主成分分析の寄与率',
        prompt: `標準化した2変数の相関係数が ρ=${rho} のとき、第1主成分の寄与率を百分率（%）で小数第1位まで入力してください。`,
        answer: contrib, tolerance: 0.1,
        hint: '相関行列の固有値は 1+ρ と 1−ρ、和は 2',
        explanation: `第1固有値 1+${rho}=${(1 + rho).toFixed(1)} を総分散 2 で割り ${contrib}%。`
      };
    },
    function markovStationary(rng) {
      const a = pick(rng, [0.1, 0.2, 0.3, 0.4]);
      const b = pick(rng, [0.2, 0.3, 0.4, 0.5, 0.6]);
      const pi = round(a / (a + b), 3);
      return {
        id: 'drill-markov', chapter: 'markov', title: '2状態マルコフ連鎖の定常分布',
        prompt: `状態0→1 の遷移確率が a=${a}、状態1→0 の遷移確率が b=${b} の2状態マルコフ連鎖で、定常状態において状態1にいる確率 π₁ を小数第3位まで入力してください。`,
        answer: pi, tolerance: 0.005,
        hint: 'π₁ = a / (a + b)',
        explanation: `${a} / (${a} + ${b}) = ${pi}。`
      };
    },
    function betaPosterior(rng) {
      const a = pick(rng, [1, 2, 3, 4]);
      const b = pick(rng, [1, 2, 3, 4]);
      const s = pick(rng, [5, 7, 8, 12]);
      const f = pick(rng, [8, 10, 13, 15]);
      const mean = round((a + s) / (a + b + s + f), 3);
      return {
        id: 'drill-beta', chapter: 'bayesian', title: 'Beta–Binomial 共役事後平均',
        prompt: `成功確率 p の事前分布を Beta(${a}, ${b}) とし、独立試行で成功 ${s} 回・失敗 ${f} 回を観測した。事後平均を小数第3位まで入力してください。`,
        answer: mean, tolerance: 0.005,
        hint: '事後分布は Beta(a+s, b+f)、平均は (a+s)/(a+b+s+f)',
        explanation: `事後は Beta(${a + s}, ${b + f})、平均は ${a + s}/${a + b + s + f} = ${mean}。`
      };
    }
  ];

  function createRng(seed) {
    let t = (seed >>> 0) || 123456789;
    return function () {
      t += 0x6D2B79F5;
      let x = Math.imul(t ^ (t >>> 15), t | 1);
      x ^= x + Math.imul(x ^ (x >>> 7), x | 61);
      return ((x ^ (x >>> 14)) >>> 0) / 4294967296;
    };
  }

  function generate(count = 6, seed = Date.now(), chapter = '') {
    const rng = createRng(seed);
    const pool = chapter ? GENERATORS.filter(fn => fn(createRng(1)).chapter === chapter) : GENERATORS;
    const source = pool.length ? pool : GENERATORS;
    const results = [];
    for (let i = 0; i < count; i++) {
      const fn = source[i % source.length];
      results.push({...fn(rng), instanceId: 'drill-' + seed + '-' + i});
    }
    return results;
  }

  const api = {GENERATORS, createRng, generate};
  if (typeof module === 'object' && module.exports) module.exports = api;
  else global.StatDrills = api;
})(typeof window === 'undefined' ? globalThis : window);
