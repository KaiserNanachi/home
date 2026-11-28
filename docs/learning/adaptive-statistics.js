/* Adapter for the existing statistics notebook and the shared adaptive study UI. */
(function () {
  'use strict';
  const dependencies = {
    dist:['prob'], transform:['prob','dist'], clt:['dist','transform'], estimate:['dist','transform'],
    ci:['clt','estimate'], test:['ci'], categorical:['test'], regression:['estimate','test'],
    glm:['regression'], anova:['test','estimate'], sampling:['clt'], multivariate:['regression'],
    markov:['prob','dist'], timeseries:['markov','regression'], selection:['regression'],
    missing:['estimate'], bayesian:['prob','estimate'], causal:['regression','prob']
  };
  const confusables = {
    prob:['bayesian','causal'],
    dist:['sampling','clt'],
    transform:['clt','estimate'],
    clt:['dist','sampling'],
    estimate:['bayesian','ci'],
    ci:['test','estimate'],
    test:['ci','anova','categorical'],
    categorical:['test','anova','glm'],
    regression:['glm','selection','causal'],
    glm:['regression','categorical'],
    anova:['test','categorical'],
    sampling:['dist','clt'],
    multivariate:['regression','selection'],
    markov:['timeseries'],
    timeseries:['markov','regression'],
    selection:['regression','multivariate'],
    missing:['causal','sampling'],
    bayesian:['estimate','prob'],
    causal:['regression','missing']
  };
  const curated = {
    prob: [
      {id:'prob-recall',type:'explain',axis:'recall',lens:'what',prompt:'条件付き確率 P(A|B) の分母と分子を説明してください。',answer:'P(B)>0 のとき P(A|B)=P(A∩B)/P(B)。',rubric:['分母を P(B) とした','分子を P(A∩B) とした','P(B)>0 に言及した']},
      {id:'prob-transfer',type:'reason',axis:'transfer',lens:'why',prompt:'ある希少疾患の検査で感度が高くても、陽性者の大半が非患者になる状況を作り、理由を説明してください。',answer:'有病率が非常に低く、偽陽性率がゼロでなければ、非患者が多いため偽陽性が真陽性を上回り得る。',rubric:['有病率と感度・偽陽性率を区別した','陽性の内訳を真陽性と偽陽性に分けた','陽性者全体を分母にして説明した']}
    ],
    dist: [
      {id:'dist-compare',type:'explain',axis:'discrimination',lens:'comparison',prompt:'倉庫から不良品数を調べます。復元抽出と非復元抽出で、二項分布と超幾何分布をどう選びますか。',answer:'独立で一定の不良率なら二項分布、有限集団からの非復元抽出なら超幾何分布。',rubric:['復元抽出では各回の確率と独立性を確認した','非復元抽出では残りの構成が変わると説明した']},
      {id:'dist-counterexample',type:'reason',axis:'understanding',lens:'counterexample',prompt:'イベントの発生回数を数える場面でも、ポアソン分布が不適切になり得る反例を一つ挙げて理由を説明してください。',answer:'例：故障が連鎖して同時に複数起きる場合や、時間帯で発生率が大きく変わる場合。独立性や一定発生率の仮定が崩れ、過分散などが生じる。',hints:['ポアソン過程の前提（独立性・一定率・同時発生の希少性）を思い出す'],rubric:['具体的な反例を挙げた','独立性や一定率などの仮定が崩れる理由を説明した']}
    ],
    clt: [
      {id:'clt-transfer',type:'reason',axis:'transfer',lens:'when',prompt:'通話時間の分布が右に歪んでいます。独立な通話を100件集めた標本平均の分布について、中心極限定理を使うときの仮定と限界を説明してください。',answer:'同分布・独立で有限分散などの条件の下、標本平均の標準化分布は大標本で正規に近づく。100件での近似精度は歪みなどに依存する。',rubric:['独立性と有限分散などの条件を挙げた','元のデータでなく標本平均の標準化分布を説明した','100件での近似は保証されないと述べた']}
    ],
    ci: [
      {id:'ci-recall',type:'explain',axis:'recall',lens:'what',prompt:'母分散が未知で正規母集団から独立標本を得たとき、母平均の区間推定で使う統計量と自由度を説明してください。',answer:'(標本平均−母平均)/(標本標準偏差/√n) は自由度 n−1 の t 分布に従う。',rubric:['標本標準偏差を使う標準誤差を示した','t 分布と自由度 n−1 を示した','正規母集団と独立標本の仮定を挙げた']},
      {id:'ci-explain',type:'explain',axis:'understanding',lens:'why',prompt:'「今回得た95%信頼区間に固定された母平均が95%の確率で入る」と言えるか、反復標本抽出の観点から説明してください。',answer:'通常の頻度論では母平均は固定される。手順を反復すると、作られた区間の約95%が母平均を覆う。',rubric:['母平均が固定されると述べた','区間構成手順の長期的被覆率として説明した']}
    ],
    test: [
      {id:'test-recall',type:'explain',axis:'recall',lens:'what',prompt:'p値の定義を、帰無仮説と観測された検定統計量に触れて説明してください。',answer:'帰無仮説の下で、観測値と同程度以上に帰無仮説に不利な検定統計量が得られる確率。',rubric:['帰無仮説の下での確率と述べた','観測値と同程度以上に極端な結果に触れた','帰無仮説が真である確率と混同しなかった']},
      {id:'test-transfer',type:'reason',axis:'transfer',lens:'application',prompt:'新しい教育法の比較で p=0.03 でした。効果が重要かを判断するために、p値以外に何を調べますか。理由も述べてください。',answer:'効果量と信頼区間、研究設計や多重比較、実務上の意味を確認する。p値だけでは効果の大きさや因果性は分からない。',rubric:['効果量と不確実性を検討した','実用的な差や設計上の限界に触れた','p値を帰無仮説の確率と解釈していない']}
    ],
    regression: [
      {id:'regression-transfer',type:'reason',axis:'transfer',lens:'counterexample',prompt:'店舗別の広告費と売上に正の回帰係数が出ました。広告費を増やすと売上が必ず増えると結論できますか。別の説明を一つ挙げてください。',answer:'観察データの係数は条件付き関連であり、店舗規模などが広告費と売上の両方に影響する交絡や逆因果があり得る。',rubric:['係数を関連として解釈した','具体的な交絡または逆因果の仕組みを挙げた','因果効果の識別に追加の設計や仮定が必要と述べた']},
      {id:'regression-method',type:'explain',axis:'discrimination',lens:'comparison',prompt:'説明変数同士の相関が非常に高いとき、通常の最小二乗法（OLS）とリッジ回帰・ラッソ回帰はどう使い分けますか。',answer:'OLS は多重共線性で係数推定の分散が大きくなる。リッジは L2 罰則で係数を縮小して安定化させ、ラッソは L1 罰則で一部係数を0にして変数選択も行う。',hints:['多重共線性が OLS の分散に与える影響を考える','L2 罰則と L1 罰則の違いを比べる'],rubric:['多重共線性による OLS の不安定さに触れた','リッジの縮小とラッソのスパース性（変数選択）を区別した']}
    ],
    anova: [
      {id:'anova-method',type:'explain',axis:'discrimination',lens:'comparison',prompt:'3群の平均差を調べたいとき、2群ずつのt検定を繰り返すのではなく一元配置分散分析や多重比較を使う理由を説明してください。',answer:'無補正でt検定を繰り返すと家族内第一種過誤率が有意水準より膨らむため、全体の差をF検定で確認したり多重比較で補正する。',hints:['検定回数が増えたときの第一種過誤の累積を考える'],rubric:['多重検定による第一種過誤の増加を説明した','全体検定または多重比較による補正の役割を述べた']}
    ],
    sampling: [
      {id:'sampling-example',type:'example',axis:'application',lens:'application',prompt:'大学の学生満足度調査で、層化抽出が有用な設計例を作り、層の分け方と集計時の注意を説明してください。',answer:'学年ごとに層を作って各層から無作為抽出し、全体の推定では母集団の学年構成に応じて重み付けする。',rubric:['具体的な層と各層での抽出方法を示した','母集団の構成に応じた重み付けを説明した','無回答など抽出以外の偏りに配慮した']}
    ],
    selection: [
      {id:'selection-compare',type:'explain',axis:'discrimination',lens:'comparison',prompt:'モデル選択で AIC と BIC は同じモデルを選ばないことがあります。標本数 n が大きいときのペナルティの違いと目的の違いを説明してください。',answer:'AIC はパラメータ1個あたり 2、BIC は log n のペナルティを課す。n≥8 では BIC の方が複雑なモデルに厳しく、真のモデルの選択を重視する傾向があり、AIC は予測精度の観点に基づく。',hints:['AIC の 2k と BIC の k log n を比べる'],rubric:['ペナルティ 2k と k log n の違いを示した','n が大きいほど BIC が単純なモデルを選びやすいこと等を説明した']}
    ],
    causal: [
      {id:'causal-transfer',type:'reason',axis:'transfer',lens:'when',prompt:'運動 A と健康 Y の関係を調べる。年齢 C は運動と健康の両方に影響し、血圧 M は運動の影響を受け健康に影響する。総効果を推定するとき、C と M をどう扱いますか。',answer:'年齢は交絡経路を遮断するため調整候補。血圧は媒介変数なので総効果の推定では通常調整しない。他の交絡などにも仮定が必要。',rubric:['C を交絡因子として説明した','M を媒介変数とし総効果での調整を避けた','他の交絡や識別条件に言及した']}
    ]
  };
  // Each pair tests retrieval and interpretation separately, without relying on a
  // chapter title as the answer. Rubrics remain visible for self-assessment.
  const foundations = {
    prob: [
      ['P(B)>0 のとき P(A|B) を交差確率で表してください。','P(A∩B)/P(B)。','交差確率を分子にした|条件 B の確率を分母にした','条件となる事象を確認する|分母は条件が成立する確率'],
      ['確率がともに正の二事象が排反なら、独立ですか。理由も説明してください。','独立ではない。交差確率は 0 だが確率の積は正。','排反なら交差確率が 0|独立の条件は確率の積との一致','交差確率を考える|独立なら積と一致する']
    ],
    dist: [
      ['独立な n 回の試行で成功確率 p が一定のとき、成功回数の平均と分散を示してください。','平均 np、分散 np(1−p)。','平均が np|分散が np(1−p)','各試行はベルヌーイ変数|分散には失敗確率も入る'],
      ['有限集団から非復元で不良品を数える場合、独立な反復試行のモデルと何が違いますか。','残りの構成が抽出のたびに変わり、抽出結果は一般に独立ではない。','残りの構成の変化|独立性が一般に成り立たない','一つ取り出した後を考える|次の不良確率が変わり得る']
    ],
    transform: [
      ['Var(aX+bY) を分散と共分散で表してください。','a²Var(X)+b²Var(Y)+2abCov(X,Y)。','係数を二乗した分散項|2ab 倍の共分散項','二乗を展開する|交差項を含める'],
      ['無相関なら独立だと一般に結論できますか。同時正規の場合にも触れてください。','一般には結論できない。同時正規なら無相関は独立を意味する。','一般には逆が成り立たない|同時正規での例外','独立から無相関への向きと区別する|同時正規という追加条件を考える']
    ],
    clt: [
      ['独立同分布で平均 μ、有限の正の分散 σ² を持つ標本平均を、中心極限定理のために標準化してください。','√n(X̄−μ)/σ は標準正規分布に分布収束する。','平均を引く|標準誤差 σ/√n で割る|分布収束を示した','標本平均の標準誤差は何か|その標準誤差で中心化した平均を割る'],
      ['大数の法則と中心極限定理は、標本平均についてそれぞれ何を主張しますか。','大数の法則は平均への収束、中心極限定理は標準化した誤差の分布収束を述べる。','標本平均の収束先|標準化誤差の分布を区別','片方は値の安定性|もう片方は誤差の分布']
    ],
    estimate: [
      ['推定量 T の平均二乗誤差を分散とバイアスで分解してください。','MSE(T)=Var(T)+(E[T]−θ)²。','分散項|バイアスの二乗','真の値との差を中心化して展開|交差項の期待値は 0'],
      ['不偏な推定量は標本数を増やすと必ず真値に収束しますか。理由を述べてください。','必ずしも収束しない。不偏性は各標本数で期待値が真値という性質で、一致性には誤差が確率的に消えることが必要。','不偏性と一致性の定義を区別|不偏性だけでは収束を保証しない','期待値が等しいこととばらつきを区別|分散が減らない場合を考える']
    ],
    ci: [
      ['母標準偏差 σ 既知の正規母集団から独立標本を取るとき、母平均の両側 95% 信頼区間を式で示してください。','X̄±1.96σ/√n。','中心が標本平均|誤差幅が 1.96σ/√n','標本平均の標準誤差を使う|標準正規分布の両側 2.5% 点'],
      ['同じ方法で標本数を4倍にすると、σ既知の平均の区間幅はどう変わりますか。','他の条件が同じなら標準誤差も区間幅も半分になる。','幅が 1/√n に比例|4倍で半分','幅は標準誤差に比例|平方根を取る']
    ],
    test: [
      ['第一種の過誤と第二種の過誤を、それぞれ仮説が真か棄却したかで説明してください。','第一種は真の帰無仮説を棄却、第二種は偽の帰無仮説を棄却しないこと。','第一種の定義|第二種の定義','真偽と判定の組合せを考える|棄却と非棄却を分ける'],
      ['有意水準を一定にしたまま標本数を増やすと、同じ固定された対立仮説に対する検出力は一般にどう変わりますか。','適切な通常の検定では標準誤差が小さくなり、固定された差を検出する力は一般に高まる。','固定した差と有意水準を前提にした|標準誤差と検出力の関係を説明','標本平均の標準誤差を考える|差を雑音と比べる']
    ],
    categorical: [
      ['2×2表の各セルを a,b,c,d としたとき、オッズ比を示してください。','ad/(bc)。ただし分母が 0 でない場合。','対角の積の比|分母が 0 でない条件','各行のオッズを作る|二つのオッズの比を取る'],
      ['同じ人の処置前後の順位を比較するとき、独立な二群の順位検定を使う場合と何が違いますか。','同一人内の対応を利用する。対応のある方法は個人ごとの差を扱い、独立二群の方法とは仮定が異なる。','同一人内の対応|差を用いる方法と独立群の仮定の違い','観測が誰に属するか確認|各人の前後差を考える']
    ],
    regression: [
      ['切片を含む単回帰の傾きの最小二乗推定量を、中心化した x と y を使って示してください。','Σ(xᵢ−x̄)(yᵢ−ȳ)/Σ(xᵢ−x̄)²。分母は正とする。','中心化した積の和が分子|x の中心化平方和が分母','残差平方和を傾きで微分|x の変動が必要'],
      ['決定係数が高いだけで、予測が将来も正確と言えますか。','言えない。訓練データへの適合は未知データへの汎化を保証せず、外部または適切な分割で評価する必要がある。','訓練適合と汎化の区別|未使用データでの評価','データを合わせた対象を確認|未知データで再評価する']
    ],
    glm: [
      ['logit(p)=β₀+β₁x で x が1増えたとき、オッズは何倍になりますか。','exp(β₁) 倍。','係数を指数変換|変化するのはオッズ','logit は対数オッズ|対数の差を指数に戻す'],
      ['追跡終了時まで発症しなかった人の記録を、発症時刻として扱うと何が問題ですか。','右打ち切りであり、発症時刻が終了時刻より後であることしか分からない。終了時を発症時刻とすると時間を誤記する。','右打ち切りの意味|終了時刻と発症時刻を区別','発症を実際に観測したか|終了時より後という情報だけがある']
    ],
    anova: [
      ['一元配置で k 群・総観測数 N のとき、群間と群内の自由度を示してください。','群間 k−1、群内 N−k。','群間 k−1|群内 N−k','全体の自由度は N−1|群ごとに平均を推定する'],
      ['二要因の交互作用があるとは、主効果だけのモデルと比べて何が異なりますか。','一方の要因の効果がもう一方の水準によって変わる。','一方の効果が他方の水準で変化|加法的な一定効果との違い','差を各水準で比較|差が一定でない場合を考える']
    ],
    sampling: [
      ['有限母集団 N から n を非復元で単純無作為抽出するとき、分母 N の母分散を用いた平均の標準誤差に掛かる有限修正を示してください。','√((N−n)/(N−1))。','分母 N の母分散を前提にした|有限修正 √((N−n)/(N−1))','全数抽出で抽出誤差は 0|母分散の分母の定義に注意'],
      ['層化抽出と集落抽出では、分けたグループをどのように抽出に使いますか。','層化は各層から標本を取る。集落抽出は集落を抽出し、その中の単位を調べる。','各層から抽出|集落そのものを抽出','層は全て使うか|集落は一部を選ぶ']
    ],
    multivariate: [
      ['標準化した2変数の相関行列の固有値が 1.6 と 0.4 のとき、第1主成分の寄与率はいくつですか。','1.6/(1.6+0.4)=0.8、80%。','第1固有値を全固有値の和で割る|80%','固有値は各成分の分散|全分散に対する比を取る'],
      ['教師あり分類とクラスタリングは、学習時に利用する情報と目的がどう違いますか。','教師あり分類はラベル付きデータを使い未知例のクラスを予測する。クラスタリングは通常ラベルなしで類似性に基づく群を探す。','教師ありの正解ラベル|クラスタリングの群探索','既知の正解があるか|予測と構造探索を区別']
    ],
    markov: [
      ['行ベクトル π と行確率行列 P について、定常分布が満たす式を示してください。','πP=π、各成分は非負で和は1。','πP=π|確率ベクトルの条件','1ステップ更新を考える|更新しても変化しない'],
      ['定常分布が存在するだけで、任意の初期分布からそこに収束すると言えますか。','必ずしも言えない。例えば周期性がある連鎖では分布が振動し得る。','定常性と収束を区別|周期性などの反例や追加条件','更新で不変な分布と初期値からの挙動を分ける|二状態を交互に移る連鎖を考える']
    ],
    timeseries: [
      ['AR(1) 過程 Xₜ=φXₜ₋₁+εₜ が定常となる代表的条件とラグ k の自己相関を示してください。','白色雑音の下で |φ|<1、自己相関は φᵏ。','|φ|<1|ラグ k の自己相関 φᵏ','衝撃が時間とともに減衰する条件|1期ごとに φ を掛ける'],
      ['将来予測を評価するとき、時系列を無作為に混ぜて分割すると何が問題になり得ますか。','未来の情報が学習側に入り、実際の予測時より楽な評価になる。時間順に訓練と検証を分ける。','未来情報の漏れ|時間順の評価','予測時点で使える情報を考える|検証期間を訓練期間より後に置く']
    ],
    selection: [
      ['最大対数尤度 ℓ と推定パラメータ数 k による AIC の式を示してください。','AIC=−2ℓ+2k。','−2ℓ|複雑さへの罰則 2k','適合は対数尤度|パラメータ数に比例する罰則'],
      ['訓練誤差の小さい複雑なモデルが、未知データでも最良とは限らない理由を説明してください。','標本固有の雑音にも適合する過学習が起こり、未知データで誤差が増える場合がある。','標本固有の雑音への適合|未知データでの誤差','訓練データの偶然の変動を考える|適合と汎化を分ける']
    ],
    missing: [
      ['MCAR、MAR、MNAR を欠測確率が依存する情報の観点で区別してください。','MCAR は観測・欠測値に依存せず、MAR は観測値を条件とすれば欠測値に依存しない。MNAR はその条件でも欠測値に依存する。','MCAR の独立性|MAR の観測値による条件付け|MNAR の残る欠測値依存','観測されている値で条件付ける|その後も欠測値自体に依存するか'],
      ['測定上限を超えると値が欠測する機器で、観測値の平均だけを使うとどんな偏りが生じ得ますか。','高い値が欠けるため、全体平均を過小評価する可能性がある。','高い値が選択的に欠ける|平均への下方向の影響','どの値が観測されないか|残った値の分布を考える']
    ],
    bayesian: [
      ['成功確率 p の事前分布が Beta(a,b)、独立試行で成功 s・失敗 f のとき事後分布を示してください。','Beta(a+s,b+f)。','成功数を a に加える|失敗数を b に加える','尤度に p の成功数乗が現れる|1−p の失敗数乗も合わせる'],
      ['bootstrap と MCMC はそれぞれ何を近似するために使いますか。','bootstrap は元標本からの再標本化で統計量の標本分布などを近似する。MCMC は所望の分布を定常分布とする連鎖でその分布からの標本を近似的に得る。','再標本化による標本分布|連鎖による目標分布からの標本','データを再抽出する方法|確率分布を連鎖で探索する方法']
    ],
    causal: [
      ['共通原因 C が処置 A と結果 Y の双方に影響する DAG のバックドア経路を記してください。','A←C→Y。C に条件付けることでこの経路を遮断できる。','A←C→Y|C による遮断','処置への矢印から入る経路|共通原因をたどる'],
      ['A→M→Y の媒介変数 M に調整すると、A の Y への総効果を評価する際に何が問題ですか。','媒介経路を遮断するため、総効果のうち M を通る成分を除いてしまう。','M を通る経路の遮断|総効果の一部が除かれる','調整すると何の経路が閉じるか|総効果は直接・間接の経路を含む']
    ]
  };
  const curatedHints = {
    'prob-recall':['条件となる事象に注目','同時に起きる確率を使う'],
    'prob-transfer':['患者と非患者の人数を置く','各集団の陽性を比べる'],
    'dist-compare':['抽出後の集団の構成を考える','各回の確率が一定か確認'],
    'clt-transfer':['個々の通話と平均を区別','有限分散と近似精度を確認'],
    'ci-recall':['標準誤差に標本標準偏差を使う','平均を推定した自由度を考える'],
    'ci-explain':['母平均は固定か確率変数か','区間を何度も作ることを考える'],
    'test-recall':['帰無仮説を仮定する','観測結果以上に不利な結果を考える'],
    'test-transfer':['効果の大きさを考える','推定の幅と研究設計を確認'],
    'regression-transfer':['両方に影響する店舗の特徴を考える','関連と介入の結果を区別'],
    'sampling-example':['異なる集団に分ける','母集団構成と標本構成を比べる'],
    'causal-transfer':['共通原因と媒介変数を区別','総効果に含まれる経路を確認']
  };
  const additional = {
    prob: [
      {id:'prob-multiple',type:'multiple',axis:'discrimination',lens:'comparison',prompt:'P(A)>0、P(B)>0 で、A と B が排反のとき必ず成り立つものをすべて選んでください。',options:['P(A∩B)=0','P(A∪B)=P(A)+P(B)','A と B は独立','P(A|B)=0'],answer:[0,1,3],hints:['排反は同時に起こらない','和の公式と条件付き確率の分母を確認'],feedback:'交差確率は 0 で、和の公式は単純な和になる。確率がともに正なら独立ではない。'},
      {id:'prob-ex1',type:'numeric',axis:'application',lens:'how',prompt:'故障率10%の設備に、感度80%・特異度90%の独立な検査を2回行い、2回とも陽性だった。この設備が故障している事後確率を小数第3位まで入力してください。',answer:0.877,tolerance:0.001,hints:['2回陽性の条件付き確率は故障時 0.8²、正常時 0.1²','事前確率 0.1 と 0.9 を掛けてベイズ更新する'],feedback:'0.1×0.64 / (0.1×0.64 + 0.9×0.01) = 0.064 / 0.073 ≈ 0.877。独立検査を重ねると事後確率が大きく上がる。'}
    ],
    dist: [
      {id:'dist-ex1',type:'numeric',axis:'application',lens:'how',prompt:'X∼Poisson(λ=2) とする。e⁻²≈0.1353 を用いて P(X≤1) を小数第3位まで入力してください。',answer:0.406,tolerance:0.002,hints:['P(X=0)=e⁻²、P(X=1)=2e⁻²','X=0 と X=1 の確率を足す'],feedback:'P(X≤1)=e⁻²(1+2)=3e⁻²≈0.406。'},
      {id:'dist-ex2',type:'choice',axis:'recall',lens:'what',prompt:'成功確率 p=0.25 の独立試行で、3回成功するまでの「失敗回数」X（負の二項分布）の期待値はどれか。',options:['3','6','9','12','15'],answer:2,hints:['失敗回数の期待値は r(1−p)/p','r=3, p=0.25 を代入する'],feedback:'E[X]=r(1−p)/p = 3×0.75/0.25 = 9。試行数なら r/p=12 なので定義の区別に注意。'}
    ],
    transform: [
      {id:'transform-ex1',type:'numeric',axis:'application',lens:'how',prompt:'X∼U(0,1) のとき、変数変換 Y = −(1/2) log X で得られる確率変数 Y の期待値 E[Y] を小数で入力してください。',answer:0.5,tolerance:1e-6,hints:['−log X は Exp(1) に従う','Y は率 2 の指数分布 Exp(2) に従う'],feedback:'Y∼Exp(2) なので平均は 1/2 = 0.5。逆関数法による乱数生成の基本変換。'},
      {id:'transform-ex2',type:'numeric',axis:'application',lens:'how',prompt:'3変数の相関係数が r_XY=0.6、r_XZ=0.5、r_YZ=0.5 のとき、Z の線形な影響を除いた偏相関係数 r_{XY·Z} を小数第3位まで入力してください。',answer:0.467,tolerance:0.001,hints:['分子は r_XY − r_XZ r_YZ','分母は √{(1−r_XZ²)(1−r_YZ²)}'],feedback:'(0.6 − 0.25) / √(0.75×0.75) = 0.35 / 0.75 ≈ 0.467。'}
    ],
    clt: [
      {id:'clt-ex1',type:'numeric',axis:'application',lens:'how',prompt:'独立同分布の標本平均 X̄ について √n(X̄−μ) →d N(0, σ²) が成り立ち、μ=2、σ²=4 とする。デルタ法による √n(1/X̄ − 1/μ) の漸近分散を小数で入力してください。',answer:0.25,tolerance:1e-6,hints:['g(x)=1/x の導関数は g′(x)=−1/x²','漸近分散は {g′(μ)}² σ²'],feedback:'g′(2)=−1/4 なので (−1/4)² × 4 = 0.25。'}
    ],
    estimate: [
      {id:'estimate-ex1',type:'numeric',axis:'application',lens:'how',prompt:'X₁,…,X₂₅ が独立に Poisson(λ) に従い、真値 λ=4 とする。λ の任意の不偏推定量が満たすべきクラーメル・ラオの下限（分散の下限）を小数で入力してください。',answer:0.16,tolerance:1e-6,hints:['Poisson(λ) 1観測の Fisher 情報量は 1/λ','n 観測では n/λ、下限はその逆数 λ/n'],feedback:'I_n(λ)=n/λ=25/4 なので CRLB = λ/n = 4/25 = 0.16。標本平均 X̄ がこの下限を達成する。'},
      {id:'estimate-ex2',type:'choice',axis:'discrimination',lens:'comparison',prompt:'正規母集団 N(μ, σ²)（μ, σ² とも未知）からの独立標本に対する標本分散について正しいものはどれか。',options:['最尤推定量は n で割り不偏である','最尤推定量は n−1 で割り不偏である','最尤推定量は n で割り一致推定量だが有限標本では過小バイアスをもつ','不偏分散は常に最尤推定量より MSE が小さい','十分統計量は存在しない'],answer:2,hints:['MLE は Σ(Xi−X̄)²/n','n→∞ でバイアスは 0 に近づく'],feedback:'σ² の MLE は n 分母で E[σ̂²]=(n−1)σ²/n となり過小バイアスをもつが、一致推定量である。'}
    ],
    ci: [
      {id:'ci-ex1',type:'numeric',axis:'application',lens:'when',prompt:'母比率 p の両側95%信頼区間の誤差幅（半幅）を 0.05 以下にしたい。p(1−p)≤0.25 と z_{0.975}=1.96 を用いた保守的な最小必要標本数（整数）を入力してください。',answer:385,tolerance:0,hints:['1.96 × √(0.25/n) ≤ 0.05 を n について解く','端数は切り上げて整数にする'],feedback:'n ≥ (1.96/0.05)² × 0.25 = 384.16 より最小の整数は 385。'}
    ],
    test: [
      {id:'test-boolean',type:'boolean',axis:'discrimination',lens:'counterexample',prompt:'p値 0.04 は、帰無仮説が真である確率が 4% であることを意味する。',answer:false,hints:['p値は何を仮定して計算するか','確率を取る対象はデータか仮説か'],feedback:'p値は帰無仮説の下で観測値以上に不利な結果が出る確率であり、仮説の真偽の確率ではない。'},
      {id:'test-ex1',type:'numeric',axis:'application',lens:'how',prompt:'5個の仮説検定を同時に行い、Bonferroni 法で家族内第一種過誤率（FWER）を 0.05 以下に制御するとき、各検定に用いる有意水準を小数で入力してください。',answer:0.01,tolerance:1e-6,hints:['有意水準 α を検定数 m で割る'],feedback:'α/m = 0.05 / 5 = 0.01。検定数が増えると検出力が下がるため、Holm 法や FDR（Benjamini–Hochberg 法）も検討される。'},
      {id:'test-ex2',type:'choice',axis:'discrimination',lens:'comparison',prompt:'尤度比検定・Wald 検定・スコア検定の特徴として正しい組み合わせはどれか。',options:['スコア検定は対立仮説の下での MLE が必須','Wald 検定は帰無仮説の下での勾配のみを使う','スコア検定は帰無仮説の下での推定だけで計算できる','尤度比検定は必ず有限標本で正確分布に従う','3手法は正則条件の下でも漸近的に一致しない'],answer:2,hints:['Score は H0 での対数尤度の傾き（スコア関数）を使う'],feedback:'スコア検定は帰無仮説下の推定のみで計算でき、正則条件の下で3手法は帰無仮説下で漸近的に同じ χ² 分布に従う。'}
    ],
    categorical: [
      {id:'categorical-ex1',type:'numeric',axis:'application',lens:'how',prompt:'【分割表読み取り】2×2表が 第1行(30, 20)・第2行(10, 40)（総数100）である。無補正の Pearson χ² 統計量を小数第2位まで入力してください。',answer:16.67,tolerance:0.02,hints:['行合計は(50,50)、列合計は(40,60)なので期待度数は(20,30; 20,30)','各セルの (O−E)²/E を4つ足す'],feedback:'10²/20 + 10²/30 + 10²/20 + 10²/30 = 5 + 3.333 + 5 + 3.333 = 16.67（df=1）。'}
    ],
    regression: [
      {id:'regression-ex1',type:'numeric',axis:'application',lens:'how',prompt:'【回帰出力表読み取り】n=43 の重回帰出力：\nIntercept: Est=10.2, SE=2.0\nx1: Est=2.40, SE=0.60\nx2: Est=-1.50, SE=0.50\nResidual SE=3.0 (df=40), R²=0.64\nx1 の係数が 0 である仮説の t 統計量を入力してください。',answer:4,tolerance:1e-6,hints:['t統計量は 推定値 / 標準誤差（Est / SE）'],feedback:'t = 2.40 / 0.60 = 4.0（自由度 40）。切片と説明変数2個で p=3、df=43−3=40。'},
      {id:'regression-ex2',type:'numeric',axis:'application',lens:'when',prompt:'重回帰で説明変数 x₁ を他の説明変数で回帰した決定係数が R₁²=0.80 のとき、x₁ の分散拡大係数（VIF）を入力してください。',answer:5,tolerance:1e-6,hints:['VIF = 1 / (1 − R₁²)'],feedback:'VIF = 1 / (1 − 0.80) = 5。一般に VIF が大きいほど多重共線性により係数推定の分散が拡大する。'}
    ],
    glm: [
      {id:'glm-ex1',type:'numeric',axis:'application',lens:'how',prompt:'【GLM出力読み取り】ロジスティック回帰の推定式が logit(p) = −1.5 + 0.5x₁ であった。x₁=3 のときの予測確率 p を小数で入力してください。',answer:0.5,tolerance:1e-6,hints:['線形予測子 η = −1.5 + 0.5×3 を計算する','p = 1 / (1 + exp(−η))'],feedback:'η = 0 なので p = 1 / (1 + e⁰) = 0.5。'},
      {id:'glm-ex2',type:'numeric',axis:'application',lens:'how',prompt:'【生存時間表】時点1：リスク集合10・イベント2。その後時点2までに2名が打ち切り。時点2：リスク集合6・イベント1。時点2直後の Kaplan–Meier 生存確率を小数第3位まで入力してください。',answer:0.667,tolerance:0.002,hints:['時点1の生存率は 1−2/10=0.8','時点2の条件付き生存率は 1−1/6=5/6（打ち切り者は分母から除く）'],feedback:'0.8 × (5/6) = 4/6 ≈ 0.667。打ち切りはイベントとせず次時点のリスク集合から除外する。'},
      {id:'glm-ex3',type:'choice',axis:'discrimination',lens:'comparison',prompt:'Cox 比例ハザードモデルで説明変数 x の係数が β=0.693（exp(0.693)≈2）であるとき、適切な解釈はどれか。',options:['生存確率が常に2倍になる','他の変数が同じなら、任意の時点でハザード比が約2倍と仮定される','生存時間の中央値が2倍になる','ベースラインハザード h₀(t) を必ず指数分布と仮定する','打ち切りデータは解析から除外される'],answer:1,hints:['Cox モデルは h(t|x)=h₀(t)exp(Xβ) で、h₀(t) の形を特定しない半パラメトリックモデル'],feedback:'他の共変量を固定して x が1増えるとハザードが exp(β)≈2 倍になる。比例ハザード性の確認が重要。'}
    ],
    anova: [
      {id:'anova-ex1',type:'numeric',axis:'application',lens:'how',prompt:'【ANOVA表穴埋め】4群・総観測数 N=24 の一元配置分散分析表で、総平方和 SST=114、群内平方和 SSW=60 が分かっている。F 統計量を入力してください。',answer:6,tolerance:1e-6,hints:['SSB = SST − SSW = 54、自由度は群間 4−1=3、群内 24−4=20','MSB = SSB/3、MSW = SSW/20、F = MSB/MSW'],feedback:'MSB = 54/3 = 18、MSW = 60/20 = 3、F = 18/3 = 6。'},
      {id:'anova-ex2',type:'choice',axis:'discrimination',lens:'comparison',prompt:'一部実施要因計画（Fractional Factorial Design）で生じる「交絡（alias）」の説明として正しいものはどれか。',options:['無作為化により誤差がゼロになること','実験数を減らした代わりに、主効果と交互作用など一部の効果が互いに分離して推定できなくなること','必ずすべての2因子交互作用が独立に推定できること','ブロック化すると必ず交互作用が消えること','直交配列では水準数を揃えなくてよいこと'],answer:1,hints:['全水準の組合せを行わないため、設計行列の列が一致する効果が生じる'],feedback:'一部実施ではコストを抑えられる反面、高次交互作用と主効果などが別名（alias）関係になり分離できなくなる。'}
    ],
    sampling: [
      {id:'sampling-ex1',type:'numeric',axis:'application',lens:'how',prompt:'層化抽出で総標本数 n=80 をネイマン最適配分する。第1層：N₁=200, S₁=10。第2層：N₂=100, S₂=60。第2層に割り当てる標本数 n₂ を入力してください。',answer:60,tolerance:1e-6,hints:['ネイマン配分は n_h ∝ N_h S_h','N₁S₁=2000、N₂S₂=6000 の比で 80 を配分する'],feedback:'n₂ = 80 × 6000 / (2000 + 6000) = 60。層サイズだけでなく層内標準偏差が大きい層に多く配分する。'}
    ],
    multivariate: [
      {id:'multivariate-ex1',type:'numeric',axis:'application',lens:'how',prompt:'【因子分析出力】直交回転後の2因子モデルで、ある標準化変数の因子負荷量が第1因子 0.8、第2因子 0.3 であった。この変数の共通性（communality）を小数第2位まで入力してください。',answer:0.73,tolerance:0.005,hints:['直交因子モデルの共通性は各因子負荷量の二乗和'],feedback:'h² = 0.8² + 0.3² = 0.64 + 0.09 = 0.73。独自性（特殊分散）は 1 − 0.73 = 0.27。'},
      {id:'multivariate-ex2',type:'numeric',axis:'application',lens:'how',prompt:'【混同行列】TP=40、FP=10、FN=30、TN=20 の分類結果について、F1 スコアを小数第3位まで入力してください。',answer:0.667,tolerance:0.002,hints:['F1 = 2·TP / (2·TP + FP + FN)'],feedback:'F1 = 80 / (80 + 10 + 30) = 80/120 ≈ 0.667（適合率 0.8、再現率 4/7 の調和平均）。'},
      {id:'multivariate-ex3',type:'choice',axis:'discrimination',lens:'comparison',prompt:'因子分析におけるバリマックス（Varimax）回転とプロマックス（Promax）回転の違いとして正しいものはどれか。',options:['バリマックスは斜交回転、プロマックスは直交回転','バリマックスは因子間の直交を保ち、プロマックスは因子間の相関を許す斜交回転','どちらも因子負荷量の合計を0にする','プロマックスは共通性を変える','バリマックスは必ず第1因子に全分散を集中させる'],answer:1,hints:['直交回転（因子無相関）か斜交回転（因子相関あり）かの区別'],feedback:'バリマックスは直交回転で解釈しやすい単純構造を目指し、プロマックスは因子間相関を許容する斜交回転である。'}
    ],
    markov: [
      {id:'markov-ex1',type:'numeric',axis:'application',lens:'how',prompt:'2状態マルコフ連鎖の遷移行列が 1行目(0.7, 0.3)、2行目(0.4, 0.6) である。初期状態が状態1のとき、2期後に状態1にいる確率を小数第2位まで入力してください。',answer:0.61,tolerance:0.005,hints:['P² の (1,1) 成分を計算する','1→1→1 と 1→2→1 の経路の確率を足す'],feedback:'0.7×0.7 + 0.3×0.4 = 0.49 + 0.12 = 0.61。'}
    ],
    timeseries: [
      {id:'timeseries-ex1',type:'choice',axis:'discrimination',lens:'comparison',prompt:'【ACF/PACF 読み取り】定常な時系列データで、標本 ACF がラグ1,2,3…と指数的に減衰し、標本 PACF がラグ2まで有意でラグ3以降ほぼ 0 に打ち切られた。最も適合候補となるモデルはどれか。',options:['MA(2)','AR(2)','ホワイトノイズ','ランダムウォーク','MA(1)'],answer:1,hints:['AR(p) は PACF がラグ p で打ち切り・ACF は減衰、MA(q) は ACF がラグ q で打ち切り'],feedback:'PACF がラグ2で打ち切られ ACF が減衰するのは AR(2) の典型的パターン。'}
    ],
    selection: [
      {id:'selection-ex1',type:'choice',axis:'application',lens:'comparison',prompt:'【モデル比較表】同一データで推定した3モデル：\nモデル1：k=2, ℓ=−100\nモデル2：k=4, ℓ=−95\nモデル3：k=6, ℓ=−94\nAIC（=−2ℓ+2k）が最小となるモデルはどれか。',options:['モデル1（AIC=204）','モデル2（AIC=198）','モデル3（AIC=200）','モデル1と3が同率最小','データ数 n がないと AIC は計算できない'],answer:1,hints:['AIC = −2ℓ + 2k を各モデルで計算する'],feedback:'モデル1: 204、モデル2: 198、モデル3: 200 よりモデル2が最小。AIC は n を直接は含まない（BIC には log n が入る）。'}
    ],
    missing: [
      {id:'missing-ex1',type:'numeric',axis:'application',lens:'how',prompt:'多重代入法（M=5 回）で推定したパラメータについて、代入データ内分散の平均が W̄=1.0、代入間の推定値分散が B=0.5 であった。Rubin の統合規則による総分散 T を小数で入力してください。',answer:1.6,tolerance:1e-6,hints:['T = W̄ + (1 + 1/M) B'],feedback:'T = 1.0 + (1 + 1/5)×0.5 = 1.0 + 0.6 = 1.6。代入の不確実性を群間分散 B で反映する。'}
    ],
    bayesian: [
      {id:'bayesian-blank',type:'blank',axis:'recall',lens:'how',prompt:'成功確率に Beta(2,3) の事前分布を置き、成功7回・失敗13回を観測した。事後分布の2つの形状パラメータを「第1パラメータ,第2パラメータ」の形式で記入してください。',answer:'9,16',hints:['成功数を第1パラメータに加える','失敗数を第2パラメータに加える'],feedback:'事後分布は Beta(2+7, 3+13)=Beta(9,16)。'},
      {id:'bayesian-ex1',type:'numeric',axis:'application',lens:'how',prompt:'Poisson(λ) の母数 λ に共役事前分布 Gamma(a=2, b=1)（b は率パラメータ）を置き、独立観測 x₁=3, x₂=5 を得た。事後平均を小数第2位まで入力してください。',answer:3.33,tolerance:0.01,hints:['事後分布は Gamma(a+Σx, b+n)','Gamma(α,β) の平均は α/β'],feedback:'事後分布は Gamma(2+8, 1+2)=Gamma(10,3)、事後平均は 10/3 ≈ 3.33。'},
      {id:'bayesian-ex2',type:'choice',axis:'discrimination',lens:'comparison',prompt:'MCMC の収束診断と実務上の扱いとして適切なものはどれか。',options:['最初の1サンプルから直ちに事後平均を計算する','バーンイン（ウォームアップ）期間を捨て、複数連鎖の R̂ が 1 に近いか確認する','サンプル数を増やしても自己相関は一切影響しない','事後分布は常に正規分布になる','95%信用区間と頻度論の95%信頼区間は定義が同一である'],answer:1,hints:['初期値の影響を除く期間と、複数チェーンの一致度を確認する'],feedback:'バーンインを除き、トレースプロットや Gelman–Rubin 統計量 R̂≈1、有効サンプルサイズ（ESS）を確認する。'}
    ],
    causal: [
      {id:'causal-ex1',type:'numeric',axis:'application',lens:'how',prompt:'【差分の差分法（DID）】処置群の平均は介入前 50 → 介入後 68、対照群の平均は介入前 40 → 介入後 46 であった。平行トレンド仮定の下での DID 推定値を入力してください。',answer:12,tolerance:1e-6,hints:['(処置群の前後差) − (対照群の前後差)'],feedback:'(68−50) − (46−40) = 18 − 6 = 12。平行トレンド仮定が成り立たない場合はバイアスが生じる。'},
      {id:'causal-ex2',type:'multiple',axis:'discrimination',lens:'when',prompt:'操作変数法（IV）で操作変数 Z が満たすべき代表的な条件（識別仮定）をすべて選んでください。',options:['関連性：Z は処置 A に影響を与える','除外制約：Z は A を通じてのみ結果 Y に影響する','独立性：Z は A と Y の未測定交絡因子と独立である','Z は結果 Y の後に決まる変数である'],answer:[0,1,2],hints:['Z→A の強さ、Z→Y の直接経路がないこと、交絡との独立性を確認'],feedback:'操作変数 Z には関連性・除外制約・独立性（交換可能性）が必要。'}
    ]
  };
  function foundationQuestions(id) {
    return (foundations[id] || []).map((row, index) => ({
      id:id+'-foundation-'+index, type:'explain', axis:index ? 'understanding' : 'recall',
      lens:index ? 'why' : 'what',
      prompt:row[0], answer:row[1], rubric:row[2].split('|'), hints:row[3].split('|')
    }));
  }
  function plain(value) {
    const el = document.createElement('div');
    el.innerHTML = String(value == null ? '' : value);
    return el.textContent || '';
  }
  function evidence(q, answer) {
    if (!answer || typeof answer.correct !== 'boolean') return null;
    const at = Date.parse(answer.at);
    if (!Number.isFinite(at)) return null;
    return {id:'statistics-legacy-'+q.id+'-'+at, itemId:q.m, questionId:q.id, at:at, correct:answer.correct, response:String(answer.value ?? '')};
  }
  window.createStatisticsAdaptiveCourse = function (chapters, questions, state) {
    return {
      id:'statistics-pre1', title:'統計学 準1級',
      description:'既存の章と演習に加え、原理の自己説明・反例検討・類似手法の識別と初見への応用を段階的に鍛えます。',
      getScope:()=>state().scope,
      topics:chapters.map(m=>({id:m.id,title:m.title})),
      items:chapters.map(m=>({
        id:m.id, topic:m.id, title:m.title, kind:'skill', importance:3,
        prerequisites:dependencies[m.id] || [], confusables:confusables[m.id] || [], group:m.group,
        ...(m.scope === 'new' ? {scope:'2027'} : {}),
        example:plain(m.example), reference:'#'+m.id+'/lesson',
        questions:questions.filter(q=>q.m===m.id).map(q=>({
          id:q.id, type:q.type==='mc'?'choice':'numeric', axis:'application', lens:'application', prompt:plain(q.t),
          answer:q.a, ...(q.type==='mc'?{options:q.o.map(plain)}:{check:response=>String(response).trim()!=='' && Number.isFinite(Number(response)) && Math.abs(Number(response)-q.a)<=q.tol}),
          hints:[plain(q.h)], feedback:plain(q.e)
        })).concat(foundationQuestions(m.id), additional[m.id] || [], (curated[m.id] || []).map(q=>({...q, hints:q.hints || curatedHints[q.id]})))
      })),
      legacy:questions.map(q=>evidence(q,state().answers[q.id])).filter(Boolean)
    };
  };
  window.statisticsAdaptiveEvidence = evidence;
}());
