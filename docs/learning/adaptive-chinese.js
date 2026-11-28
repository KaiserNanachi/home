'use strict';
(() => {
  const root = document.getElementById('adaptive-study');
  const rows = window.CHINESE_VOCABULARY;
  if (!root || !Array.isArray(rows) || !window.AdaptiveStudy) {
    if (root) root.textContent = 'おすすめ学習を読み込めませんでした。単語一覧や単語テストをご利用ください。';
    return;
  }
  // Source correction: 地铁 is dìtiě. Leave the original vocabulary file intact.
  const valid = rows.filter(row => row[0] && row[1] && row[3] && row[4] && row[5])
    .map(row => row[3] === '地铁' && row[4] === 'detiě' ? Object.assign([...row], {4: 'dìtiě'}) : row);
  const meanings = new Map();
  for (const row of valid) {
    if (!meanings.has(row[3])) meanings.set(row[3], new Set());
    meanings.get(row[3]).add(row[5]);
  }
  const allMeanings = [...new Set(valid.map(row => row[5]))];
  const categoryMeanings = new Map();
  for (const row of valid) {
    if (!categoryMeanings.has(row[2])) categoryMeanings.set(row[2], new Set());
    categoryMeanings.get(row[2]).add(row[5]);
  }
  const vocabulary = valid.map((row, index) => {
    const id = `vocab-${row[0]}`;
    const excluded = meanings.get(row[3]);
    const distractors = [];
    const choices = [...categoryMeanings.get(row[2]), ...allMeanings];
    for (let offset = 1; offset <= choices.length && distractors.length < 3; offset++) {
      const candidate = choices[(index + offset) % choices.length];
      // Avoid exact duplicates, the same headword's meanings, and obvious partial overlaps.
      if (!excluded.has(candidate) && !distractors.includes(candidate) &&
          !candidate.includes(row[5]) && !row[5].includes(candidate)) distractors.push(candidate);
    }
    const questions = [];
    if (distractors.length === 3) {
      const options = distractors.slice();
      const answer = index % 4;
      options.splice(answer, 0, row[5]);
      questions.push({
        id: `${id}-meaning`, type: 'choice', axis: 'recognition',
        prompt: `「${row[3]}」の意味として、この教材に記載されているものは？`,
        options, answer,
        feedback: `${row[3]}（${row[4]}）：${row[5]}`
      });
    }
    questions.push({
      id: `${id}-produce`, type: 'short', axis: 'recall', lens: 'what',
      prompt: `「${row[5]}」を表す中国語を一つ書いてください。`,
      answer: `例：${row[3]}`,
      hints: [`ピンイン：${row[4]}`, row[7] ? `例文の意味：${row[7]}` : ''].filter(Boolean),
      rubric: ['意味に合う中国語を思い出せた', '別の自然な表現も、意味と用法が合っていれば正解とする'],
      feedback: '文脈により複数の言い方が可能です。模範例は一例です。'
    });
    questions.push({
      id: `${id}-reading`, type: 'flash', axis: 'recall', lens: 'what',
      prompt: `「${row[3]}」（${row[5]}）のピンインを声調記号付きで入力してください。`,
      answer: row[4], check: () => null,
      hints: [row[6] ? `例文：${row[6]}` : ''].filter(Boolean),
      rubric: ['声調記号を含めて読みを確認した', '別の読みや教材の表記に疑問がある場合は辞書で確認した'],
      feedback: `教材の読み：${row[4]}。声調や文脈による読みの違いに注意しましょう。`
    });
    if (row[6] && row[7] && row[6].includes(row[3])) {
      const masked = row[6].replace(row[3], '＿＿');
      questions.push({
        id: `${id}-context`, type: 'cloze', axis: 'application', lens: 'application',
        prompt: `次の日本語に合うように空欄に入る中国語を書いてください。\n「${row[7]}」\n${masked}`,
        answer: row[3],
        hints: [`意味の手がかり：${row[5]}`, `ピンイン：${row[4]}`],
        feedback: `${row[6]}（${row[3]}：${row[4]}）`
      });
    }
    return {id, topic: `stage-${row[1]}`, title: `語彙 #${row[0]}`, mapTitle: `${row[3]}（${row[4]}）`, kind: 'memory',
      importance: Number(row[1]) <= 1 ? 3 : Number(row[1]) <= 2 ? 2 : 1,
      prerequisites: [], group: `stage-${row[1]}`, questions,
      example: `${row[6]} / ${row[7]}`};
  });
  const skills = [
    {id: 'grammar-negation', title: '不と没の使い分け', confusables: ['grammar-le'], example: '我不喝咖啡。 / 我昨天没喝咖啡。', questions: [
      {id: 'negation-compare', type: 'explain', axis: 'discrimination', lens: 'comparison', prompt: '「我不喝咖啡」と「我昨天没喝咖啡」は、いつのどんな否定を表しますか。', answer: '前者は習慣・意志などの否定、後者は昨日飲まなかったという過去の事実の否定。', rubric: ['不は習慣や意志などの否定と説明した', '没は過去の出来事が起きなかったことを説明した']},
      {id: 'negation-recall', type: 'example', axis: 'recall', lens: 'application', prompt: '「昨日はコーヒーを飲まなかった」を中国語で表してください。', answer: '例：我昨天没喝咖啡。', rubric: ['昨日の出来事の否定を表した', '没を適切に使った']},
      {id: 'negation-cloze', type: 'cloze', axis: 'application', lens: 'when', prompt: '「昨日は映画を見なかった」を表すには「我昨天＿＿看电影。」の空欄に何を入れますか。漢字一字で答えてください。', answer: '没', hints: ['習慣の話ではなく、過去の出来事です。', '実際に起こらなかった行為を表す否定を選びます。'], feedback: '過去の行為が起きなかったことには没（没看）を使います。'},
      {id: 'negation-counterexample', type: 'reason', axis: 'understanding', lens: 'counterexample', prompt: '過去の出来事でも「没」ではなく「不」を使うのはどのような場合ですか。例文または状況を一つ挙げて説明してください。', answer: '例：当時「行きたくなかった・飲むつもりがなかった」という主観的な意志や、「是・知道」などの状態を否定するときは不を使う。', hints: ['単なる事実の不発生と、本人の意志や状態の違いを考えます。'], rubric: ['過去の意志や状態の否定では不を使うことに触れた', '単なる事実の不発生（没）と区別した']},
      {id: 'negation-transfer', type: 'example', axis: 'transfer', lens: 'comparison', prompt: '今日は行かないつもりだという文と、昨日行かなかったという文を中国語で作り、違いを説明してください。', answer: '例：我今天不去。 / 我昨天没去。', rubric: ['不を使い、今日の意志を示した', '没を使い、昨日の事実を示した']}
    ]},
    {id: 'grammar-le', title: '了と時間の文脈', confusables: ['grammar-negation', 'grammar-ba'], example: '我吃了饭。 / 下雨了。', questions: [
      {id: 'le-compare', type: 'explain', axis: 'understanding', lens: 'why', prompt: '「我吃了饭」と「下雨了」の了は、それぞれどんな意味を表しますか。了だけで過去時制と決まりますか。', answer: '前者は動作の実現、後者は雨が降り始めたという変化を示す。了だけで過去とは決まらない。', rubric: ['動作の実現と状況の変化を区別した', '了だけで過去時制とは決まらないと説明した']},
      {id: 'le-recall', type: 'explain', axis: 'recall', lens: 'what', prompt: '中国語で動作の実現や状況の変化を表す助詞を一つ挙げ、その二つの用法を説明してください。', answer: '了。動作の実現や新しい状況への変化を表す。', rubric: ['了を挙げた', '動作の実現と状況の変化の二つを説明した']},
      {id: 'le-context', type: 'free', axis: 'application', lens: 'application', prompt: '友人に「もう食事を済ませた」と伝える中国語の一文を作ってください。', answer: '例：我已经吃饭了。', rubric: ['食事が既に済んだことを自然に表した', '了などを文脈に合う位置で使った']},
      {id: 'le-counterexample', type: 'reason', axis: 'discrimination', lens: 'counterexample', prompt: '「昨日は行かなかった」を「我昨天没去了」とせず「了」を外すのはなぜですか。', answer: '没は動作が実現しなかったことを表すため、動作の実現を表す動詞直後の了とは通常同時に使わない。', hints: ['了が表す意味と、没が表す意味が両立するか考えます。'], rubric: ['没が動作の不実現を表すことに触れた', '実現を表す了と意味が矛盾することを説明した']}
    ]},
    {id: 'grammar-ba', title: '把構文と結果', confusables: ['grammar-le', 'conversation-request'], example: '请把门关上。', questions: [
      {id: 'ba-conditions', type: 'reason', axis: 'discrimination', lens: 'when', prompt: '「请把门关上」と「请关门」を比べ、把構文が注目させる対象と結果を説明してください。', answer: '把構文は特定のドアをどう処置し、どんな結果にするかに焦点を当てる。', rubric: ['特定の対象を取り上げると説明した', '対象への処置や結果に触れた']},
      {id: 'ba-recall', type: 'example', axis: 'recall', lens: 'application', prompt: '「ドアを閉めてください」を、処置する対象と結果に焦点を当てた中国語で表してください。', answer: '例：请把门关上。', rubric: ['把を使い対象を示した', '閉める結果と依頼を表した']},
      {id: 'ba-compose', type: 'example', axis: 'transfer', lens: 'application', prompt: '友人に本を机の上に置いてもらうよう、把を使って丁寧に頼んでください。', answer: '例：请把书放在桌子上。', rubric: ['本を対象として把の後に置いた', '机の上に置く結果と依頼を表した']}
    ]},
    {id: 'conversation-request', title: '場面に合う依頼', confusables: ['grammar-ba'], example: '请问，地铁站怎么走？', questions: [
      {id: 'request-compose', type: 'example', axis: 'application', lens: 'application', prompt: '初対面の人に、駅への行き方を丁寧に尋ねる中国語を作ってください。', answer: '例：请问，车站怎么走？', rubric: ['駅への道を尋ねている', '请问などを用いて丁寧に話しかけている']},
      {id: 'request-recall', type: 'example', axis: 'recall', lens: 'what', prompt: '「すみません、駅はどちらですか」を初対面の人に尋ねてください。', answer: '例：请问，车站在哪里？', rubric: ['丁寧に話しかけた', '駅の場所を尋ねた']},
      {id: 'request-compare', type: 'explain', axis: 'discrimination', lens: 'comparison', prompt: '初対面の人への「请问，车站在哪里？」と「车站在哪里？」を比べて、場面に合う丁寧さの違いを説明してください。', answer: '请问を添えると、初対面の相手に丁寧に尋ねる言い方になる。', rubric: ['请问が丁寧な呼びかけであると説明した', '相手や場面による使い分けに触れた']},
      {id: 'request-transfer', type: 'example', axis: 'transfer', lens: 'application', prompt: '店員に商品の値段を尋ね、その後お礼を言う短い会話を中国語で作ってください。', answer: '例：请问，这个多少钱？谢谢！', rubric: ['値段を丁寧に尋ねた', '返答の後にお礼を述べた']}
    ]}
  ];
  const prerequisites = {
    'grammar-negation': ['vocab-4', 'vocab-26'], 'grammar-le': ['vocab-27'],
    'grammar-ba': ['vocab-47'], 'conversation-request': ['vocab-39', 'vocab-72']
  };
  for (const item of skills) Object.assign(item, {topic: 'grammar', kind: 'skill', importance: 3, prerequisites: prerequisites[item.id], group: 'grammar'});
  const course = {
    id: 'chinese', title: '中国語',
    description: '語彙は認識・自力再現・文脈穴埋めを組み合わせて定着させ、文法や会話は類似表現の識別・反例・作文を自己評価で鍛えます。',
    topics: [...new Set(valid.map(row => row[1]))].map(stage => ({id: `stage-${stage}`, title: `語彙・段階 ${stage}`})).concat({id: 'grammar', title: '文法と会話'}),
    items: [...vocabulary, ...skills]
  };
  window.chineseAdaptive = window.AdaptiveStudy.mount(root, course);
})();
