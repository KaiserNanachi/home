'use strict';
(() => {
  const rows = window.CHINESE_VOCABULARY;
  const search = document.getElementById('search');
  const stage = document.getElementById('stage');
  const use = document.getElementById('use');
  const results = document.getElementById('results');
  const status = document.getElementById('status');
  const pageLabel = document.getElementById('page');
  const previous = document.getElementById('previous');
  const next = document.getElementById('next');
  const pageSize = 50;
  let page = 0;
  let filtered = [];
  const normalise = value => value.normalize('NFD').replace(/[\u0300-\u036f]/g, '').toLocaleLowerCase().replace(/\s+/g, '');
  function addOptions(select, values, label) {
    [...new Set(values)].filter(Boolean).forEach(value => {
      const option = document.createElement('option');
      option.value = value;
      option.textContent = label(value);
      select.appendChild(option);
    });
  }
  if (!Array.isArray(rows)) {
    status.textContent = '単語データを読み込めませんでした。CSVをダウンロードしてご覧ください。';
    previous.disabled = next.disabled = true;
    return;
  }
  addOptions(stage, rows.map(row => row[1]), value => `段階 ${value}`);
  addOptions(use, rows.map(row => row[2]), value => value);
  const indexed = rows.map(row => ({row, text: normalise(row.slice(3, 8).join(' '))}));
  function element(tag, className, text) {
    const el = document.createElement(tag);
    if (className) el.className = className;
    el.textContent = text;
    return el;
  }
  function render() {
    const count = filtered.length;
    const pages = Math.max(1, Math.ceil(count / pageSize));
    page = Math.min(page, pages - 1);
    const start = page * pageSize;
    status.textContent = count ? `${count.toLocaleString()}語中 ${start + 1}〜${Math.min(start + pageSize, count)}語を表示` : '該当する単語はありません。検索条件を変えてください。';
    pageLabel.textContent = `${page + 1} / ${pages}`;
    previous.disabled = page === 0;
    next.disabled = page >= pages - 1;
    results.replaceChildren();
    if (!count) {
      results.appendChild(element('p', 'empty', '条件に一致する単語が見つかりませんでした。'));
      return;
    }
    const fragment = document.createDocumentFragment();
    filtered.slice(start, start + pageSize).forEach(({row}) => {
      const card = document.createElement('article');
      card.className = 'card';
      const head = document.createElement('div');
      head.className = 'card-head';
      const word = element('h2', 'word', row[3]);
      word.lang = 'zh-Hans';
      head.append(word, element('span', 'rank', `#${row[0]}`));
      const example = document.createElement('div');
      example.className = 'example';
      const chineseExample = element('p', '', row[6]);
      chineseExample.lang = 'zh-Hans';
      example.append(chineseExample, element('p', 'ja', row[7]));
      card.append(head, element('p', 'pinyin', row[4]), element('p', 'meaning', row[5]), element('p', 'meta', `段階 ${row[1]} · ${row[2]}`), example, element('p', 'note', `出典区分：${row[8]}　${row[9]}`));
      fragment.appendChild(card);
    });
    results.appendChild(fragment);
  }
  function filter() {
    const query = normalise(search.value.trim());
    filtered = indexed.filter(item => (!query || item.text.includes(query)) && (!stage.value || item.row[1] === stage.value) && (!use.value || item.row[2] === use.value));
    page = 0;
    render();
  }
  const listView = document.getElementById('list-view');
  const quizView = document.getElementById('quiz-view');
  const showList = document.getElementById('show-list');
  const showQuiz = document.getElementById('show-quiz');
  const direction = document.getElementById('quiz-direction');
  const countSelect = document.getElementById('quiz-count');
  const availability = document.getElementById('quiz-availability');
  const settingsNote = document.getElementById('quiz-settings-note');
  const startButton = document.getElementById('quiz-start');
  const endButton = document.getElementById('quiz-end');
  const quizContent = document.getElementById('quiz-content');
  let quiz = null;
  const formats = {
    'zh-ja': {promptIndex: 3, answerIndex: 5, title: 'この中国語の意味は？', promptLang: 'zh-Hans', answerLang: 'ja'},
    'ja-zh': {promptIndex: 5, answerIndex: 3, title: 'この日本語に合う中国語は？', promptLang: 'ja', answerLang: 'zh-Hans'},
    'zh-pinyin': {promptIndex: 3, answerIndex: 4, title: 'この中国語のピンインは？', promptLang: 'zh-Hans', answerLang: 'zh-Latn-pinyin'},
    'pinyin-zh': {promptIndex: 4, answerIndex: 3, title: 'このピンインに合う中国語は？', promptLang: 'zh-Latn-pinyin', answerLang: 'zh-Hans'},
    'example-zh-ja': {promptIndex: 6, answerIndex: 7, title: 'この中国語例文の意味は？', promptLang: 'zh-Hans', answerLang: 'ja'},
    'example-ja-zh': {promptIndex: 7, answerIndex: 6, title: 'この日本語例文に合う中国語は？', promptLang: 'ja', answerLang: 'zh-Hans'}
  };
  const answerSets = Object.fromEntries(Object.keys(formats).map(dir => [dir, new Map()]));
  for (const row of rows) {
    for (const [dir, {promptIndex, answerIndex}] of Object.entries(formats)) {
      const prompt = row[promptIndex].trim();
      const answer = row[answerIndex].trim();
      if (!prompt || !answer) continue;
      if (!answerSets[dir].has(prompt)) answerSets[dir].set(prompt, new Set());
      answerSets[dir].get(prompt).add(answer);
    }
  }
  function shuffle(items) {
    const copy = items.slice();
    for (let i = copy.length - 1; i > 0; i--) {
      const j = Math.floor(Math.random() * (i + 1));
      [copy[i], copy[j]] = [copy[j], copy[i]];
    }
    return copy;
  }
  function uniqueQuestions(source, dir) {
    const {promptIndex, answerIndex} = formats[dir];
    const seen = new Set();
    return shuffle(source).filter(row => {
      const prompt = row[promptIndex].trim();
      if (!prompt || !row[answerIndex].trim() || seen.has(prompt)) return false;
      seen.add(prompt);
      return true;
    });
  }
  function updateAvailability() {
    const available = uniqueQuestions(filtered.map(item => item.row), direction.value).length;
    const requested = Number(countSelect.value);
    availability.textContent = available ? `現在の条件で出題できる問題は${available.toLocaleString()}問です。${Math.min(available, requested)}問を出題します。` : '現在の条件では出題できる単語がありません。検索条件を変更してください。';
    startButton.disabled = available === 0;
  }
  function settingsChanged() {
    updateAvailability();
    if (quiz && quiz.active) settingsNote.textContent = '設定を変更しました。進行中のテストは開始時の設定で続き、再開始すると新しい設定が適用されます。';
  }
  const adaptiveView = document.getElementById('adaptive-study');
  const adaptiveButton = document.getElementById('show-adaptive');
  const legacyControls = document.getElementById('legacy-controls');
  function setMode(test, adaptive = false) {
    window.dispatchEvent(new Event('chinese-practice-close'));
    adaptiveView.hidden = !adaptive;
    legacyControls.hidden = adaptive;
    adaptiveButton.setAttribute('aria-pressed', String(adaptive));
    listView.hidden = test || adaptive;
    quizView.hidden = !test || adaptive;
    showList.setAttribute('aria-pressed', String(!test && !adaptive));
    showQuiz.setAttribute('aria-pressed', String(test));
    if (test) updateAvailability();
  }
  function choicesFor(row, dir) {
    const {promptIndex, answerIndex} = formats[dir];
    const correct = row[answerIndex].trim();
    const accepted = answerSets[dir].get(row[promptIndex].trim());
    const alternatives = [...new Set(rows.map(item => item[answerIndex].trim()).filter(answer => answer && !accepted.has(answer)))];
    return shuffle([correct, ...shuffle(alternatives).slice(0, 3)]);
  }
  function lockModeSwitch(locked) {
    showList.disabled = locked;
    adaptiveButton.disabled = locked;
    document.getElementById('show-dictation').disabled = locked;
    document.getElementById('show-shadowing').disabled = locked;
  }
  function begin(source, dir, limit) {
    const questions = uniqueQuestions(source, dir).slice(0, limit);
    if (!questions.length) return;
    quiz = {questions, dir, index: 0, correct: 0, wrong: [], answered: false, active: true};
    lockModeSwitch(true);
    startButton.textContent = '新しい設定で再開始';
    endButton.hidden = false;
    settingsNote.textContent = '進行中は開始時の条件で出題します。設定の変更は再開始後に反映されます。';
    renderQuestion();
  }
  function finish() {
    quiz.active = false;
    lockModeSwitch(false);
    startButton.textContent = '新しいテストを開始';
    endButton.hidden = true;
    settingsNote.textContent = '';
    const panel = document.createElement('section');
    panel.className = 'quiz-panel';
    const resultTitle = element('h2', '', 'テスト結果');
    resultTitle.tabIndex = -1;
    panel.append(resultTitle);
    const total = quiz.questions.length;
    panel.append(element('p', 'quiz-score', `${total}問中 ${quiz.correct}問正解（正答率 ${Math.round(quiz.correct / total * 100)}%）`));
    if (quiz.wrong.length) {
      panel.append(element('h3', '', '間違えた単語'));
      const list = document.createElement('ul');
      list.className = 'quiz-result-list';
      quiz.wrong.forEach(row => list.append(element('li', '', `${row[3]}（${row[4]}）— ${row[5]}｜${row[6]} / ${row[7]}`)));
      panel.append(list);
      const retry = element('button', 'primary', '間違えた語だけ再テスト');
      retry.type = 'button';
      retry.id = 'quiz-retry';
      retry.addEventListener('click', () => begin(quiz.wrong.slice(), quiz.dir, quiz.wrong.length));
      panel.append(retry);
    } else panel.append(element('p', '', '全問正解です。'));
    quizContent.replaceChildren(panel);
    resultTitle.focus();
  }
  function renderQuestion() {
    const row = quiz.questions[quiz.index];
    const {promptIndex, answerIndex, title, promptLang, answerLang} = formats[quiz.dir];
    quiz.answered = false;
    const panel = document.createElement('section');
    panel.className = 'quiz-panel';
    const promptTitle = element('h2', '', title);
    promptTitle.tabIndex = -1;
    const progress = element('p', 'quiz-progress', `第 ${quiz.index + 1} / ${quiz.questions.length} 問 · 正解 ${quiz.correct} 問`);
    const prompt = element('p', 'quiz-question', row[promptIndex]);
    prompt.lang = promptLang;
    const options = document.createElement('div');
    options.className = 'quiz-options';
    options.setAttribute('aria-label', '回答の選択肢');
    const feedback = document.createElement('div');
    feedback.className = 'quiz-feedback';
    feedback.hidden = true;
    choicesFor(row, quiz.dir).forEach(choice => {
      const button = element('button', '', choice);
      button.type = 'button';
      button.lang = answerLang;
      button.addEventListener('click', () => {
        if (quiz.answered) return;
        quiz.answered = true;
        const correct = choice === row[answerIndex].trim();
        if (correct) quiz.correct++; else quiz.wrong.push(row);
        if (quiz.dir === 'zh-ja' && window.chineseAdaptive) {
          window.chineseAdaptive.recordLegacy({id: `quiz-${Date.now()}-${Math.random().toString(36).slice(2)}`, itemId: `vocab-${row[0]}`, questionId: `vocab-${row[0]}-meaning`, at: Date.now(), correct, response: choice});
        }
        options.querySelectorAll('button').forEach(item => { item.disabled = true; });
        progress.textContent = `第 ${quiz.index + 1} / ${quiz.questions.length} 問 · 正解 ${quiz.correct} 問`;
        feedback.append(element('strong', '', correct ? '正解！' : '不正解'));
        feedback.append(element('p', '', `正答：${row[answerIndex]}`));
        feedback.append(element('p', '', `単語：${row[3]}（${row[4]}）— ${row[5]}`));
        const example = element('p', '', row[6]);
        example.lang = 'zh-Hans';
        feedback.append(example, element('p', '', row[7]));
        const nextQuestion = element('button', 'primary', quiz.index + 1 === quiz.questions.length ? '結果を見る' : '次の問題へ');
        nextQuestion.type = 'button';
        nextQuestion.id = 'quiz-next';
        nextQuestion.addEventListener('click', () => {
          quiz.index++;
          if (quiz.index === quiz.questions.length) finish(); else renderQuestion();
        });
        feedback.append(nextQuestion);
        feedback.hidden = false;
        nextQuestion.focus();
      });
      options.append(button);
    });
    panel.append(promptTitle, progress, prompt, options, feedback);
    quizContent.replaceChildren(panel);
    promptTitle.focus();
  }
  adaptiveButton.addEventListener('click', () => setMode(false, true));
  window.addEventListener('chinese-practice-open', () => { adaptiveView.hidden = true; legacyControls.hidden = false; adaptiveButton.setAttribute('aria-pressed', 'false'); });
  showList.addEventListener('click', () => setMode(false));
  showQuiz.addEventListener('click', () => setMode(true));
  startButton.addEventListener('click', () => begin(filtered.map(item => item.row), direction.value, Number(countSelect.value)));
  endButton.addEventListener('click', () => {
    quiz = null;
    lockModeSwitch(false);
    startButton.textContent = 'テスト開始';
    endButton.hidden = true;
    settingsNote.textContent = 'テストを終了しました。';
    quizContent.replaceChildren();
  });
  direction.addEventListener('change', settingsChanged);
  countSelect.addEventListener('change', settingsChanged);
  for (const control of [search, stage, use]) control.addEventListener(control === search ? 'input' : 'change', () => { filter(); settingsChanged(); });
  previous.addEventListener('click', () => { if (page > 0) { page--; render(); } });
  next.addEventListener('click', () => { if ((page + 1) * pageSize < filtered.length) { page++; render(); } });
  filter();
})();
