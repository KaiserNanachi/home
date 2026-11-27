'use strict';
(() => {
  const rows = window.CHINESE_VOCABULARY;
  if (!Array.isArray(rows)) return;
  const byId = id => document.getElementById(id);
  const view = byId('practice-view');
  const content = byId('practice-content');
  const kind = byId('practice-kind');
  const count = byId('practice-count');
  const voice = byId('practice-voice');
  const audioStatus = byId('practice-audio-status');
  const start = byId('practice-start');
  const end = byId('practice-end');
  const buttons = [byId('show-dictation'), byId('show-shadowing')];
  const synth = window.speechSynthesis;
  let voices = [];
  let mode = 'dictation';
  let session = null;
  const normalize = text => text.normalize('NFKC').toLowerCase().replace(/[\p{P}\p{S}\s]/gu, '');
  const searchNormalize = text => text.normalize('NFD').replace(/[\u0300-\u036f]/g, '').toLowerCase().replace(/\s+/g, '');
  function candidates() {
    const query = searchNormalize(byId('search').value.trim());
    const seen = new Set();
    const index = kind.value === 'word' ? 3 : 6;
    return rows.filter(row => {
      const text = row[index].trim();
      if (!text || seen.has(text) || (query && !searchNormalize(row.slice(3, 8).join(' ')).includes(query)) || (byId('stage').value && row[1] !== byId('stage').value) || (byId('use').value && row[2] !== byId('use').value)) return false;
      seen.add(text);
      return true;
    });
  }
  function updateAvailability() {
    const available = candidates().length;
    byId('practice-availability').textContent = `現在の条件で ${available.toLocaleString()} 問練習できます。設定変更は再開始時に反映されます。`;
    start.disabled = !available;
  }
  function updateVoices() {
    if (!synth || !window.SpeechSynthesisUtterance) {
      voice.disabled = true;
      audioStatus.textContent = 'このブラウザでは音声再生を利用できません。';
      return;
    }
    const selected = voice.value;
    const language = item => item.lang.toLowerCase().replaceAll('_', '-');
    const priority = item => {
      const lang = language(item);
      if (/^(zh-(cn|hans|sg)|cmn)(-|$)/.test(lang)) return 0;
      if (/^zh-(tw|hant)(-|$)/.test(lang)) return 1;
      return 2;
    };
    voices = synth.getVoices()
      .filter(item => /^(zh|cmn)(-|$)/.test(language(item)))
      .sort((a, b) => priority(a) - priority(b));
    voice.replaceChildren();
    voices.forEach(item => voice.add(new Option(`${item.name} (${item.lang})`, item.voiceURI)));
    if (voices.some(item => item.voiceURI === selected)) voice.value = selected;
    voice.disabled = voices.length === 0;
    audioStatus.textContent = voices.length ? '' : '中国語音声が見つかりません。端末の音声設定を確認してください。';
  }
  function stop() { if (synth) synth.cancel(); }
  function play(text) {
    stop();
    if (!voices.length) updateVoices();
    if (!voices.length) return;
    const utterance = new SpeechSynthesisUtterance(text);
    utterance.voice = voices.find(item => item.voiceURI === voice.value) || voices[0];
    utterance.lang = utterance.voice.lang;
    utterance.rate = Number(byId('practice-speed').value);
    utterance.onerror = event => { if (event.error !== 'canceled' && event.error !== 'interrupted') audioStatus.textContent = '音声を再生できませんでした。別の中国語音声を選んでください。'; };
    audioStatus.textContent = '';
    synth.speak(utterance);
  }
  function make(tag, text, className) {
    const el = document.createElement(tag);
    el.textContent = text;
    if (className) el.className = className;
    return el;
  }
  function button(label, action, className) {
    const el = make('button', label, className);
    el.type = 'button';
    el.addEventListener('click', action);
    return el;
  }
  function finish() {
    stop();
    const panel = make('section', '', 'quiz-panel');
    const heading = make('h2', '練習終了');
    heading.tabIndex = -1;
    panel.append(heading);
    if (mode === 'dictation') {
      panel.append(make('p', `${session.questions.length} 問中 ${session.correct} 問正解`));
      if (session.wrong.length) panel.append(button('間違えた問題をもう一度', () => begin(session.wrong, session.kind, session.wrong.length)));
    }
    content.replaceChildren(panel);
    end.hidden = true;
    heading.focus();
  }
  function render() {
    stop();
    const row = session.questions[session.index];
    const text = row[session.kind === 'word' ? 3 : 6];
    const translation = row[session.kind === 'word' ? 5 : 7];
    const panel = make('section', '', 'quiz-panel');
    const heading = make('h2', `第 ${session.index + 1} / ${session.questions.length} 問`);
    heading.tabIndex = -1;
    panel.append(heading);
    const actions = make('div', '', 'quiz-actions');
    actions.append(button('音声を再生', () => play(text), 'primary'), button('停止', stop));
    panel.append(actions);
    const next = () => { session.index++; if (session.index === session.questions.length) finish(); else render(); };
    if (mode === 'dictation') {
      const form = document.createElement('form');
      const label = make('label', '聞こえた中国語を入力');
      label.htmlFor = 'practice-answer';
      const input = document.createElement('input');
      input.id = 'practice-answer'; input.className = 'practice-input'; input.lang = 'zh-Hans'; input.autocomplete = 'off';
      const feedback = make('div', '', 'quiz-feedback'); feedback.hidden = true;
      const controls = make('div', '', 'quiz-actions');
      const submit = make('button', '答え合わせ', 'primary'); submit.type = 'submit';
      const reveal = button('答えを見る', () => check(true));
      controls.append(submit, reveal);
      function check(revealed) {
        if (!revealed && !input.value.trim()) { input.focus(); return; }
        const correct = !revealed && normalize(input.value) === normalize(text);
        if (correct) session.correct++; else session.wrong.push(row);
        input.disabled = submit.disabled = reveal.disabled = true;
        feedback.append(make('strong', correct ? '正解' : revealed ? '答え' : 'もう一度確認しましょう'));
        const answer = make('p', text); answer.lang = 'zh-Hans';
        const advance = button(session.index + 1 === session.questions.length ? '結果を見る' : '次の問題', next, 'primary');
        feedback.append(answer, make('p', translation), advance);
        feedback.hidden = false;
        advance.focus();
      }
      input.addEventListener('keydown', event => { if (event.key === 'Enter' && (event.isComposing || event.keyCode === 229)) event.preventDefault(); });
      form.addEventListener('submit', event => { event.preventDefault(); if (!event.isComposing && !input.disabled) check(false); });
      form.append(label, input, controls, feedback); panel.append(form);
    } else {
      panel.append(make('p', '音声を聞きながら声に出して繰り返しましょう。', 'quiz-note'));
      const details = document.createElement('details');
      details.open = true;
      details.append(make('summary', '中国語と日本語を表示・非表示'));
      const answer = make('p', text, 'quiz-question'); answer.lang = 'zh-Hans';
      details.append(answer, make('p', translation));
      panel.append(details, button(session.index + 1 === session.questions.length ? '練習を終える' : '次の問題', next));
    }
    content.replaceChildren(panel);
    heading.focus();
  }
  function begin(source, questionKind, limit) {
    if (!source.length) return;
    const shuffled = source.slice();
    for (let i = shuffled.length - 1; i > 0; i--) { const j = Math.floor(Math.random() * (i + 1)); [shuffled[i], shuffled[j]] = [shuffled[j], shuffled[i]]; }
    session = {questions: shuffled.slice(0, limit), index: 0, correct: 0, wrong: [], kind: questionKind};
    end.hidden = false;
    render();
  }
  buttons.forEach((buttonEl, index) => buttonEl.addEventListener('click', () => {
    if (buttonEl.getAttribute('aria-pressed') === 'true') return;
    stop(); session = null; content.replaceChildren(); end.hidden = true;
    window.dispatchEvent(new Event('chinese-practice-open'));
    mode = index ? 'shadowing' : 'dictation';
    byId('list-view').hidden = true; byId('quiz-view').hidden = true; view.hidden = false;
    byId('show-list').setAttribute('aria-pressed', 'false'); byId('show-quiz').setAttribute('aria-pressed', 'false');
    buttons.forEach((item, i) => item.setAttribute('aria-pressed', String(i === index)));
    byId('practice-title').textContent = index ? 'シャドーイング' : 'ディクテーション';
    updateAvailability(); updateVoices();
  }));
  window.addEventListener('chinese-practice-close', () => { stop(); view.hidden = true; buttons.forEach(item => item.setAttribute('aria-pressed', 'false')); });
  window.addEventListener('pagehide', stop);
  start.addEventListener('click', () => {
    const source = candidates();
    const limit = count.value === 'all' ? source.length : Number(count.value);
    begin(source, kind.value, limit);
  });
  end.addEventListener('click', () => { stop(); session = null; content.replaceChildren(); end.hidden = true; });
  [kind, count, byId('search'), byId('stage'), byId('use')].forEach(item => item.addEventListener(item.id === 'search' ? 'input' : 'change', updateAvailability));
  if (synth) synth.addEventListener('voiceschanged', updateVoices);
  updateVoices();
})();
