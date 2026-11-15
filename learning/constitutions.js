(() => {
  'use strict';
  const data = window.CONSTITUTIONS_DATA;
  const filters = document.getElementById('country-filters');
  const allButton = document.getElementById('all-countries');
  const countryMessage = document.getElementById('country-message');
  const search = document.getElementById('search');
  const theme = document.getElementById('theme');
  const reset = document.getElementById('reset');
  const status = document.getElementById('results-status');
  const results = document.getElementById('results');
  const verified = document.getElementById('verified-date');
  let selected = [];

  function node(tag, className, text) {
    const element = document.createElement(tag);
    if (className) element.className = className;
    if (text !== undefined && text !== null) element.textContent = String(text);
    return element;
  }
  function normalize(value) {
    return String(value || '').normalize('NFD').replace(/[\u0300-\u036f]/g, '').toLocaleLowerCase().replace(/\s+/g, '');
  }
  function addLink(container, label, url) {
    try {
      const safe = new URL(url);
      if (safe.protocol !== 'https:' && safe.protocol !== 'http:') return;
      const a = node('a', '', label + ' ↗');
      a.href = safe.href;
      a.target = '_blank';
      a.rel = 'noopener noreferrer';
      container.appendChild(a);
    } catch (_) { /* Omit malformed source URLs. */ }
  }
  if (!data || !Array.isArray(data.countries)) {
    status.textContent = '条文データを読み込めませんでした。時間をおいて再度お試しください。';
    reset.disabled = true;
    search.disabled = true;
    theme.disabled = true;
    allButton.disabled = true;
    return;
  }
  if (data.verifiedOn) verified.textContent = `出典確認日：${data.verifiedOn}`;
  const items = [];
  const themeSet = new Set();
  data.countries.forEach((country, countryIndex) => {
    const button = node('button', 'country-filter', country.name);
    button.type = 'button';
    button.dataset.country = country.id;
    button.setAttribute('aria-pressed', 'false');
    button.addEventListener('click', () => {
      const id = country.id;
      if (selected.includes(id)) selected = selected.filter(value => value !== id);
      else if (selected.length < 2) selected.push(id);
      else {
        countryMessage.textContent = '国は2か国まで選べます。別の国を選ぶには、選択中の国を解除してください。';
        return;
      }
      countryMessage.textContent = '';
      render();
    });
    filters.appendChild(button);
    (country.excerpts || []).forEach((excerpt, excerptIndex) => {
      (excerpt.themes || []).forEach(value => themeSet.add(value));
      const fields = [country.name, country.englishName, country.code, country.documentTitle, excerpt.article, excerpt.title, excerpt.original, excerpt.translation, excerpt.ipa, excerpt.pinyin, excerpt.reading].concat(excerpt.themes || []);
      items.push({country, excerpt, countryIndex, excerptIndex, haystack: normalize(fields.join(' '))});
    });
  });
  document.getElementById('collection-stats').textContent = `${data.countries.length} COUNTRIES / ${items.length} EXCERPTS`;
  [...themeSet].sort((a, b) => a.localeCompare(b, 'ja')).forEach(value => {
    const option = node('option', '', value);
    option.value = value;
    theme.appendChild(option);
  });
  function addText(parent, tag, className, text) {
    const element = node(tag, className, text);
    parent.appendChild(element);
    return element;
  }
  function languageBlock(parent, label, text, language, className) {
    const block = node('div', 'language-block');
    addText(block, 'p', 'small-label', label);
    const paragraph = addText(block, 'p', 'excerpt-text ' + className, text);
    if (language) paragraph.lang = language;
    parent.appendChild(block);
  }
  function pronunciationBlock(parent, label, text, language) {
    if (!text) return;
    const block = node('div', 'pronunciation');
    addText(block, 'p', 'small-label', label);
    const paragraph = addText(block, 'p', 'excerpt-text', text);
    if (language) paragraph.lang = language;
    parent.appendChild(block);
  }
  function cardFor(item) {
    const {country, excerpt, countryIndex, excerptIndex} = item;
    const card = node('article', 'excerpt-card');
    const header = node('header', 'excerpt-header');
    const meta = node('div', 'country-meta');
    addText(meta, 'p', 'country-code', `${String(countryIndex + 1).padStart(2, '0')} / ${country.code || ''} · ${country.englishName || ''}`);
    addText(meta, 'h3', 'country-name', country.name);
    addText(meta, 'p', 'document-title', country.documentTitle);
    header.append(meta, node('span', 'excerpt-number', excerpt.article || `EXCERPT ${excerptIndex + 1}`));
    const body = node('div', 'excerpt-body');
    addText(body, 'h4', 'article-heading', excerpt.title || excerpt.article);
    const themes = node('div', 'theme-list');
    (excerpt.themes || []).forEach(value => themes.appendChild(node('span', 'theme-tag', value)));
    body.appendChild(themes);
    const language = node('div', 'language-grid');
    const isJapanese = country.lang === 'ja' || country.id === 'japan';
    languageBlock(language, `ORIGINAL / 原文${country.language ? `（${country.language}）` : ''}`, excerpt.original, country.lang, 'original');
    languageBlock(language, isJapanese ? 'JAPANESE / 日本語（原文と同一）' : 'TRANSLATION / 日本語訳（学習用）', excerpt.translation, 'ja', 'translation');
    body.appendChild(language);
    pronunciationBlock(body, 'IPA / 発音の目安（広い転写）', excerpt.ipa, 'und-fonipa');
    pronunciationBlock(body, 'PINYIN / ピンイン', excerpt.pinyin, 'zh-Latn-pinyin');
    pronunciationBlock(body, 'READING / 読み', excerpt.reading, 'ja');
    const notes = node('div', 'excerpt-notes');
    if (excerpt.scopeNote) addText(notes, 'p', '', `抜粋範囲：${excerpt.scopeNote}`);
    if (country.contextNote) addText(notes, 'p', '', `文脈：${country.contextNote}`);
    if (country.pronunciationNote) addText(notes, 'p', '', `発音について：${country.pronunciationNote}`);
    const links = node('div', 'source-links');
    if (excerpt.sourceUrl) addLink(links, 'この抜粋の出典', excerpt.sourceUrl);
    (country.sources || []).forEach(source => addLink(links, source.label || '憲法の原文', source.url));
    if (links.childNodes.length) notes.appendChild(links);
    body.appendChild(notes);
    card.append(header, body);
    return card;
  }
  function render() {
    allButton.setAttribute('aria-pressed', String(selected.length === 0));
    filters.querySelectorAll('[data-country]').forEach(button => button.setAttribute('aria-pressed', String(selected.includes(button.dataset.country))));
    const query = normalize(search.value.trim());
    const found = items.filter(({country, excerpt, haystack}) => (!selected.length || selected.includes(country.id)) && (!theme.value || (excerpt.themes || []).includes(theme.value)) && (!query || haystack.includes(query)));
    status.textContent = `${found.length}件の抜粋を表示 / 全${items.length}件`;
    results.replaceChildren();
    if (!found.length) {
      results.appendChild(node('p', 'empty-state', '条件に合う抜粋はありません。検索語や絞り込みを変えてください。'));
      return;
    }
    const fragment = document.createDocumentFragment();
    found.forEach(item => fragment.appendChild(cardFor(item)));
    results.appendChild(fragment);
  }
  allButton.addEventListener('click', () => { selected = []; countryMessage.textContent = ''; render(); });
  search.addEventListener('input', render);
  theme.addEventListener('change', render);
  reset.addEventListener('click', () => {
    selected = [];
    search.value = '';
    theme.value = '';
    countryMessage.textContent = '';
    render();
  });
  render();
})();
