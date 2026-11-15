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
  search.addEventListener('input', filter);
  stage.addEventListener('change', filter);
  use.addEventListener('change', filter);
  previous.addEventListener('click', () => { if (page > 0) { page--; render(); } });
  next.addEventListener('click', () => { if ((page + 1) * pageSize < filtered.length) { page++; render(); } });
  filter();
})();
