'use strict';
const assert = require('node:assert/strict');
const {readFileSync} = require('node:fs');
const path = require('node:path');
const test = require('node:test');
const {JSDOM, ResourceLoader, VirtualConsole} = require('jsdom');

const root = path.resolve(__dirname, '..');
const origin = 'https://study.test';
const published = process.env.STUDY_SOURCE === '1' ? root : path.join(root, 'docs');

class LocalFiles extends ResourceLoader {
  fetch(url) {
    const address = new URL(url);
    if (address.origin !== origin) return null;
    return Promise.resolve(readFileSync(path.join(published, address.pathname)));
  }
}

async function open(page, options = {}) {
  const errors = [];
  const virtualConsole = new VirtualConsole();
  virtualConsole.on('jsdomError', error => errors.push(error.message));
  virtualConsole.on('error', error => errors.push(String(error)));
  const pathname = '/learning/' + page + '.html';
  const dom = new JSDOM(readFileSync(path.join(published, pathname), 'utf8'), {
    url: origin + pathname + (options.hash || ''),
    runScripts: 'dangerously',
    resources: new LocalFiles(),
    pretendToBeVisual: true,
    virtualConsole,
    beforeParse(window) {
      window.scrollTo = () => {};
      window.HTMLElement.prototype.scrollIntoView = () => {};
      if (options.seed) {
        Object.entries(options.seed).forEach(([key, value]) => window.localStorage.setItem(key, value));
      }
    }
  });
  await new Promise(resolve => dom.window.addEventListener('load', resolve, {once: true}));
  return {dom, errors, window: dom.window, document: dom.window.document};
}
const byText = (root, label) => {
  const button = [...root.querySelectorAll('button')].find(entry => entry.textContent === label);
  assert.ok(button, 'expected button ' + label);
  return button;
};
const change = (window, control, value) => {
  assert.ok(control);
  control.value = value;
  control.dispatchEvent(new window.Event('input', {bubbles: true}));
};
const submit = (window, form) => form.dispatchEvent(new window.Event('submit', {bubbles: true, cancelable: true}));
const copyStorage = window => Object.fromEntries(Array.from({length: window.localStorage.length}, (_, index) => {
  const key = window.localStorage.key(index);
  return [key, window.localStorage.getItem(key)];
}));
const findings = (window, subject) => Object.entries(copyStorage(window))
  .filter(([key]) => key.startsWith('adaptive-study:v1:' + subject + ':attempt:'))
  .map(([, value]) => JSON.parse(value));
const advanceHash = window => new Promise(resolve => {
  window.addEventListener('hashchange', () => resolve(), {capture: true, once: true});
});

test('Chinese opens adaptively, resumes a typed answer, saves self-grading, retains all legacy modes', async () => {
  const first = await open('chinese');
  let snapshot;
  try {
    const study = first.document.querySelector('#adaptive-study');
    assert.match(study.querySelector('h1').textContent, /中国語/);
    assert.equal(first.document.querySelector('#legacy-controls').hidden, true);
    byText(study, '15分の学習を始める').click();
    assert.equal(study.querySelector('.as-feedback'), null);
    submit(first.window, study.querySelector('form'));
    assert.match(study.textContent, /回答を入力/);
    assert.equal(findings(first.window, 'chinese').length, 0);
    const input = study.querySelector('form input:not([type=radio]),form textarea');
    change(first.window, input, '测试答案');
    change(first.window, study.querySelector('form select'), '3');
    snapshot = copyStorage(first.window);
    assert.deepEqual(first.errors, []);
  } finally {
    first.dom.window.close();
  }
  const second = await open('chinese', {seed: snapshot});
  try {
    const study = second.document.querySelector('#adaptive-study');
    assert.equal(study.querySelector('form input:not([type=radio]),form textarea').value, '测试答案');
    submit(second.window, study.querySelector('form'));
    assert.ok(study.querySelector('.as-feedback'));
    assert.equal(findings(second.window, 'chinese').length, 0);
    byText(study, '要復習').click();
    byText(study, '次へ進む').click();
    byText(study, '思い出せない').click();
    const events = findings(second.window, 'chinese');
    assert.equal(events.length, 2);
    assert.equal(events[0].method, 'self');
    assert.equal(events[0].confidence, 3);
    assert.equal(events[1].method, 'unable');
    byText(study, 'セッションを終了').click();
    byText(study, '学習ホームへ').click();
    assert.match(study.textContent, /再学習が必要/);
    byText(study, '学習分析').click();
    assert.match(study.textContent, /初見への応用/);
    byText(study, '学習ホームへ').click();
    byText(study, '知識マップ').click();
    change(second.window, study.querySelector('input[type=search]'), '地铁');
    assert.match(study.querySelector('.as-map').textContent, /dìtiě/);
    for (const [mode, panel] of [
      ['show-list', 'list-view'], ['show-quiz', 'quiz-view'],
      ['show-dictation', 'practice-view'], ['show-shadowing', 'practice-view']
    ]) {
      second.document.getElementById(mode).click();
      assert.equal(second.document.getElementById(panel).hidden, false);
      assert.equal(study.hidden, true);
    }
    second.document.getElementById('show-adaptive').click();
    assert.equal(study.hidden, false);
    assert.deepEqual(second.errors, []);
  } finally {
    second.dom.window.close();
  }
});

test('statistics imports dated legacy history once and keeps original notebook routes', async () => {
  const legacyKey = 'statistics-pre1-statlab-v1';
  const latest = new Date(Date.now() - 3 * 86400000).toISOString();
  const seed = {[legacyKey]: JSON.stringify({scope: '2026', answers: {
    q1: {value: 0, correct: false, firstCorrect: false, tries: 3, at: latest}
  }, learned: {prob: true}, last: 'prob', exam: null})};
  const site = await open('statistics-pre1-interactive', {seed});
  try {
    const {window, document} = site;
    const study = document.querySelector('.adaptive-study');
    assert.match(study.querySelector('h1').textContent, /統計学/);
    assert.equal(findings(window, 'statistics-pre1').length, 1);
    assert.equal(window.StatLab.getState().learned.prob, true);
    byText(study, '学習設定').click();
    assert.equal(study.querySelector('option[value=causal]'), null);
    const scope = document.getElementById('scope');
    scope.value = '2027';
    scope.dispatchEvent(new window.Event('change', {bubbles: true}));
    assert.ok(study.querySelector('option[value=causal]'));
    byText(study, '学習ホームへ').click();
    byText(study, '15分の学習を始める').click();
    assert.equal(study.querySelector('.as-feedback'), null);
    byText(study, '思い出せない').click();
    assert.ok(study.querySelector('.as-feedback'));
    assert.equal(findings(window, 'statistics-pre1').length, 2);
    const switched = advanceHash(window);
    window.location.hash = '#prob/lesson';
    await switched;
    assert.equal(document.contains(study), false);
    assert.match(document.getElementById('content').textContent, /条件付き確率/);
    const quiz = advanceHash(window);
    window.location.hash = '#prob/quiz';
    await quiz;
    assert.match(document.getElementById('content').textContent, /条件付き確率/);
    const returned = advanceHash(window);
    window.location.hash = '#adaptive';
    await returned;
    assert.equal(document.contains(study), true);
    assert.ok(study.querySelector('.as-feedback'));
    assert.deepEqual(site.errors, []);
  } finally {
    site.dom.window.close();
  }
});
