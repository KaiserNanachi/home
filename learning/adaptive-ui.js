'use strict';
(function () {
  const E = window.AdaptiveEngine;
  const SVG_NS = 'http://www.w3.org/2000/svg';
  const formatDate = value => new Date(value).toLocaleString('ja-JP', {dateStyle: 'medium', timeStyle: 'short'});
  const percent = (n, total) => total ? Math.round(n / total * 100) + '%' : '未測定';
  const scoreText = rate => rate?.total ? rate.correct + '/' + rate.total +
    (rate.self ? '（自己評価 ' + rate.self + ' 件）' : '') : '未測定';
  const displayAnswer = (question, value) => {
    if (question.options) return [value].flat().map(index => question.options[index]).join('／');
    if (question.type === 'boolean' && typeof value === 'boolean') return value ? '正しい' : '誤り';
    return [value].flat().join('／');
  };
  const uid = () => window.crypto?.randomUUID?.() || Date.now().toString(36) + '-' + Math.random().toString(36).slice(2);
  const today = (at = Date.now()) => {
    const date = new Date(at);
    date.setHours(0, 0, 0, 0);
    return +date;
  };
  const STATUS_CLASS = {
    '未学習': 'unseen',
    '学習済み': 'learning',
    '再学習が必要': 'relearn',
    '習得済み': 'learned',
    '長期定着済み': 'retained'
  };

  function node(tag, value, classes) {
    const element = document.createElement(tag);
    if (value != null) element.textContent = value;
    if (classes) element.className = classes;
    return element;
  }
  function svgNode(tag, attrs = {}, text = null) {
    const element = document.createElementNS(SVG_NS, tag);
    for (const [key, value] of Object.entries(attrs)) element.setAttribute(key, String(value));
    if (text != null) element.textContent = text;
    return element;
  }
  function badge(text, kind = '') {
    return node('span', text, 'as-badge' + (kind ? ' as-' + kind : ''));
  }
  function button(text, action, primary = false) {
    const element = node('button', text, primary ? 'as-primary' : '');
    element.type = 'button';
    element.addEventListener('click', action);
    return element;
  }
  function labeled(text, input) {
    const label = node('label', text);
    label.className = 'as-field';
    label.append(input);
    return label;
  }
  function select(entries, value) {
    const control = node('select');
    for (const [key, name] of entries) {
      const option = node('option', name);
      option.value = key;
      control.append(option);
    }
    control.value = value;
    return control;
  }
  function disclosure(title, ...children) {
    const details = node('details');
    details.append(node('summary', title), ...children);
    return details;
  }
  function table(headings, rows) {
    const wrapper = node('div', null, 'as-table');
    const element = node('table');
    const heading = element.createTHead().insertRow();
    for (const text of headings) {
      const th = node('th', text);
      th.scope = 'col';
      heading.append(th);
    }
    const body = element.createTBody();
    for (const row of rows) {
      const tr = body.insertRow();
      for (const value of row) {
        const cell = tr.insertCell();
        if (value instanceof Node) cell.append(value);
        else cell.textContent = value;
      }
    }
    wrapper.append(element);
    return wrapper;
  }

  function renderPlanBar(plan, activeId = null) {
    const list = node('ol', null, 'as-plan');
    list.setAttribute('aria-label', '学習セッションの構成');
    for (const phase of plan) {
      const item = node('li', null, phase.id === activeId ? 'is-active' : '');
      item.style.flexGrow = String(Math.max(1, Math.round(phase.ratio * 100)));
      const minutes = Math.max(1, Math.round(phase.seconds / 60));
      item.append(node('span', phase.label, 'as-plan-label'), node('span', minutes + '分', 'as-plan-min'));
      list.append(item);
    }
    return list;
  }

  function renderDependencyGraph(model, analysis, available, onSelect) {
    const visibleIds = new Set(available.map(item => item.id));
    const connected = new Set();
    for (const item of available) {
      const prereqs = (item.prerequisites || []).filter(id => visibleIds.has(id));
      if (prereqs.length) {
        connected.add(item.id);
        prereqs.forEach(id => connected.add(id));
      }
    }
    if (!connected.size) return null;
    const nodes = available.filter(item => connected.has(item.id));
    const depth = new Map();
    const visit = (id, stack = new Set()) => {
      if (depth.has(id)) return depth.get(id);
      if (stack.has(id)) return 0;
      stack.add(id);
      const item = model.items.get(id);
      const prereqs = (item?.prerequisites || []).filter(p => connected.has(p));
      const d = prereqs.length ? Math.max(...prereqs.map(p => visit(p, stack))) + 1 : 0;
      stack.delete(id);
      depth.set(id, d);
      return d;
    };
    nodes.forEach(item => visit(item.id));
    const columns = new Map();
    for (const item of nodes) {
      const col = depth.get(item.id) || 0;
      if (!columns.has(col)) columns.set(col, []);
      columns.get(col).push(item);
    }
    const maxCol = Math.max(...columns.keys());
    const maxRows = Math.max(...[...columns.values()].map(list => list.length));
    const colW = 165;
    const rowH = 54;
    const nodeW = 136;
    const nodeH = 34;
    const padX = 16;
    const padY = 18;
    const width = (maxCol + 1) * colW + padX * 2;
    const height = maxRows * rowH + padY * 2;
    const coords = new Map();
    for (const [col, list] of columns.entries()) {
      const offsetY = (maxRows - list.length) * rowH / 2;
      list.forEach((item, row) => {
        coords.set(item.id, {
          x: padX + col * colW,
          y: padY + offsetY + row * rowH
        });
      });
    }
    const wrap = node('div', null, 'as-graph-wrap');
    const svg = svgNode('svg', {
      viewBox: `0 0 ${width} ${height}`,
      width,
      height,
      class: 'as-graph',
      role: 'img',
      'aria-label': '概念の前提関係グラフ'
    });
    const defs = svgNode('defs');
    const marker = svgNode('marker', {
      id: 'as-arrow', markerWidth: 7, markerHeight: 7, refX: 6, refY: 3.5, orient: 'auto'
    });
    marker.append(svgNode('path', {d: 'M0,0 L7,3.5 L0,7 Z', class: 'as-graph-arrow'}));
    defs.append(marker);
    svg.append(defs);
    for (const item of nodes) {
      const to = coords.get(item.id);
      for (const prereq of item.prerequisites || []) {
        const from = coords.get(prereq);
        if (!from) continue;
        const x1 = from.x + nodeW;
        const y1 = from.y + nodeH / 2;
        const x2 = to.x;
        const y2 = to.y + nodeH / 2;
        const mid = (x1 + x2) / 2;
        svg.append(svgNode('path', {
          d: `M${x1},${y1} C${mid},${y1} ${mid},${y2} ${x2},${y2}`,
          class: 'as-graph-edge',
          'marker-end': 'url(#as-arrow)'
        }));
      }
    }
    for (const item of nodes) {
      const pos = coords.get(item.id);
      const status = E.level(item, analysis.states.get(item.id));
      const g = svgNode('g', {
        class: 'as-graph-node as-status-' + (STATUS_CLASS[status] || 'unseen'),
        tabindex: '0',
        role: 'button',
        'aria-label': (item.mapTitle || item.title) + '（' + status + '）'
      });
      const label = (item.mapTitle || item.title);
      const shortLabel = label.length > 11 ? label.slice(0, 10) + '…' : label;
      g.append(
        svgNode('rect', {x: pos.x, y: pos.y, width: nodeW, height: nodeH, rx: 8}),
        svgNode('text', {x: pos.x + nodeW / 2, y: pos.y + 21, 'text-anchor': 'middle'}, shortLabel)
      );
      g.addEventListener('click', () => onSelect(item));
      g.addEventListener('keydown', event => {
        if (event.key === 'Enter' || event.key === ' ') {
          event.preventDefault();
          onSelect(item);
        }
      });
      svg.append(g);
    }
    wrap.append(svg);
    const legend = node('div', null, 'as-legend');
    for (const [label, cls] of Object.entries(STATUS_CLASS)) {
      const chip = node('span', label, 'as-legend-item as-status-' + cls);
      legend.append(chip);
    }
    const container = node('div', null, 'as-graph-section');
    container.append(node('p', 'ノードをクリックすると下の詳細カードへ移動します。矢印は前提知識の向きです。', 'as-muted'), legend, wrap);
    return container;
  }

  function mount(root, course) {
    const model = E.compile(course);
    let storage;
    try { storage = window.localStorage; } catch {}
    const store = E.makeStore(storage, course.id);
    for (const evidence of course.legacy || []) E.record(model, store, {...evidence, method: 'legacy'});
    const saved = store.read('settings') || {};
    let settings = {
      minutes: E.clamp(Number(saved.minutes) || 15, 5, 90),
      goal: Object.hasOwn(E.GOALS, saved.goal) ? saved.goal : 'balanced',
      topic: course.topics.some(topic => topic.id === saved.topic) ? saved.topic : '',
      examDate: /^\d{4}-\d{2}-\d{2}$/.test(saved.examDate || '') ? saved.examDate : '',
      focus: Boolean(saved.focus)
    };
    let session = store.read('active');
    let analysis = E.build(model, store.events());
    let page = session && !session.finished ? 'session' : 'home';
    let notice = '';
    let refreshPending = false;
    let clock = null;
    let stopClock = () => {};

    const current = () => session?.steps[session.steps.length - 1];
    function sync() { analysis = E.build(model, store.events()); }
    function saveSession() { store.write('active', session); }
    function heading(text) {
      const h = node('h1', text);
      h.tabIndex = -1;
      return h;
    }
    function openPage(target) {
      if (target === 'session') session = store.read('active');
      page = target;
      sync();
      render(true);
    }
    function start() {
      const latest = store.read('active');
      if (latest && !latest.finished) {
        openPage('session');
        return;
      }
      const now = Date.now();
      sync();
      const plan = E.planSession(model, analysis, settings, now);
      session = {id: uid(), startedAt: now, settings: {...settings}, plan, steps: [], retries: [],
        spent: 0, completed: null, finished: false, nextBreak: 25 * 60};
      const step = E.nextStep(model, analysis, session, now);
      if (!step) {
        notice = '現在の条件では復習期限の項目や新規項目がありません。学習範囲を変えるか、次の復習日を確認してください。';
        render(true);
        return;
      }
      saveSession();
      openPage('session');
    }
    function changeSession(action) {
      const latest = store.read('active');
      if (latest?.id === session.id && !latest.finished) {
        session = latest;
        action();
      } else {
        session = latest;
        page = 'home';
        notice = 'このセッションは別のタブで終了しました。';
      }
      sync();
      render(true);
    }
    function advance(step, breakAccepted = false) {
      changeSession(() => {
        if (current()?.id !== step.id) return;
        const event = store.read('attempt:' + step.id);
        E.completeStep(model, session, current(), event);
        if (breakAccepted) session.nextBreak += 25 * 60;
        if (session.spent >= session.nextBreak && session.spent < session.settings.minutes * 60) {
          session.break = true;
        } else {
          session.break = false;
          sync();
          if (!E.nextStep(model, analysis, session, Date.now())) session.finished = Date.now();
        }
        saveSession();
      });
    }
    function end() {
      changeSession(() => {
        if (current()) E.completeStep(model, session, current(), store.read('attempt:' + current().id));
        session.finished = Date.now();
        saveSession();
      });
    }
    function actions(...children) {
      const group = node('div', null, 'as-actions');
      group.append(...children);
      return group;
    }

    function home() {
      const now = Date.now();
      const stats = E.summary(model, analysis, now);
      const recommendation = node('section', null, 'as-card as-intro');
      recommendation.append(heading(course.title + 'の学習'));
      if (course.description) recommendation.append(node('p', course.description));
      const active = session && !session.finished;
      recommendation.append(actions(
        button(active ? 'セッションを再開' : settings.minutes + '分の学習を始める', active ? () => openPage('session') : start, true),
        button('学習設定', () => openPage('settings'))
      ));
      if (settings.topic && model.available().some(item => item.topic === settings.topic)) {
        recommendation.append(node('p', '範囲　' + course.topics.find(topic => topic.id === settings.topic).title, 'as-muted'));
      }
      const previewPlan = active && session.plan ? session.plan : E.planSession(model, analysis, settings, now);
      const preview = active ? null : E.nextStep(model, analysis, {
        id: 'preview', settings, plan: previewPlan, steps: [], retries: [], spent: 0
      }, now);
      if (preview) {
        const first = model.items.get(preview.itemId);
        recommendation.append(node('p', '最初のおすすめ　' + (analysis.states.has(first.id) ? first.title : course.topics.find(topic =>
          topic.id === first.topic)?.title || '事前問題') + '　·　' + preview.reasons[0], 'as-muted'));
      }
      if (stats.misconceptions.length) {
        recommendation.append(node('p', '確信度の高い誤答（誤概念候補）が ' + stats.misconceptions.length + ' 件あります。次の学習で優先的に確認します。', 'as-alert'));
      }
      if (previewPlan.length) {
        recommendation.append(disclosure('本日のセッション時間配分（動的調整）',
          renderPlanBar(previewPlan),
          node('p', '復習期限数・新規項目数・学習目標に応じて配分を自動調整しています（目安値）。', 'as-muted')));
      }
      const tiles = node('dl', null, 'as-tiles');
      const {daily} = stats;
      const attempted = daily.filter(event => event.generated);
      for (const [title, value] of Object.entries({
        '期限を迎えた復習': stats.due + '件',
        '新規項目': stats.fresh + '件',
        '再学習が必要': stats.relearn + '件',
        '今日の自力正答率': percent(attempted.filter(event => event.correct && !event.hints).length, attempted.length),
        '今日のヒント使用率': percent(daily.filter(event => event.hints).length, daily.length)
      })) {
        const tile = node('div');
        tile.append(node('dt', title), node('dd', value));
        tiles.append(tile);
      }
      const buttons = actions(button('学習分析', () => openPage('analysis')), button('知識マップ', () => openPage('map')));
      root.append(recommendation, tiles, buttons);
    }

    function settingsPage() {
      const panel = node('section', null, 'as-card');
      panel.append(heading('学習設定'));
      const form = node('form', null, 'as-settings');
      const minutes = node('input');
      minutes.type = 'number';
      minutes.min = '5';
      minutes.max = '90';
      minutes.required = true;
      minutes.value = settings.minutes;
      const goal = select(Object.entries(E.GOALS), settings.goal);
      const topic = select([['', 'すべて'], ...course.topics.filter(topic => model.available().some(item => item.topic === topic.id))
        .map(topic => [topic.id, topic.title])], settings.topic);
      const exam = node('input');
      exam.type = 'date';
      exam.value = settings.examDate;
      const focus = node('input');
      focus.type = 'checkbox';
      focus.checked = settings.focus;
      const submit = node('button', '設定を保存', 'as-primary');
      submit.type = 'submit';
      form.append(labeled('利用できる時間（分）', minutes), labeled('学習の目標', goal), labeled('優先する範囲', topic),
        labeled('試験・実践の予定日（任意）', exam), labeled('集中モード', focus), actions(submit));
      form.addEventListener('submit', event => {
        event.preventDefault();
        settings = {minutes: +minutes.value, goal: goal.value, topic: topic.value, examDate: exam.value, focus: focus.checked};
        store.write('settings', settings);
        openPage('home');
      });
      const exportData = button('回答履歴を書き出す', () => {
        const payload = {format: 'adaptive-study-v1', subject: course.id, attempts: store.events()};
        const url = URL.createObjectURL(new Blob([JSON.stringify(payload, null, 2)], {type: 'application/json'}));
        const link = node('a');
        link.href = url;
        link.download = course.id + '-learning-history.json';
        link.click();
        setTimeout(() => URL.revokeObjectURL(url), 1500);
      });
      const file = node('input');
      file.type = 'file';
      file.accept = 'application/json,.json';
      file.addEventListener('change', async () => {
        if (!file.files.length) return;
        try {
          const data = JSON.parse(await file.files[0].text());
          if (data.format !== 'adaptive-study-v1' || data.subject !== course.id || !Array.isArray(data.attempts)) throw new Error();
          let imported = 0;
          for (const record of data.attempts) {
            if (record && !store.read('attempt:' + record.id) && E.record(model, store, record)) imported++;
          }
          notice = '回答履歴を ' + imported + ' 件追加しました。';
          sync();
        } catch {
          notice = 'この科目の回答履歴ファイルを選んでください。';
        }
        render(true);
      });
      panel.append(form, disclosure('回答履歴のバックアップ',
        node('p', '取り込みは記録 ID が異なる回答を追加します。'), actions(exportData), labeled('履歴を取り込む', file)));
      root.append(panel);
    }

    function answerControl(question, initial) {
      const discrete = ['choice', 'multiple', 'boolean'].includes(question.type);
      if (discrete) {
        const container = node('fieldset', null, 'as-options');
        container.append(node('legend', question.type === 'multiple' ? '当てはまるものをすべて選ぶ' : '回答を選ぶ'));
        const values = question.options || ['正しい', '誤り'];
        const controls = values.map((option, index) => {
          const control = node('input');
          control.type = question.type === 'multiple' ? 'checkbox' : 'radio';
          control.name = 'adaptive-response';
          const value = question.options || question.type !== 'boolean' || typeof question.answer !== 'boolean' ? index : !index;
          control.checked = Array.isArray(initial) ? initial.includes(value) : initial === value;
          const label = node('label');
          label.append(control, node('span', option));
          container.append(label);
          return {control, value};
        });
        return {control: container, controls, value: () => {
          const selected = controls.filter(entry => entry.control.checked).map(entry => entry.value);
          return question.type === 'multiple' ? (selected.length ? selected : null) : selected[0] ?? null;
        }};
      }
      const field = node(['free', 'explain', 'reason', 'example', 'blank'].includes(question.type) ? 'textarea' : 'input');
      field.autocomplete = 'off';
      field.value = initial || '';
      if (question.type === 'numeric') field.inputMode = 'decimal';
      return {control: labeled(question.type === 'flash' ? 'カードの裏を思い出して入力' : '回答', field),
        controls: null,
        value: () => field.value.trim() || null};
    }

    function feedback(panel, entry, sessionStep, savedAttempt, selfReview) {
      const {item, question} = entry;
      const correction = node('section', null, 'as-feedback');
      const title = savedAttempt ? (savedAttempt.correct ? '正答' : '訂正と再学習') : '模範解答と採点基準';
      correction.append(node('h2', title));
      correction.append(node('p', displayAnswer(question, question.answer), 'as-answer'));
      if (question.feedback) correction.append(node('p', question.feedback));
      if (question.rubric?.length) {
        const list = node('ul');
        question.rubric.forEach(text => list.append(node('li', text)));
        correction.append(list);
      }
      if (item.example) {
        const fading = E.fadingLevel(item, analysis.states.get(item.id));
        const label = fading === 'worked' ? '解答例・具体例（基礎確認）' :
          fading === 'faded' ? '解答例・具体例（段階的支援）' : '解答例・具体例（参考）';
        const ex = disclosure(label, node('p', item.example, 'as-answer'));
        if (fading === 'worked' && (!savedAttempt?.correct || sessionStep.pretest) && !sessionStep.retry) {
          ex.open = true;
        }
        correction.append(ex);
      }
      if (item.reference) {
        const link = node('a', '関連教材を開く');
        link.href = item.reference;
        correction.append(link);
      }
      if (savedAttempt?.confidence === 3 && !savedAttempt.correct) {
        correction.append(node('p', '高い確信度での誤答でした（誤概念候補）。判断の根拠を見直してください。次回はこの問題の復習を優先します。', 'as-alert'));
      }
      if (savedAttempt) {
        if (savedAttempt.method === 'self') correction.append(node('p', '自己評価を記録しました。', 'as-muted'));
        if (!savedAttempt.correct) {
          const advice = node('p', savedAttempt.cause ? (E.CAUSE_GUIDANCE[savedAttempt.cause] || '') : '', 'as-guidance');
          advice.setAttribute('aria-live', 'polite');
          const cause = select([['', '選択しない'], ...E.CAUSES.map(value => [value, value])], savedAttempt.cause || '');
          cause.addEventListener('change', () => {
            const latest = store.read('attempt:' + savedAttempt.id);
            store.write('attempt:' + savedAttempt.id, {...latest, cause: cause.value || null});
            advice.textContent = cause.value ? (E.CAUSE_GUIDANCE[cause.value] || '') : '';
          });
          correction.append(labeled('誤答の原因（任意）', cause), advice);
        }
      }
      if (selfReview) {
        correction.append(actions(button('基準を満たした', () => selfReview(true), true),
          button('要復習', () => selfReview(false))));
      } else {
        correction.append(actions(button('次へ進む', () => advance(sessionStep), true)));
      }
      panel.append(correction);
    }

    function sessionPage() {
      const panel = node('section', null, 'as-card as-session');
      root.append(panel);
      if (!session || session.finished) {
        const attempts = analysis.history.filter(event => event.sessionId === session?.id);
        const independentCorrect = attempts.filter(event => event.correct && !event.hints && event.generated).length;
        const highConfErrors = attempts.filter(event => !event.correct && event.confidence === 3).length;
        panel.append(heading('セッション終了'),
          node('p', '記録した回答 ' + attempts.length + '件　·　自力正答 ' + independentCorrect + '件' +
            (highConfErrors ? '　·　優先修正フラグ ' + highConfErrors + '件' : '')));
        if (attempts.length) {
          const byAxis = Object.entries(E.AXES).map(([axis, label]) => {
            const rows = attempts.filter(event => event.axis === axis);
            return rows.length ? [label, rows.filter(e => e.correct).length + '/' + rows.length] : null;
          }).filter(Boolean);
          if (byAxis.length) panel.append(table(['このセッションの能力別結果', '正答 / 回答'], byAxis));
        }
        const next = [...analysis.states.values()].filter(state => state.due > Date.now()).reduce((min, state) => Math.min(min, state.due), Infinity);
        if (next !== Infinity) panel.append(node('p', '次の復習予定　' + formatDate(next)));
        panel.append(actions(button('学習ホームへ', () => openPage('home'), true), button('学習分析', () => openPage('analysis'))));
        return;
      }
      const step = current();
      if (session.break) {
        panel.append(heading('休憩を取りませんか'),
          node('p', '画面から目を離して少し休んだら再開できます。'),
          actions(button('学習を続ける', () => advance(step, true), true), button('ここで終了', end)));
        return;
      }
      const entry = model.questions.get(step?.questionId);
      if (!entry || !model.available().includes(entry.item)) {
        panel.append(heading('現在の出題範囲を確認してください'),
          node('p', '出題範囲を戻すと途中のセッションを再開できます。'),
          actions(button('ここで終了', end)));
        return;
      }
      const {item, question} = entry;
      const earned = store.read('attempt:' + step.id);
      const draft = store.read('draft:' + step.id) || {};
      const progressRatio = E.clamp(session.spent / Math.max(1, session.settings.minutes * 60), 0, 1);
      const progressBar = node('div', null, 'as-progress');
      const fill = node('span');
      fill.style.width = Math.round(progressRatio * 100) + '%';
      progressBar.append(fill);
      const phaseName = step.phase && E.PHASES[step.phase] ? E.PHASES[step.phase] + '　·　' : '';
      const meta = node('p', phaseName + (step.retry ? '訂正後の再検索' : step.pretest ? '事前問題' : E.AXES[question.axis]) +
        '　·　' + session.steps.length + '問目　·　残り目安 ' + Math.max(0, Math.ceil(session.settings.minutes - session.spent / 60)) + '分', 'as-muted');
      panel.append(actions(button('一時中断', () => openPage('home')), button('セッションを終了', end)),
        progressBar, meta);
      if (question.lens && E.ELABORATION_LENSES[question.lens]) {
        const lensRow = node('p', null, 'as-lens-row');
        lensRow.append(badge(E.ELABORATION_LENSES[question.lens], 'lens'));
        panel.append(lensRow);
      }
      panel.append(heading(question.prompt));
      if (step.reasons.length) {
        panel.append(disclosure('出題の理由', node('p', step.reasons.join('、'))));
      }
      const persistDraft = values => {
        Object.assign(draft, values);
        store.write('draft:' + step.id, draft);
      };
      function saveAnswer(correct, response, method) {
        const latest = store.read('active');
        if (latest?.id !== session.id || latest.finished || latest.steps.at(-1)?.id !== step.id) {
          session = latest;
          render(true);
          return;
        }
        stopClock();
        E.record(model, store, {
          id: step.id, itemId: item.id, questionId: question.id, at: Date.now(), sessionId: session.id,
          correct, response, method, hints: draft.hints || 0, confidence: draft.confidence || null,
          seconds: draft.seconds ?? 0
        });
        store.write('draft:' + step.id, null);
        sync();
        render(true);
      }
      if (earned || draft.review) {
        if (earned?.method === 'unable') {
          panel.append(node('p', '「思い出せない」を学習記録に保存しました。'));
        } else {
          const response = earned?.response ?? draft.response;
          panel.append(node('p', 'あなたの回答　' + displayAnswer(question, response), 'as-answer'));
        }
        feedback(panel, entry, step, earned, !earned && draft.review ? correct => saveAnswer(correct, draft.response, 'self') : null);
        return;
      }
      const form = node('form');
      const control = answerControl(question, draft.response);
      const confidence = select([['', '未入力'], ['1', '低い'], ['2', '中程度'], ['3', '高い']], String(draft.confidence || ''));
      const hints = node('ol', null, 'as-hints');
      hints.setAttribute('aria-live', 'polite');
      const clues = (question.hints || []).filter(Boolean);
      clues.slice(0, draft.hints || 0).forEach(clue => hints.append(node('li', clue)));
      const hint = button('ヒントを見る', () => {
        const unlocked = (draft.hints || 0) + 1;
        persistDraft({hints: unlocked});
        hints.append(node('li', clues[unlocked - 1]));
        hint.disabled = unlocked >= clues.length;
      });
      hint.disabled = clues.length <= (draft.hints || 0);
      const submit = node('button', '回答を確認', 'as-primary');
      submit.type = 'submit';
      const prompt = node('p', '', 'as-muted');
      prompt.setAttribute('role', 'status');
      const controls = actions(submit, button('思い出せない', () => {
        saveAnswer(false, '', 'unable');
      }));
      if (clues.length) controls.append(hint);
      form.append(control.control, labeled('回答への確信度（任意）', confidence), hints, controls, prompt,
        node('p', 'ショートカット：Ctrl/Cmd + Enter で回答を確認', 'as-muted as-shortcut'));
      form.addEventListener('input', () => persistDraft({response: control.value(), confidence: +confidence.value}));
      form.addEventListener('keydown', event => {
        if (event.key === 'Enter' && (event.ctrlKey || event.metaKey) && !event.isComposing) {
          form.requestSubmit();
          return;
        }
        const tag = event.target?.tagName;
        const typing = tag === 'TEXTAREA' || tag === 'SELECT' ||
          (tag === 'INPUT' && !['radio', 'checkbox'].includes(event.target.type));
        if (typing || event.ctrlKey || event.metaKey || event.altKey) return;
        if (event.key === 'h' && clues.length && !hint.disabled) {
          event.preventDefault();
          hint.click();
        } else if (control.controls && /^[1-9]$/.test(event.key)) {
          const targetControl = control.controls[Number(event.key) - 1]?.control;
          if (targetControl) {
            event.preventDefault();
            targetControl.click();
            targetControl.focus();
          }
        }
      });
      form.addEventListener('submit', event => {
        event.preventDefault();
        const response = control.value();
        if (response === null) {
          prompt.textContent = '回答を入力するか、「思い出せない」を選んでください。';
          return;
        }
        stopClock();
        persistDraft({response, confidence: +confidence.value});
        const correct = E.grade(question, response);
        if (correct === null) {
          persistDraft({review: true});
          render(true);
        } else {
          saveAnswer(correct, response, 'auto');
        }
      });
      panel.append(form);
      let lastTick = performance.now();
      const tick = () => {
        const now = performance.now();
        if (form.isConnected && !root.closest('[hidden]') && !document.hidden) {
          persistDraft({seconds: (draft.seconds || 0) + Math.min(1.5, (now - lastTick) / 1000)});
        }
        lastTick = now;
      };
      clock = setInterval(tick, 1000);
      stopClock = () => {
        if (clock === null) return;
        clearInterval(clock);
        clock = null;
        tick();
      };
    }

    function analysisPage() {
      const panel = node('section', null, 'as-card');
      panel.append(heading('学習分析'));
      const legendRow = node('p', null, 'as-badge-row');
      legendRow.append(badge('実測', 'observed'), node('span', ' 回答履歴に基づく値　'), badge('モデル推定', 'estimated'), node('span', ' FSRSモデルによる推定値'));
      panel.append(legendRow);
      const now = Date.now();
      const {native, learned, memory, misconceptions, calibration} = E.summary(model, analysis, now);
      const results = events => [String(events.filter(event => event.correct).length), String(events.length),
        percent(events.filter(event => event.correct).length, events.length),
        String(events.filter(event => event.method === 'self').length)];
      const initial = native.filter(event => event.firstSeen && !event.hints && event.generated);
      const transfer = initial.filter(event => event.axis === 'transfer');
      const time = native.reduce((sum, event) => sum + (event.seconds ?? 0), 0);
      const efficiency = time ? (learned * 3600 / time).toLocaleString('ja-JP', {maximumFractionDigits: 1}) + '項目 / 回答1時間' : '未測定';
      const observed = table(['実測・自己評価の記録', '正答', '回答', '正答率', '自己評価'], [
        ['初見・自力', ...results(initial)], ['初見への応用・自力', ...results(transfer)]
      ]);
      const intervals = ['1日以上3日未満', '3日以上7日未満', '7日以上30日未満', '30日以上'];
      const retained = [0, 1, 2, 3].map(() => ({correct: 0, total: 0, self: 0}));
      for (const state of analysis.states.values()) {
        state.retention.forEach((rate, index) => {
          Object.keys(rate).forEach(key => { retained[index][key] += rate[key]; });
        });
      }
      const retention = table(['前回の再現からの経過', '正答 / 回答', '正答率'], intervals.map((label, index) => [
        label, scoreText(retained[index]), percent(retained[index].correct, retained[index].total)
      ]));
      const risk = memory.length ? percent(memory.reduce((a, b) => a + b, 0), memory.length) + '（' + memory.length + '項目）' : '未推定';
      const confidence = table(['確信度', '正答', '回答', '正答率', '自己評価'],
        ['低い', '中程度', '高い'].map((label, index) => [label, ...results(native.filter(event => event.confidence === index + 1))]));
      const calibrationNote = node('p', calibration.rated
        ? '過信（高確信で誤答）：' + percent(calibration.overconfident, calibration.highTotal) +
          '（' + calibration.overconfident + '/' + calibration.highTotal + '） ｜ 過小評価（低確信で正答）：' +
          percent(calibration.underconfident, calibration.lowTotal) +
          ' ｜ キャリブレーション誤差目安：' + calibration.brier.toFixed(2)
        : '確信度を入力して回答すると、メタ認知の精度（過信・過小評価）を分析できます。', 'as-muted');
      const errors = native.filter(event => !event.correct);
      const causes = table(['誤答原因（自己申告）', '件数'], [...E.CAUSES, null].map(cause => [
        cause || '未分類', String(errors.filter(event => event.cause === cause).length)
      ]));
      const misconceptionView = misconceptions.length
        ? table(['項目', '記録日時', '原因'], misconceptions.map(entry => [
          entry.title, formatDate(entry.at), entry.cause || '未分類'
        ]))
        : node('p', '現在、優先修正が必要な高確信誤答はありません。', 'as-muted');
      const load = new Map();
      const start = today(now);
      for (const state of analysis.states.values()) {
        const day = state.due > now ? today(state.due) : start;
        load.set(day, (load.get(day) || 0) + 1);
      }
      const upcoming = Array.from({length: 7}, (_, index) => {
        const date = new Date(start);
        date.setDate(date.getDate() + index);
        return [index ? date.toLocaleDateString('ja-JP', {month: 'numeric', day: 'numeric'}) : '本日（期限超過を含む）',
          String(load.get(+date) || 0)];
      });
      const workload = table(['復習予定', '項目数'], upcoming);
      const topics = table(['範囲', '学習記録あり', '習得・長期定着'], course.topics.map(topic => {
        const items = model.available().filter(item => item.topic === topic.id);
        return [topic.title, String(items.filter(item => analysis.states.has(item.id)).length),
          String(items.filter(item => ['習得済み', '長期定着済み'].includes(E.level(item, analysis.states.get(item.id)))).length)];
      }));
      const estBox = node('div', null, 'as-estimated-box');
      estBox.append(badge('モデル推定', 'estimated'),
        node('p', '平均 ' + risk + '。自力再現を記録できた記憶項目を対象とする、FSRS型べき乗忘却モデルによる推定値です（実測値ではありません）。'),
        node('p', '上の「経過日数別の再現率」は実際の回答結果に基づく実測値です。'));
      const ready = E.readiness(model, analysis, now);
      const readyTable = table(['実測ベースの到達指標', '現在値', '目標目安', '判定'], ready.checks.map(c => [
        c.label, c.value, c.target, c.met ? '達成' : '未達'
      ]));
      const readyBox = disclosure('合格・到達ライン確認（' + ready.metCount + '/4 達成）',
        node('p', '合格を保証する予測値ではなく、記憶・初見応用・長期定着・誤概念解消の実測基準です。', 'as-muted'),
        readyTable);
      if (ready.priorities.length) {
        const list = node('ul');
        ready.priorities.forEach(p => list.append(node('li', p.title + '（' + p.status + '）')));
        readyBox.append(node('h3', '次に優先して強化すべき項目'), list);
      }
      readyBox.open = true;
      panel.append(readyBox, observed,
        disclosure('経過日数別の再現率（実測）', retention),
        disclosure('語彙・用語の想起確率（モデル推定）', estBox),
        disclosure('確信度と正答の関係（メタ認知）', confidence, calibrationNote),
        disclosure('優先修正が必要な誤概念（' + misconceptions.length + '件）', misconceptionView),
        disclosure('誤答の原因', causes),
        disclosure('今後7日間の復習負荷', workload),
        disclosure('範囲別の習熟状況', topics, node('p', '現在の習得数を累積回答時間で割った目安　' + efficiency + '。教材を読む時間と旧履歴を含みません。')));
      root.append(panel);
    }

    function knowledgeMap() {
      const panel = node('section', null, 'as-card');
      panel.append(heading('知識マップ'));
      const available = model.available();
      const topic = select([['', 'すべて'], ...course.topics.filter(entry => available.some(item => item.topic === entry.id))
        .map(entry => [entry.id, entry.title])], settings.topic);
      const search = node('input');
      search.type = 'search';
      const status = select([['', 'すべて'], ...['未学習', '学習済み', '再学習が必要', '習得済み', '長期定着済み']
        .map(value => [value, value])], '');
      const results = node('div', null, 'as-map');
      let offset = 0;
      const focusItem = targetItem => {
        topic.value = status.value = '';
        search.value = targetItem.mapTitle || targetItem.title;
        offset = 0;
        list();
        const row = results.querySelector('details');
        if (row) {
          row.open = true;
          row.querySelector('summary').focus();
        }
      };
      const graph = renderDependencyGraph(model, analysis, available, focusItem);
      function list() {
        const items = available.filter(item => (!topic.value || item.topic === topic.value) &&
          (!status.value || E.level(item, analysis.states.get(item.id)) === status.value) &&
          (!search.value || (item.mapTitle || item.title).toLocaleLowerCase().includes(search.value.trim().toLocaleLowerCase())));
        const total = items.length;
        const visible = items.slice(offset, offset + 20);
        const previous = button('前の項目', () => { offset = Math.max(0, offset - 20); list(); });
        const next = button('次の項目', () => { offset += 20; list(); });
        previous.disabled = !offset;
        next.disabled = offset + 20 >= total;
        const label = node('p', total ? (offset + 1) + '–' + (offset + visible.length) + ' / ' + total + '項目' : '条件に合う項目がありません。', 'as-muted');
        label.setAttribute('role', 'status');
        results.replaceChildren(label);
        for (const item of visible) {
          const state = analysis.states.get(item.id);
          const detail = disclosure((item.mapTitle || item.title) + '　' + E.level(item, state));
          detail.addEventListener('toggle', () => {
            if (!detail.open || detail.children.length > 1) return;
            const links = (item.prerequisites || []).map(id => {
              const required = model.items.get(id);
              return button((required.mapTitle || required.title) + ' · ' + E.level(required, analysis.states.get(id)), () => focusItem(required));
            });
            if (links.length) detail.append(node('h3', '前提知識'), actions(...links));
            const confusableLinks = (item.confusables || [])
              .filter(id => model.items.has(id) && available.some(a => a.id === id))
              .map(id => {
                const other = model.items.get(id);
                return button((other.mapTitle || other.title) + ' · ' + E.level(other, analysis.states.get(id)), () => focusItem(other));
              });
            if (confusableLinks.length) detail.append(node('h3', '混同しやすい概念（交互学習対象）'), actions(...confusableLinks));
            if (state) {
              const obs = node('p');
              obs.append(badge('実測', 'observed'),
                node('span', ' 回答 ' + state.total + '件 ｜ 正答 ' + state.correct + '件 ｜ 誤答 ' + (state.total - state.correct) +
                  '件 ｜ 連続正答 ' + state.streak + '回'));
              detail.append(node('p', '次回復習　' + formatDate(state.due) + ' ｜ 最終回答　' + formatDate(state.last)), obs);
              if (item.kind === 'memory') {
                const chance = E.probability(state, Date.now());
                if (chance !== null) {
                  const est = node('p');
                  est.append(badge('モデル推定', 'estimated'),
                    node('span', ' 想起確率 ' + percent(chance, 1) +
                      ' ｜ 難易度 ' + state.difficulty.toFixed(1) + '/10 ｜ 安定性 ' + state.stability.toFixed(1) + '日'));
                  detail.append(est);
                }
              }
              if (state.misconception) detail.append(node('p', '確信度の高い誤答の再確認が必要です。', 'as-alert'));
            }
            detail.append(table(['能力', '正答 / 独立した回答'], Object.entries(E.AXES).map(([axis, name]) => [name, scoreText(state?.axes[axis])])));
            if (item.reference) {
              const link = node('a', '教材を開く');
              link.href = item.reference;
              detail.append(link);
            }
          });
          results.append(detail);
        }
        if (total > 20) results.append(actions(previous, next));
      }
      [topic, search, status].forEach(control => control.addEventListener('input', () => { offset = 0; list(); }));
      const filters = node('div', null, 'as-settings');
      filters.append(labeled('テーマ', topic), labeled('項目名', search), labeled('習熟状態', status));
      if (graph) {
        const graphBox = disclosure('前提知識の依存関係グラフ', graph);
        graphBox.open = available.length <= 30;
        panel.append(graphBox);
      }
      panel.append(filters, results);
      root.append(panel);
      list();
    }

    function render(moveFocus = false) {
      stopClock();
      root.classList.add('adaptive-study');
      root.classList.toggle('as-focused', page === 'session' && !session?.finished && Boolean(session?.settings.focus));
      root.replaceChildren();
      if (page !== 'home' && page !== 'session') root.append(button('学習ホームへ', () => openPage('home')));
      ({home, settings: settingsPage, session: sessionPage, analysis: analysisPage, map: knowledgeMap}[page] || home)();
      if (notice || !store.available()) {
        const status = node('p', [notice, store.available() ? '' : 'ブラウザーに保存できません。現在のページ内に一時保持します。'].filter(Boolean).join(' '), 'as-notice');
        status.setAttribute('role', 'status');
        root.prepend(status);
        notice = '';
      }
      if (moveFocus) root.querySelector('h1')?.focus();
    }
    function refresh() {
      sync();
      if (page !== 'session' || !root.querySelector('input,textarea')) render();
    }
    window.addEventListener('storage', event => {
      const prefix = 'adaptive-study:v1:' + course.id + ':';
      if (event.key && (!event.key.startsWith(prefix) || event.key.startsWith(prefix + 'draft:'))) return;
      if (refreshPending) return;
      refreshPending = true;
      setTimeout(() => {
        refreshPending = false;
        if (page !== 'session') {
          session = store.read('active');
          refresh();
        }
      }, 0);
    });
    window.addEventListener('pagehide', () => stopClock());
    window.addEventListener('pageshow', event => { if (event.persisted) render(); });
    render();
    return {
      refresh,
      recordLegacy(evidence) {
        if (!evidence) return;
        E.record(model, store, {...evidence, method: 'legacy'});
        refresh();
      }
    };
  }
  window.AdaptiveStudy = {mount};
})();
