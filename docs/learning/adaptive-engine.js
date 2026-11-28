'use strict';
(function (global) {
  const DAY = 86400000;
  const MINUTE = 60000;
  const TARGET_RETENTION = 0.85;
  const FSRS_FACTOR = (1 - TARGET_RETENTION) / TARGET_RETENTION;
  const AXES = {
    recognition: '認識', recall: '自力再現', understanding: '原理の説明',
    discrimination: '類似概念の識別', application: '定型への適用',
    transfer: '初見への応用', retention: '時間をおいた再現'
  };
  const CAUSES = [
    '知識不足', '思い出せなかった', '概念を誤認した', '類似概念と混同した',
    '前提条件を見落とした', '推論を誤った', '解法を選び違えた', '計算・操作ミス', '問題文を読み違えた'
  ];
  const CAUSE_GUIDANCE = {
    '知識不足': '関連教材と解答例を確認し、まず自分の言葉で要点を1〜2文にまとめてから再挑戦しましょう。',
    '思い出せなかった': '検索の失敗は定着のきっかけになります。ヒントや手がかりを思い浮かべながら時間を置いて再度思い出しましょう。',
    '概念を誤認した': '「なぜ成立するか（Why）」と「成立しない反例（Counterexample）」を確認して定義を修正しましょう。',
    '類似概念と混同した': '混同した概念同士を並べて「何が違うか（Comparison）」を言語化してから再演習しましょう。',
    '前提条件を見落とした': '解法を選ぶ前に「どんな条件で成立するか（When）」を最初に確認する習慣をつけましょう。',
    '推論を誤った': '途中経過を一段階ずつ言葉にして、飛躍や逆の論理がないか検証しましょう。',
    '解法を選び違えた': '問題の構造（データの尺度・独立性・目的など）から手法を選ぶ基準を整理しましょう。',
    '計算・操作ミス': '符号・分母・単位・境界条件など検算ポイントを1つ決めて見直しましょう。',
    '問題文を読み違えた': '問われている対象（条件付きか同時か、否定か肯定か等）に印を付けてから回答しましょう。'
  };
  const ELABORATION_LENSES = {
    what: 'What：それは何か',
    why: 'Why：なぜ成立するのか',
    how: 'How：どのような仕組みか',
    when: 'When：どんな条件で成立するか',
    counterexample: 'Counterexample：成立しない例は何か',
    comparison: 'Comparison：類似概念と何が違うか',
    application: 'Application：どんな場面で使えるか'
  };
  const PHASES = {
    retrieval: '既習の検索練習',
    acquisition: '新規知識の獲得',
    elaboration: '自己説明・構造化',
    practice: '応用・交互学習',
    review: '誤答分析・再確認'
  };
  const GOALS = {balanced: 'バランスよく', recall: '定着を優先', transfer: '理解・応用を優先'};
  const clamp = (value, low, high) => Math.min(high, Math.max(low, value));
  const independent = event => !event.hints && event.method !== 'legacy' && event.axis !== 'recognition';
  const retentionEligible = event => independent(event) && event.generated && event.axis === 'recall';
  const rate = () => ({correct: 0, total: 0, self: 0});
  const addRate = (summary, event) => {
    summary.total++;
    summary.correct += Number(event.correct);
    summary.self += Number(event.method === 'self');
  };
  // FSRS-inspired power-law forgetting curve calibrated so R(S, S) = 0.85.
  const probability = (state, now) => state?.memoryAt == null ? null :
    Math.pow(1 + FSRS_FACTOR * Math.max(0, now - state.memoryAt) / (DAY * state.stability), -1);

  function compile(course) {
    const items = new Map(course.items.map(item => [item.id, item]));
    if (items.size !== course.items.length) throw new Error('学習項目 ID が重複しています。');
    const questions = new Map();
    const downstream = new Map();
    for (const item of course.items) {
      for (const prerequisite of item.prerequisites || []) {
        if (!items.has(prerequisite)) throw new Error('前提項目が存在しません。 ' + prerequisite);
        downstream.set(prerequisite, (downstream.get(prerequisite) || 0) + 1);
      }
      for (const question of item.questions) {
        if (questions.has(question.id)) throw new Error('問題 ID が重複しています。 ' + question.id);
        questions.set(question.id, {item, question});
      }
    }
    const available = () => !course.getScope ? course.items : course.items.filter(
      item => !item.scope || item.scope === course.getScope()
    );
    return {course, items, questions, downstream, available};
  }

  function makeStore(storage, subject) {
    const base = 'adaptive-study:v1:' + subject + ':';
    const cache = new Map();
    let failed = !storage;
    function read(key) {
      if (cache.has(key)) return cache.get(key);
      try {
        return JSON.parse(storage?.getItem(base + key) ?? 'null');
      } catch {
        failed = true;
        return null;
      }
    }
    function write(key, value) {
      try {
        if (!storage) throw new Error('Storage unavailable');
        if (value === null) storage.removeItem(base + key);
        else storage.setItem(base + key, JSON.stringify(value));
        cache.delete(key);
      } catch {
        cache.set(key, value);
        failed = true;
      }
    }
    function events() {
      const keys = new Set(cache.keys());
      try {
        for (let index = 0; index < (storage?.length || 0); index++) {
          const key = storage.key(index);
          if (key?.startsWith(base + 'attempt:')) keys.add(key.slice(base.length));
        }
      } catch {
        failed = true;
      }
      return [...keys].filter(key => key.startsWith('attempt:')).map(read).filter(Boolean);
    }
    return {read, write, events, available: () => !failed};
  }

  function eventFor(model, input) {
    const entry = model.questions.get(input.questionId);
    if (!entry || entry.item.id !== input.itemId || typeof input.correct !== 'boolean' ||
        typeof input.id !== 'string' || !input.id || !Number.isFinite(input.at) ||
        input.at < 0 || input.at > 8640000000000000) return null;
    return {
      id: input.id, itemId: input.itemId, questionId: input.questionId, at: input.at,
      axis: entry.question.axis, correct: input.correct,
      generated: !['choice', 'multiple', 'boolean'].includes(entry.question.type),
      sessionId: typeof input.sessionId === 'string' ? input.sessionId : null,
      method: ['legacy', 'self', 'unable'].includes(input.method) ? input.method : 'auto',
      response: typeof input.response === 'string' || typeof input.response === 'boolean' ||
        Number.isFinite(input.response) || (Array.isArray(input.response) && input.response.every(Number.isInteger)) ?
        input.response : '', hints: clamp(Math.floor(Number(input.hints) || 0), 0, 20),
      seconds: Number.isFinite(input.seconds) ? Math.max(0, input.seconds) : null,
      confidence: [1, 2, 3].includes(input.confidence) ? input.confidence : null,
      cause: !input.correct && CAUSES.includes(input.cause) ? input.cause : null
    };
  }

  function record(model, store, input) {
    const event = eventFor(model, input);
    if (!event) return null;
    const key = 'attempt:' + event.id;
    const previous = store.read(key);
    if (previous) return previous;
    store.write(key, event);
    return event;
  }

  function inferRating(event) {
    if (!event.correct) return 1; // Again
    if (event.hints > 0 || event.confidence === 1) return 2; // Hard
    if (event.confidence === 3 && (event.seconds == null || event.seconds <= 30)) return 4; // Easy
    return 3; // Good
  }

  function update(state, event, item) {
    const isIndependent = independent(event);
    const gap = state.last == null ? null : Math.max(0, (event.at - state.last) / DAY);
    const sameSession = event.sessionId && state.sessions.has(event.sessionId);
    const axisSession = event.sessionId && state.sessions.has(event.sessionId + ':' + event.axis);
    const previous = state.byQuestion.get(event.questionId);
    const firstSeen = !previous;
    const qualified = isIndependent && !sameSession;
    const previousAxisExposure = state.exposures[event.axis];
    const delayed = retentionEligible(event) && qualified && previousAxisExposure != null &&
      event.at - previousAxisExposure >= DAY;
    const stat = state.axes[event.axis] ||= rate();
    if (!axisSession && (isIndependent || event.axis === 'recognition')) {
      if (event.axis !== 'transfer' || firstSeen) addRate(stat, event);
    }
    if (delayed) {
      addRate(state.axes.retention ||= rate(), event);
      const retentionGap = (event.at - previousAxisExposure) / DAY;
      addRate(state.retention[retentionGap < 3 ? 0 : retentionGap < 7 ? 1 : retentionGap < 30 ? 2 : 3], event);
      if (event.correct) {
        state.lastDelayedSuccess = event.at;
        if (retentionGap >= 7) state.lastWeekSuccess = event.at;
      }
    }
    state.total++;
    state.correct += Number(event.correct);
    state.lapses += Number(!event.correct);
    state.streak = event.correct ? state.streak + 1 : 0;
    const rating = inferRating(event);
    const lastFailure = state.failureAt;
    const weakness = state.misconception;
    if (!event.correct) {
      state.failureAt = event.at;
      state.needsReview = true;
      state.difficulty = clamp(state.difficulty + (event.confidence === 3 ? .9 : .55), 1, 10);
      state.stability = clamp(state.stability * .55, .25, 365);
      if (event.confidence === 3) {
        const recordEntry = {at: event.at, questionId: event.questionId, cause: event.cause};
        state.misconception = recordEntry;
        state.misconceptions.set(event.questionId, recordEntry);
      }
      state.due = event.at + 10 * MINUTE;
    } else if (qualified) {
      const retrievability = state.memoryAt != null
        ? probability(state, event.at)
        : gap != null
          ? Math.pow(1 + FSRS_FACTOR * gap / Math.max(0.25, state.stability), -1)
          : TARGET_RETENTION;
      const gradeFactor = rating === 2 ? .72 : rating === 4 ? 1.25 : 1;
      const spacingGain = Math.exp(1.25 * (1 - retrievability)) - 1;
      const growth = 1 + (.28 + (10 - state.difficulty) * .11 + spacingGain * 1.45) *
        Math.pow(Math.max(.5, state.stability), -.18) * gradeFactor;
      const spaced = state.memoryAt === null || event.at - state.memoryAt >= 20 * 60 * MINUTE;
      if (spaced) {
        state.stability = clamp(Math.max(1, state.stability * growth), 1, 365);
        const deltaD = rating === 2 ? .2 : rating === 4 ? -.45 : -.2;
        state.difficulty = clamp(state.difficulty + deltaD, 1, 10);
      }
      if (item.kind === 'memory' && event.axis === 'recall' && spaced) state.memoryAt = event.at;
      if (lastFailure === null || event.at - lastFailure >= DAY) state.needsReview = false;
      const tracked = state.misconceptions.get(event.questionId);
      if (tracked && event.at - tracked.at >= DAY) {
        state.misconceptions.delete(event.questionId);
      }
      if (weakness && event.questionId === weakness.questionId && event.at - weakness.at >= DAY) {
        const remaining = [...state.misconceptions.values()].sort((a, b) => b.at - a.at);
        state.misconception = remaining[0] || null;
      }
      const days = item.kind === 'skill' ? Math.min(state.stability, 30) : state.stability;
      const interval = state.needsReview ? DAY : days * DAY;
      state.due = event.at + interval;
    } else if (!state.due || (sameSession && isIndependent && state.needsReview)) {
      state.due = event.at + DAY;
    }
    state.last = event.at;
    state.exposures[event.axis] = event.at;
    state.byQuestion.set(event.questionId, {event, count: (previous?.count || 0) + 1});
    if (event.sessionId) {
      state.sessions.add(event.sessionId);
      state.sessions.add(event.sessionId + ':' + event.axis);
    }
    return {firstSeen, delayed, qualified, rating};
  }

  function build(model, stored, now = Date.now()) {
    const visible = new Set(model.available().map(item => item.id));
    const states = new Map();
    const history = [];
    for (const input of [...stored].sort((a, b) => a.at - b.at || String(a.id).localeCompare(String(b.id)))) {
      const event = eventFor(model, input);
      if (!event || !visible.has(event.itemId) || event.at > now) continue;
      const state = states.get(event.itemId) || {
        total: 0, correct: 0, lapses: 0, streak: 0, last: null, due: null, difficulty: 5, stability: .5,
        memoryAt: null, needsReview: false, failureAt: null, misconception: null, misconceptions: new Map(),
        lastDelayedSuccess: null, lastWeekSuccess: null, axes: {}, exposures: {}, sessions: new Set(), byQuestion: new Map(),
        retention: [rate(), rate(), rate(), rate()]
      };
      const item = model.items.get(event.itemId);
      history.push({...event, ...update(state, event, item)});
      states.set(event.itemId, state);
    }
    return {states, history};
  }

  function level(item, state) {
    if (!state) return '未学習';
    if (state.needsReview) return '再学習が必要';
    const successful = axis => state.axes[axis]?.correct > 0;
    const learned = item.kind === 'memory' ? successful('recall') :
      (successful('understanding') || successful('discrimination')) &&
      (successful('application') || successful('transfer'));
    if (learned && state.axes.retention?.correct >= 2 && state.lastWeekSuccess !== null &&
        state.lastWeekSuccess >= (state.failureAt || 0)) return '長期定着済み';
    return learned ? '習得済み' : '学習済み';
  }

  function fadingLevel(item, state) {
    const status = level(item, state);
    if (status === '未学習' || status === '再学習が必要' || !state?.correct) return 'worked';
    if (status === '習得済み' || status === '長期定着済み') return 'independent';
    return 'faded';
  }

  function planSession(model, analysis, settings, now = Date.now()) {
    const stats = summary(model, analysis, now);
    const totalSeconds = clamp(Number(settings?.minutes) || 15, 5, 90) * 60;
    const weights = {
      retrieval: 0.15,
      acquisition: 0.25,
      elaboration: 0.15,
      practice: 0.30,
      review: 0.15
    };
    if (stats.due === 0 && stats.relearn === 0) {
      weights.retrieval = 0.05;
      weights.review = 0.10;
      weights.acquisition += 0.10;
      weights.elaboration += 0.05;
    } else if (stats.due >= 10) {
      weights.retrieval += 0.10;
      weights.acquisition = Math.max(0.10, weights.acquisition - 0.10);
    }
    if (stats.fresh === 0) {
      weights.practice += weights.acquisition * 0.5;
      weights.retrieval += weights.acquisition * 0.5;
      weights.acquisition = 0;
    }
    if (settings?.goal === 'recall') {
      weights.retrieval += 0.10;
      weights.review += 0.05;
      weights.practice = Math.max(0.15, weights.practice - 0.10);
      weights.elaboration = Math.max(0.10, weights.elaboration - 0.05);
    } else if (settings?.goal === 'transfer') {
      weights.elaboration += 0.08;
      weights.practice += 0.07;
      weights.retrieval = Math.max(0.08, weights.retrieval - 0.08);
      weights.acquisition = Math.max(0.12, weights.acquisition - 0.07);
    }
    const sum = Object.values(weights).reduce((a, b) => a + b, 0) || 1;
    let cursor = 0;
    return Object.entries(weights)
      .filter(([, weight]) => weight > 0)
      .map(([id, weight]) => {
        const ratio = weight / sum;
        const seconds = Math.round(totalSeconds * ratio);
        const phase = {id, label: PHASES[id], seconds, start: cursor, end: cursor + seconds, ratio};
        cursor += seconds;
        return phase;
      });
  }

  function currentPhase(session) {
    if (!Array.isArray(session?.plan) || !session.plan.length) return null;
    const spent = session.spent || 0;
    return session.plan.find(phase => spent < phase.end) || session.plan[session.plan.length - 1];
  }

  function isConfusable(item, other) {
    if (!item || !other || item.id === other.id) return false;
    return Boolean(item.confusables?.includes(other.id) || other.confusables?.includes(item.id));
  }

  function choose(model, states, session, now) {
    const available = model.available();
    const visible = new Set(available.map(item => item.id));
    const seen = new Set(session.steps.map(step => step.itemId));
    const last = session.steps[session.steps.length - 1];
    const phase = currentPhase(session);
    const overdue = [];
    const fresh = [];
    const weak = [];
    const target = available.some(item => item.topic === session.settings.topic) ? session.settings.topic : '';
    const deadline = session.settings.examDate ? Date.parse(session.settings.examDate + 'T00:00:00') : NaN;
    for (const item of available) {
      if (!item.questions.length || seen.has(item.id) || (target && item.topic !== target)) continue;
      const state = states.get(item.id);
      if (state && state.due <= now) overdue.push(item);
      else if (!state) fresh.push(item);
      else if (state.misconception && now - state.misconception.at >= 20 * 60 * MINUTE) weak.push(item);
    }
    const pool = overdue.length ? overdue : weak.length ? weak : fresh;
    const ranked = new Map();
    for (const original of pool) {
      const missing = item => (item.prerequisites || []).filter(id => visible.has(id) && !seen.has(id) &&
        (!states.has(id) || (states.get(id).due <= now &&
          !['習得済み', '長期定着済み'].includes(level(model.items.get(id), states.get(id))))));
      let item = original;
      const chain = new Set([item.id]);
      while (item !== original || !states.has(original.id)) {
        const dependency = missing(item).find(id => !chain.has(id) && model.items.get(id).questions.length);
        if (!dependency) break;
        chain.add(dependency);
        item = model.items.get(dependency);
      }
      const state = states.get(item.id);
      const risk = probability(state, now);
      const lastItem = last && model.items.get(last.itemId);
      const lastState = lastItem && states.get(lastItem.id);
      const related = Boolean(item.group && item.group === lastItem?.group);
      const confusable = isConfusable(item, lastItem);
      const lastFamiliar = Boolean(lastState?.axes.recall?.correct || lastState?.axes.application?.correct);
      const itemFamiliar = Boolean(state?.axes.recall?.correct || state?.axes.application?.correct);
      let score = (item.importance || 1) * 4 + Math.min(5, model.downstream.get(item.id) || 0) * 2;
      const reasons = [];
      if (original !== item) reasons.push(original.title + 'の前提を確認');
      if (!state) reasons.push('事前知識の確認');
      if (state?.due <= now) {
        score += 40 + clamp((now - state.due) / DAY, 0, 20);
        reasons.push('復習期限');
      }
      if (risk !== null) {
        score += (1 - risk) * 15;
        if (risk < .8) reasons.push('想起確率の推定値が低い');
      }
      if (state?.needsReview) {
        score += 15 + Math.min(10, state.total - state.correct);
        reasons.push('誤答の再学習');
      }
      if (state?.misconception) {
        score += 35;
        reasons.push('確信度の高い誤答（優先修正）');
      }
      if (deadline >= now - DAY && deadline - now < 21 * DAY) {
        score += (item.importance || 1) * (deadline - now < 7 * DAY ? 4 : 2);
        reasons.push('試験日が近い');
      }
      if (session.settings.goal === 'transfer' && item.kind === 'skill') {
        score += 12;
        reasons.push('理解・応用を優先');
      }
      if (session.settings.goal === 'recall' && item.kind === 'memory') {
        score += 12;
        reasons.push('定着を優先');
      }
      if (confusable && lastFamiliar && itemFamiliar) {
        score += 14;
        reasons.push('類似概念を交互に識別');
      } else if (related && lastFamiliar && itemFamiliar) {
        score += 9;
        reasons.push('混同しやすい項目を識別');
      } else if (related && !state && !lastState?.axes.recall?.correct) {
        score += 5;
        reasons.push('同じテーマの基礎を集中的に練習');
      }
      if (phase) {
        if (phase.id === 'elaboration' && state && item.questions.some(q => ['understanding', 'discrimination'].includes(q.axis))) {
          score += 5;
        } else if (phase.id === 'practice' && state && item.questions.some(q => ['application', 'transfer'].includes(q.axis))) {
          score += 5;
        }
      }
      if (!ranked.has(item.id) || ranked.get(item.id).score < score) ranked.set(item.id, {item, score, reasons});
    }
    const candidates = [...ranked.values()].sort((a, b) => b.score - a.score);
    for (const {item, reasons} of candidates) {
      const state = states.get(item.id);
      const lastQuestion = last && model.questions.get(last.questionId)?.question;
      const rank = question => {
        const evidence = state?.byQuestion.get(question.id);
        let score = evidence ? -evidence.count * 2 : 8;
        if (evidence && !evidence.event.correct) score += 20;
        if (state?.misconception?.questionId === question.id) score += 40;
        if (!state) score += question.axis === 'recall' ? 25 :
          ['application', 'recognition'].includes(question.axis) ? 5 : -15;
        if (state && !state.axes.recall?.correct && question.axis === 'recall') score += 12;
        if (state?.axes.recall?.correct && state.due <= now && item.kind === 'memory' && question.axis === 'recall') score += 12;
        if (session.settings.goal === 'transfer' && state && ['understanding', 'transfer', 'discrimination'].includes(question.axis)) score += 8;
        if (phase?.id === 'elaboration' && state && ['understanding', 'discrimination'].includes(question.axis)) score += 6;
        if (phase?.id === 'practice' && state && ['application', 'transfer'].includes(question.axis)) score += 6;
        if (phase?.id === 'retrieval' && state && question.axis === 'recall') score += 5;
        if (state && question.axis === 'recall' && state.exposures.recall != null &&
            now - state.exposures.recall >= Math.min(state.stability, 14) * DAY) score += 7;
        if (question.type !== lastQuestion?.type) score += 2;
        if (question.axis === 'recognition') score -= 10;
        return score;
      };
      const question = [...item.questions].sort((a, b) => rank(b) - rank(a))
        .find(candidate => fits(session, candidate));
      if (question) {
        return {itemId: item.id, questionId: question.id, reasons, retry: false, pretest: !state, phase: phase?.id || null};
      }
    }
    return null;
  }

  const estimatedSeconds = question => question.seconds || (question.rubric?.length ? 105 : 45);
  const fits = (session, question) => !session.steps.length ||
    session.spent + estimatedSeconds(question) <= session.settings.minutes * 60;

  function nextStep(model, analysis, session, now) {
    if (session.finished) return null;
    if (!Array.isArray(session.plan)) {
      session.plan = planSession(model, analysis, session.settings, now);
    }
    const active = new Set(model.available().map(item => item.id));
    const last = session.steps[session.steps.length - 1];
    const retryStep = force => {
      const retry = session.retries.find(entry => active.has(entry.itemId) && model.questions.has(entry.questionId) &&
        (force || entry.after <= session.steps.length) &&
        fits(session, model.questions.get(entry.questionId).question));
      if (!retry) return null;
      session.retries = session.retries.filter(entry => entry !== retry);
      return {...retry, reasons: ['訂正後の再検索'], retry: true, phase: 'review'};
    };
    const followUp = () => {
      if (!last?.pretest || last.retry) return null;
      const item = model.items.get(last.itemId);
      if (!active.has(item.id)) return null;
      const base = model.questions.get(last.questionId).question;
      const event = analysis.history.find(entry => entry.id === last.id);
      if (!event?.correct || event.hints) return null;
      const axes = item.kind === 'skill' ? ['understanding', 'discrimination', 'application'] : ['application'];
      const question = item.questions.find(candidate => candidate.axis !== base.axis &&
        axes.includes(candidate.axis) && fits(session, candidate));
      return question && {
        itemId: item.id, questionId: question.id,
        reasons: ['事前問題を通過した項目を別の観点から確認'], retry: false, phase: 'elaboration'
      };
    };
    const primary = retryStep(false) || followUp() ||
      choose(model, analysis.states, session, now) || retryStep(true);
    if (!primary) return null;
    const step = {...primary, id: session.id + ':' + session.steps.length, openedAt: now};
    session.steps.push(step);
    return step;
  }

  function completeStep(model, session, step, event) {
    if (session.steps[session.steps.length - 1]?.id !== step.id || session.completed === step.id) return;
    const question = model.questions.get(step.questionId)?.question;
    session.completed = step.id;
    if (!question) return;
    session.spent += Math.max(estimatedSeconds(question), Math.min(600, event?.seconds || 0));
    if ((!event || !event.correct || event.hints) && !step.retry) {
      session.retries.push({itemId: step.itemId, questionId: step.questionId, after: session.steps.length + 2});
    }
  }

  const normalise = value => String(value).normalize('NFKC').trim().replace(/\s+/g, ' ').toLocaleLowerCase();
  function grade(question, response) {
    if (question.check) return question.check(response);
    if (question.rubric?.length) return null;
    if (question.type === 'choice' || question.type === 'boolean') return response === question.answer;
    if (question.type === 'multiple') {
      return response.length === question.answer.length && question.answer.every(index => response.includes(index));
    }
    if (question.type === 'numeric') {
      const text = normalise(response).replace(/−/g, '-');
      if (!/^[+-]?(?:\d+(?:\.\d*)?|\.\d+)(?:e[+-]?\d+)?$/i.test(text)) return false;
      return Math.abs(Number(text) - Number(question.answer)) <= (question.tolerance ?? 1e-8);
    }
    return [question.answer].flat().some(answer => normalise(response) === normalise(answer));
  }

  function calibration(events) {
    const rated = events.filter(event => event.confidence != null);
    const high = rated.filter(event => event.confidence === 3);
    const low = rated.filter(event => event.confidence === 1);
    const overconfident = high.filter(event => !event.correct).length;
    const underconfident = low.filter(event => event.correct).length;
    const expected = {1: 0.33, 2: 0.67, 3: 0.95};
    const brier = rated.length
      ? rated.reduce((sum, event) => sum + Math.pow(expected[event.confidence] - Number(event.correct), 2), 0) / rated.length
      : null;
    return {
      rated: rated.length,
      highTotal: high.length,
      overconfident,
      lowTotal: low.length,
      underconfident,
      brier
    };
  }

  function readiness(model, analysis, now = Date.now()) {
    const items = model.available();
    const {states, history} = analysis;
    const native = history.filter(event => event.method !== 'legacy');
    const learned = items.filter(item => ['習得済み', '長期定着済み'].includes(level(item, states.get(item.id)))).length;
    const coverageRatio = items.length ? learned / items.length : 0;
    const initial = native.filter(event => event.firstSeen && !event.hints && event.generated);
    const initialCorrect = initial.filter(event => event.correct).length;
    const initialRatio = initial.length ? initialCorrect / initial.length : null;
    let delayedTotal = 0;
    let delayedCorrect = 0;
    for (const state of states.values()) {
      delayedTotal += (state.retention[2]?.total || 0) + (state.retention[3]?.total || 0);
      delayedCorrect += (state.retention[2]?.correct || 0) + (state.retention[3]?.correct || 0);
    }
    const delayedRatio = delayedTotal ? delayedCorrect / delayedTotal : null;
    let misconceptionCount = 0;
    for (const item of items) misconceptionCount += states.get(item.id)?.misconceptions?.size || 0;
    const priorities = items
      .filter(item => !['習得済み', '長期定着済み'].includes(level(item, states.get(item.id))))
      .map(item => {
        const state = states.get(item.id);
        let urgency = (item.importance || 1) * 3 + Math.min(5, model.downstream.get(item.id) || 0) * 2;
        if (state?.misconception) urgency += 40;
        if (state?.needsReview) urgency += 25;
        if (state?.due <= now) urgency += 15;
        return {itemId: item.id, topic: item.topic, title: item.mapTitle || item.title, status: level(item, state), urgency};
      })
      .sort((a, b) => b.urgency - a.urgency)
      .slice(0, 3);
    const checks = [
      {id: 'coverage', label: '範囲網羅（習得・長期定着）', met: coverageRatio >= 0.8 && items.length > 0, target: '80%以上',
        value: learned + '/' + items.length + '（' + Math.round(coverageRatio * 100) + '%）'},
      {id: 'firstSeen', label: '初見・自力正答率', met: initial.length >= 10 && initialRatio >= 0.65, target: '65%以上（10問以上）',
        value: initial.length ? initialCorrect + '/' + initial.length + '（' + Math.round(initialRatio * 100) + '%）' : '未測定'},
      {id: 'delayed', label: '7日以上の長期再現率', met: delayedTotal >= 5 && delayedRatio >= 0.7, target: '70%以上（5問以上）',
        value: delayedTotal ? delayedCorrect + '/' + delayedTotal + '（' + Math.round(delayedRatio * 100) + '%）' : '未測定'},
      {id: 'misconceptions', label: '未解決の誤概念（高確信誤答）', met: misconceptionCount === 0 && native.length > 0, target: '0件',
        value: misconceptionCount + '件'}
    ];
    return {checks, priorities, metCount: checks.filter(c => c.met).length};
  }

  function summary(model, analysis, now) {
    const {states, history} = analysis;
    const today = new Date(now);
    today.setHours(0, 0, 0, 0);
    const daily = history.filter(event => event.at >= +today && event.method !== 'legacy');
    const native = history.filter(event => event.method !== 'legacy');
    const items = model.available();
    const learned = items.filter(item => ['習得済み', '長期定着済み'].includes(level(item, states.get(item.id)))).length;
    const memory = items.filter(item => item.kind === 'memory').map(item => probability(states.get(item.id), now))
      .filter(value => value != null);
    const misconceptions = [];
    for (const item of items) {
      const state = states.get(item.id);
      if (!state?.misconceptions?.size) continue;
      for (const entry of state.misconceptions.values()) {
        misconceptions.push({itemId: item.id, title: item.mapTitle || item.title, ...entry});
      }
    }
    misconceptions.sort((a, b) => b.at - a.at);
    return {
      daily, native, learned, memory, misconceptions,
      calibration: calibration(native),
      due: items.filter(item => states.get(item.id)?.due <= now).length,
      fresh: items.length - states.size,
      relearn: [...states.values()].filter(state => state.needsReview).length
    };
  }

  const api = {
    DAY, MINUTE, TARGET_RETENTION, AXES, CAUSES, CAUSE_GUIDANCE, ELABORATION_LENSES, PHASES, GOALS,
    clamp, probability, compile, makeStore, record, build, level, fadingLevel, planSession, currentPhase,
    calibration, readiness, nextStep, completeStep, grade, summary
  };
  if (typeof module === 'object' && module.exports) module.exports = api;
  else global.AdaptiveEngine = api;
})(typeof window === 'undefined' ? globalThis : window);
