'use strict';
const test = require('node:test');
const assert = require('node:assert/strict');
const E = require('../learning/adaptive-engine.js');
const at = Date.parse('2026-11-27T23:57:00+09:00');
const memory = {id: 'word', topic: 'basics', kind: 'memory', questions: [
  {id: 'production', axis: 'recall', type: 'short', answer: '字'},
  {id: 'reading', axis: 'recall', type: 'short', answer: 'zì'},
  {id: 'recognition', axis: 'recognition', type: 'choice', answer: 0}
]};
const skill = {id: 'skill', topic: 'skill', kind: 'skill', prerequisites: ['word'], questions: [
  {id: 'definition', axis: 'recall', type: 'short'},
  {id: 'explain', axis: 'understanding', type: 'explain', rubric: ['criterion']},
  {id: 'apply', axis: 'application', type: 'numeric'},
  {id: 'transfer', axis: 'transfer', type: 'reason', rubric: ['criterion']}
]};
const model = E.compile({id: 'demo', items: [memory, skill]});
function attempt(id, elapsed, fields = {}) {
  return {id, itemId: 'word', questionId: 'production', at: at + elapsed, correct: true, sessionId: id, ...fields};
}
function session(fields = {}) {
  return {id: 'current', steps: [], retries: [], spent: 0, settings: {
    minutes: 5, topic: '', goal: 'balanced', ...fields
  }};
}
function storage() {
  const values = new Map();
  return {
    get length() { return values.size; },
    key: index => [...values.keys()][index] ?? null,
    getItem: key => values.get(key) ?? null,
    setItem: (key, value) => values.set(key, value),
    removeItem: key => values.delete(key)
  };
}

test('incorrect high-confidence attempt persists and schedules retrieval after ten minutes', () => {
  const backing = storage();
  const writer = E.makeStore(backing, 'demo');
  const evidence = attempt('a', 0, {correct: false, confidence: 3, cause: E.CAUSES[3]});
  E.record(model, writer, evidence);
  E.record(model, writer, {...evidence, correct: true});
  const learned = E.build(model, E.makeStore(backing, 'demo').events(), at);
  const state = learned.states.get('word');
  assert.equal(state.due, at + 10 * E.MINUTE);
  assert.equal(state.misconception.questionId, 'production');
  assert.equal(E.level(memory, state), '再学習が必要');
  assert.equal(learned.history[0].cause, E.CAUSES[3]);
  assert.equal(E.summary(model, learned, state.due - 1).due, 0);
  assert.equal(E.summary(model, learned, state.due).due, 1);
});

test('successive misses narrow spacing and same-session correction does not establish retention', () => {
  const answer = attempt('tenth', 15 * E.MINUTE, {correct: false, sessionId: 'sid'});
  const events = [
    attempt('first', 0, {correct: false, sessionId: 'sid'}), answer,
    attempt('corrected', 16 * E.MINUTE, {sessionId: 'sid'})
  ];
  const state = E.build(model, events, at + E.DAY).states.get('word');
  assert.equal(state.total, 3);
  assert.equal(state.correct, 1);
  assert.equal(state.stability, .25);
  assert.equal(state.due, at + 16 * E.MINUTE + E.DAY);
  assert.equal(state.axes.retention, undefined);
  assert.equal(state.axes.recall.correct, 0);
  assert.equal(E.level(memory, state), '再学習が必要');
});

test('unassisted generation at spaced intervals changes stability and can establish retention', () => {
  const events = [attempt('a', 0), attempt('b', E.DAY, {questionId: 'reading'}),
    attempt('c', 9 * E.DAY, {questionId: 'production'})];
  const first = E.build(model, events, at).states.get('word');
  assert.equal(E.level(memory, first), '習得済み');
  const final = E.build(model, events, at + 9 * E.DAY).states.get('word');
  assert.equal(E.level(memory, final), '長期定着済み');
  assert.equal(final.axes.retention.correct, 2);
  assert.ok(final.stability > first.stability);
  assert.equal(E.probability(final, final.memoryAt), 1);
  assert.ok(Math.abs(E.probability(final, final.due) - .85) < 1e-9);
});

test('recognition, hint use, and legacy application do not imply unmeasured mastery', () => {
  const events = [
    attempt('a', 0, {questionId: 'recognition'}),
    attempt('b', E.DAY, {hints: 1}),
    attempt('c', E.DAY, {itemId: 'skill', questionId: 'apply', method: 'legacy'})
  ];
  const states = E.build(model, events, at + E.DAY).states;
  assert.equal(states.get('word').axes.recognition.correct, 1);
  assert.equal(states.get('word').axes.recall.total, 0);
  assert.equal(E.level(memory, states.get('word')), '学習済み');
  assert.equal(E.level(skill, states.get('skill')), '学習済み');
  assert.equal(E.probability(states.get('word'), at + E.DAY), null);
});

test('unsubmitted and explicitly unable differ, and the scheduler finds prerequisites and retries', () => {
  const active = session({topic: 'skill'});
  const first = E.nextStep(model, E.build(model, [], at), active, at);
  assert.equal(first.itemId, 'word');
  assert.match(first.reasons[0], /前提/);
  const evidence = attempt(first.id, 1, {correct: false, method: 'unable', sessionId: active.id});
  E.completeStep(model, active, first, evidence);
  assert.equal(active.retries[0].questionId, first.questionId);
  const second = E.nextStep(model, E.build(model, [evidence], at + 1), active, at + 1);
  assert.equal(second.itemId, 'skill');
  assert.equal(E.build(model, [evidence], at + 1).states.has('skill'), false);
  E.completeStep(model, active, second, null);
  const third = E.nextStep(model, E.build(model, [evidence], at + 2), active, at + 2);
  assert.equal(third.retry, true);
  assert.equal(third.itemId, 'word');
});

test('exact elapsed-time buckets remain distinct from local calendar reporting', () => {
  const start = new Date(at);
  start.setHours(0, 0, 0, 0);
  const next = new Date(start);
  next.setDate(next.getDate() + 1);
  const recorded = [attempt('a', +next - at - 1), attempt('b', +next - at)];
  const analysis = E.build(model, recorded, +next);
  assert.deepEqual(E.summary(model, analysis, +next).daily.map(event => event.id), ['b']);
  assert.equal(analysis.states.get('word').axes.retention, undefined);
});

test('grading formats handle numeric bounds, option equality, and manual scores', () => {
  assert.equal(E.grade({type: 'numeric', answer: -.25, tolerance: .001}, '−０.２５０'), true);
  assert.equal(E.grade({type: 'numeric', answer: 0}, ''), false);
  assert.equal(E.grade({type: 'numeric', answer: 2}, '0x02'), false);
  assert.equal(E.grade({type: 'multiple', answer: [2, 0]}, [0, 2]), true);
  assert.equal(E.grade({type: 'multiple', answer: [2, 0]}, [0, 0]), false);
  assert.equal(E.grade({type: 'boolean', answer: false}, false), true);
  assert.equal(E.grade(memory.questions[1], 'zi'), false);
  assert.equal(E.grade(skill.questions[1], 'answer'), null);
});

test('scope and storage failure preserve evidence without polluting current analysis', () => {
  let scope = 'future';
  const scoped = E.compile({id: 'scope', items: [{...memory, scope: 'future'}], getScope: () => scope});
  const st = E.makeStore(null, 'scope');
  E.record(scoped, st, attempt('a', 0));
  assert.equal(E.record(scoped, st, attempt('b', 0, {itemId: 'invalid'})), null);
  assert.equal(st.available(), false);
  assert.equal(E.build(scoped, st.events(), at).states.size, 1);
  scope = 'current';
  assert.equal(E.build(scoped, st.events(), at).states.size, 0);
  assert.equal(st.events().length, 1);
});

test('FSRS power-law retrievability has heavier tail than exponential and scales with spacing', () => {
  const events = [attempt('a', 0, {confidence: 3, seconds: 10}), attempt('b', 3 * E.DAY, {questionId: 'reading', confidence: 2})];
  const state = E.build(model, events, at + 3 * E.DAY).states.get('word');
  const atThreeS = E.probability(state, state.memoryAt + 3 * state.stability * E.DAY);
  assert.ok(atThreeS > Math.pow(0.85, 3));
  assert.ok(Math.abs(E.probability(state, state.due) - 0.85) < 1e-9);
});

test('planSession dynamically adapts phase ratios to due count and learning goal', () => {
  const empty = E.build(model, [], at);
  const transferPlan = E.planSession(model, empty, {minutes: 60, goal: 'transfer'}, at);
  const recallPlan = E.planSession(model, empty, {minutes: 60, goal: 'recall'}, at);
  const byId = plan => Object.fromEntries(plan.map(p => [p.id, p.seconds]));
  assert.ok(byId(transferPlan).elaboration > byId(recallPlan).elaboration);
  assert.ok(byId(recallPlan).retrieval > byId(transferPlan).retrieval);
});

test('fadingLevel and confusable interleaving adapt to learner mastery', () => {
  const a = {id: 'a', topic: 'g', kind: 'skill', group: 'g', confusables: ['b'], questions: [
    {id: 'a-q', axis: 'recall', type: 'short', answer: 'a'}
  ]};
  const b = {id: 'b', topic: 'g', kind: 'skill', group: 'g', confusables: ['a'], questions: [
    {id: 'b-q', axis: 'recall', type: 'short', answer: 'b'}
  ]};
  const c = {id: 'c', topic: 'g', kind: 'skill', group: 'g', questions: [
    {id: 'c-q', axis: 'recall', type: 'short', answer: 'c'}
  ]};
  const m = E.compile({id: 'interleave', items: [a, b, c]});
  assert.equal(E.fadingLevel(a, undefined), 'worked');
  const events = [
    {id: 'e1', itemId: 'a', questionId: 'a-q', at, correct: true, sessionId: 's1'},
    {id: 'e2', itemId: 'b', questionId: 'b-q', at, correct: true, sessionId: 's1'},
    {id: 'e3', itemId: 'c', questionId: 'c-q', at, correct: true, sessionId: 's1'}
  ];
  const analysis = E.build(m, events, at + 5 * E.DAY);
  assert.equal(E.fadingLevel(a, analysis.states.get('a')), 'faded');
  const active = session();
  active.steps.push({id: 's0', itemId: 'a', questionId: 'a-q', reasons: []});
  const next = E.nextStep(m, analysis, active, at + 5 * E.DAY);
  assert.equal(next.itemId, 'b');
  assert.ok(next.reasons.includes('類似概念を交互に識別'));
});

test('calibration and misconceptions summarize metacognitive accuracy', () => {
  const events = [
    attempt('e1', 0, {correct: false, confidence: 3, cause: E.CAUSES[2]}),
    attempt('e2', 2 * E.DAY, {correct: true, confidence: 1, questionId: 'reading'})
  ];
  const analysis = E.build(model, events, at + 2 * E.DAY);
  const stats = E.summary(model, analysis, at + 2 * E.DAY);
  assert.equal(stats.misconceptions.length, 1);
  assert.equal(stats.misconceptions[0].cause, E.CAUSES[2]);
  assert.equal(stats.calibration.overconfident, 1);
  assert.equal(stats.calibration.underconfident, 1);
  assert.ok(stats.calibration.brier > 0);
});

test('readiness evaluates four evidence-based criteria and ranks priority items', () => {
  const empty = E.build(model, [], at);
  const r0 = E.readiness(model, empty, at);
  assert.equal(r0.checks.length, 4);
  assert.equal(r0.metCount, 0);
  assert.ok(r0.priorities.length > 0);
});

test('statistics calculation drills generate valid parameterized problems across all 12 archetypes', () => {
  const Drills = require('../learning/statistics-drills.js');
  assert.equal(Drills.GENERATORS.length, 12);
  const generated = Drills.generate(12, 20261128);
  assert.equal(generated.length, 12);
  for (const item of generated) {
    assert.ok(item.prompt.length > 10);
    assert.ok(Number.isFinite(item.answer));
    assert.ok(Number.isFinite(item.tolerance) && item.tolerance >= 0);
    assert.ok(item.explanation.length > 5);
  }
});
