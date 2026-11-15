(() => {
  'use strict';
  if (window.__fieldnotesSiteLoaded) return;
  window.__fieldnotesSiteLoaded = true;
  const script = document.currentScript;
  const base = new URL('../', script.src);
  if (!document.querySelector('link[rel~="icon"]')) {
    const icon = document.createElement('link');
    icon.rel = 'icon';
    icon.type = 'image/svg+xml';
    icon.href = new URL('site-assets/favicon.svg', base).href;
    document.head.append(icon);
  }
  const pages = [
    ['中国語', 'learning/chinese.html'],
    ['統計学', 'learning/statistics-pre1-interactive.html'],
    ['夜間光', 'shumi.html'],
    ['テキスト分析', 'topicmodel.html'],
    ['受験生マップ', 'childexp.html']
  ];
  let initialized = false;
  function initialize() {
    if (initialized) return;
    initialized = true;
    const path = window.location.pathname;
    const isStat = path.endsWith('/learning/statistics-pre1-interactive.html');
    const isChinese = path.endsWith('/learning/chinese.html');
    const isHome = path.endsWith('/') || path.endsWith('/index.html');
    document.body.classList.add(isStat ? 'site-stat' : isChinese ? 'site-chinese' : 'site-legacy');
    if (isHome) document.body.classList.add('field-home');
    let header = document.getElementById('site-header');
    if (!header) {
      header = document.createElement('header');
      header.id = 'site-header';
      header.className = 'site-header';
      const inner = document.createElement('div');
      inner.className = 'site-header-inner';
      const logo = document.createElement('a');
      logo.className = 'site-logo';
      logo.href = new URL('index.html', base).href;
      logo.setAttribute('aria-label', 'FIELD NOTES ホーム');
      logo.textContent = 'FIELD NOTES';
      const nav = document.createElement('nav');
      nav.className = 'site-nav';
      nav.setAttribute('aria-label', 'メインナビゲーション');
      for (const [label, href] of pages) {
        const anchor = document.createElement('a');
        anchor.href = new URL(href, base).href;
        anchor.textContent = label;
        if (path.endsWith('/' + href)) anchor.setAttribute('aria-current', 'page');
        nav.append(anchor);
      }
      inner.append(logo, nav);
      header.append(inner);
      document.body.prepend(header);
    }
    header.querySelectorAll('.site-nav a').forEach(anchor => {
      if (new URL(anchor.href).pathname === path) anchor.setAttribute('aria-current', 'page');
    });
    document.body.classList.add('site-ready');
    if (isStat) {
      const updateHeight = () => document.documentElement.style.setProperty('--site-header-height', `${Math.max(0, header.getBoundingClientRect().bottom)}px`);
      updateHeight();
      window.addEventListener('resize', updateHeight);
      window.addEventListener('scroll', updateHeight, {passive: true});
      if ('ResizeObserver' in window) new ResizeObserver(updateHeight).observe(header);
    }
    const filters = document.getElementById('home-filters');
    if (filters) {
      filters.hidden = false;
      const buttons = filters.querySelectorAll('button[data-filter]');
      const sections = document.querySelectorAll('[data-home-category]');
      buttons.forEach(button => button.addEventListener('click', () => {
        buttons.forEach(item => item.setAttribute('aria-pressed', String(item === button)));
        sections.forEach(section => { section.hidden = button.dataset.filter !== 'all' && section.dataset.homeCategory !== button.dataset.filter; });
      }));
    }
    const pick = document.getElementById('surprise-link');
    if (pick) pick.addEventListener('click', () => {
      pick.href = new URL(pages[Math.floor(Math.random() * pages.length)][1], base).href;
    });
  }
  if (document.readyState === 'loading') {
    document.addEventListener('DOMContentLoaded', initialize, {once: true});
    // A readiness fallback also covers old report scripts that suppress later listeners.
    const checkReady = () => {
      if (document.readyState !== 'loading') initialize();
      else setTimeout(checkReady, 25);
    };
    setTimeout(checkReady, 25);
  } else initialize();
})();
