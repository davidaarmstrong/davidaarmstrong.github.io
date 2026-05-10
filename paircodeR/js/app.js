/**
 * app.js  —  paircodeR main controller
 *
 * Wires together: WebR execution, the AI tutor, CodeMirror editor,
 * curriculum loading, task navigation, and all UI interactions.
 */

import { init as initWebR, run as runCode, check as checkCode, isReady } from './webr-runner.js';
import { sendMessage, congratulate } from './tutor.js';
import { openBuilder, initBuilder } from './builder.js';

// CodeMirror 5 is loaded via <script> tags in index.html (global `CodeMirror`)

// ── State ─────────────────────────────────────────────────────────────────────
let module        = null;   // loaded curriculum module
let taskIndex     = 0;      // current task index
let editor        = null;   // CodeMirror EditorView instance
let chatHistory   = [];     // conversation history (provider-specific format)
let lastOutput    = '';     // plain-text of last console run (for AI context)
let isSending     = false;  // guard against double-sends
let hintCount      = 0;      // hint/stuck requests for current task
let checkAttempts  = 0;      // check attempts for current task
let completedTasks = {};     // taskIndex → { title, code, completedAt }

// ── Persistent settings (localStorage) ───────────────────────────────────────
const PROVIDER_MODELS = {
  claude: [
    { value: 'claude-sonnet-4-6',   label: 'Claude Sonnet 4.6 (default)' },
    { value: 'claude-opus-4-5',     label: 'Claude Opus 4.5' },
    { value: 'claude-haiku-4-5',    label: 'Claude Haiku 4.5 (fast)' },
  ],
  openai: [
    { value: 'gpt-4o-mini',  label: 'GPT-4o mini (default)' },
    { value: 'gpt-4o',       label: 'GPT-4o' },
  ],
  gemini: [
    { value: 'gemini-1.5-flash',  label: 'Gemini 1.5 Flash (default)' },
    { value: 'gemini-1.5-pro',    label: 'Gemini 1.5 Pro' },
    { value: 'gemini-2.0-flash',  label: 'Gemini 2.0 Flash' },
  ],
  github: [
    { value: 'gpt-4o-mini',  label: 'GPT-4o mini (default)' },
    { value: 'gpt-4o',       label: 'GPT-4o' },
    { value: 'Meta-Llama-3.1-70B-Instruct',  label: 'Llama 3.1 70B' },
    { value: 'Mistral-large-2407',           label: 'Mistral Large' },
  ],
};

function loadSettings() {
  return {
    provider: localStorage.getItem('pcr_provider') ?? 'claude',
    model:    localStorage.getItem('pcr_model')    ?? '',
    apiKey:   localStorage.getItem('pcr_apikey')   ?? '',
  };
}
function saveSettings(provider, model, apiKey) {
  localStorage.setItem('pcr_provider', provider);
  localStorage.setItem('pcr_model',    model);
  localStorage.setItem('pcr_apikey',   apiKey);
}

function populateModelSelect(provider, currentModel) {
  const sel = el['ai-model'];
  const models = PROVIDER_MODELS[provider] ?? [];
  sel.innerHTML = models.map(m =>
    `<option value="${m.value}">${m.label}</option>`
  ).join('');
  // Select saved model if it exists in the list, otherwise first option
  if (currentModel && models.find(m => m.value === currentModel)) {
    sel.value = currentModel;
  }
}

// ── DOM helpers ───────────────────────────────────────────────────────────────
const $  = id => document.getElementById(id);
const el = {};

function cacheElements() {
  [
    'module-select', 'webr-status', 'settings-btn', 'settings-modal',
    'api-key-input', 'ai-provider', 'ai-model', 'settings-save', 'settings-cancel',
    'task-progress', 'task-title', 'task-instructions', 'code-editor',
    'run-btn', 'reset-btn', 'check-btn',
    'console-output', 'plot-output', 'plot-canvas', 'no-plot-msg',
    'output-tabs',
    'prev-btn', 'next-btn', 'task-status-icon', 'download-btn',
    'chat-messages', 'chat-input', 'send-btn', 'clear-chat-btn',
    'loading-overlay', 'loading-message',
  ].forEach(id => { el[id] = $(id); });
}

// ── Markdown renderer (minimal, no dependency) ────────────────────────────────
function renderMarkdown(text) {
  return text
    // fenced code blocks
    .replace(/```(\w*)\n([\s\S]*?)```/g, (_, lang, code) =>
      `<pre><code>${escHtml(code.trimEnd())}</code></pre>`)
    // inline code
    .replace(/`([^`]+)`/g, (_, c) => `<code>${escHtml(c)}</code>`)
    // bold
    .replace(/\*\*(.+?)\*\*/g, '<strong>$1</strong>')
    // italic
    .replace(/\*(.+?)\*/g, '<em>$1</em>')
    // unordered list items
    .replace(/^[-*] (.+)$/gm, '<li>$1</li>')
    .replace(/(<li>[\s\S]*?<\/li>)/g, '<ul>$1</ul>')
    // numbered list items
    .replace(/^\d+\. (.+)$/gm, '<li>$1</li>')
    // blank-line-separated paragraphs
    .split(/\n{2,}/)
    .map(p => p.trim().startsWith('<') ? p : `<p>${p.replace(/\n/g, ' ')}</p>`)
    .join('\n');
}

function escHtml(s) {
  return s.replace(/&/g,'&amp;').replace(/</g,'&lt;').replace(/>/g,'&gt;');
}

// ── WebR status badge ─────────────────────────────────────────────────────────
function setStatus(state, label) {
  el['webr-status'].className = `status-badge status-${state}`;
  el['webr-status'].textContent = label;
}

// ── Loading overlay ───────────────────────────────────────────────────────────
function setLoading(visible, msg = '') {
  el['loading-overlay'].style.display = visible ? 'flex' : 'none';
  if (msg) el['loading-message'].textContent = msg;
}

function setProgress(fraction, detail = '') {
  const pct = Math.round(fraction * 100);
  document.getElementById('progress-fill').style.width = `${pct}%`;
  if (detail) document.getElementById('loading-sub-message').textContent = detail;
}

// ── Curriculum / module ───────────────────────────────────────────────────────

function getSavedModules() {
  try { return JSON.parse(localStorage.getItem('pcr_custom_modules') ?? '[]'); }
  catch { return []; }
}

function saveCustomModule(title, url) {
  const saved = getSavedModules();
  if (!saved.find(m => m.url === url)) {
    saved.push({ title, url });
    localStorage.setItem('pcr_custom_modules', JSON.stringify(saved));
  }
}

function rebuildModuleSelect(manifest, currentUrl) {
  el['module-select'].innerHTML = '';

  manifest.modules.forEach(m => {
    const opt = new Option(m.title, m.file);
    el['module-select'].appendChild(opt);
  });

  getSavedModules().forEach(m => {
    const opt = new Option(`↗ ${m.title}`, m.url);
    el['module-select'].appendChild(opt);
  });

  el['module-select'].appendChild(new Option('— Import from URL… —', '__import__'));

  if (currentUrl) el['module-select'].value = currentUrl;
  el['module-select'].disabled = false;
}

async function loadModule(url) {
  const res = await fetch(url);
  if (!res.ok) throw new Error(`Could not load module: ${url}`);
  module         = await res.json();
  taskIndex      = 0;
  completedTasks = {};
  if (el['download-btn']) el['download-btn'].classList.add('hidden');
}

// ── Import-from-URL modal ─────────────────────────────────────────────────────
let _cachedManifest = { modules: [] };

let _importFileData = null;   // parsed JSON from local file pick

function showImportModal() {
  $('import-url-input').value = '';
  $('import-error').textContent = '';
  $('import-error').classList.add('hidden');
  $('import-file-name').textContent = 'No file chosen';
  $('import-file-name').classList.remove('chosen');
  _importFileData = null;
  $('import-modal').classList.remove('hidden');
}

function hideImportModal() {
  $('import-modal').classList.add('hidden');
  _importFileData = null;
}

async function handleImportLoad() {
  const errEl = $('import-error');
  errEl.classList.add('hidden');

  $('import-load-btn').disabled    = true;
  $('import-load-btn').textContent = 'Loading…';

  try {
    let data;

    if (_importFileData) {
      // File was already read by the picker
      data = _importFileData;
    } else {
      const url = $('import-url-input').value.trim();
      if (!url) throw new Error('Choose a file or enter a URL.');
      const res = await fetch(url);
      if (!res.ok) throw new Error(`HTTP ${res.status}`);
      data = await res.json();
      saveCustomModule(data.title ?? 'Imported module', url);
    }

    if (!data.tasks || !Array.isArray(data.tasks)) throw new Error('Not a valid paircodeR curriculum file.');

    module    = data;
    taskIndex = 0;

    // Add to selector using a blob URL so file-loaded modules appear in the list
    const blob    = new Blob([JSON.stringify(data)], { type: 'application/json' });
    const blobUrl = URL.createObjectURL(blob);
    const opt     = new Option(`↗ ${data.title ?? 'Local module'}`, blobUrl);
    // Insert before the __import__ sentinel
    const sel = $('module-select');
    sel.insertBefore(opt, sel.querySelector('option[value="__import__"]'));
    sel.value = blobUrl;

    hideImportModal();
    renderTask();
  } catch (e) {
    errEl.textContent = `Failed to load: ${e.message}`;
    errEl.classList.remove('hidden');
  } finally {
    $('import-load-btn').disabled    = false;
    $('import-load-btn').textContent = 'Load';
  }
}

// ── Task rendering ────────────────────────────────────────────────────────────
function renderTask() {
  if (!module) return;
  const task = module.tasks[taskIndex];

  el['task-progress'].textContent =
    `${module.title}  ·  Task ${taskIndex + 1} of ${module.tasks.length}`;
  el['task-title'].textContent = task.title;
  el['task-instructions'].innerHTML = renderMarkdown(task.instructions);

  // Reset editor to starter code
  setEditorCode(task.starter_code ?? '');

  // Clear output
  clearConsole();
  clearPlot();

  // Nav buttons
  el['prev-btn'].disabled = taskIndex === 0;
  el['next-btn'].disabled = taskIndex === module.tasks.length - 1;
  el['task-status-icon'].textContent = '';

  // Reset chat history and performance counters when task changes
  chatHistory   = [];
  hintCount     = 0;
  checkAttempts = 0;
  el['chat-messages'].innerHTML = '';
  addChatBubble('assistant',
    `Let's work on **${task.title}**. Read the instructions, give it a try, and I'm here if you need a nudge. 🙂`);
}

// ── CodeMirror 5 editor ───────────────────────────────────────────────────────
function createEditor(initialCode) {
  editor = window.CodeMirror(el['code-editor'], {
    value:        initialCode,
    mode:         'r',
    theme:        'dracula',
    lineNumbers:  true,
    indentWithTabs: false,
    tabSize:      2,
    lineWrapping: true,
    autofocus:    false,
    extraKeys: { Tab: cm => cm.execCommand('indentMore') },
  });
}

function setEditorCode(code) {
  if (!editor) { createEditor(code); return; }
  editor.setValue(code);
}

function getEditorCode() {
  return editor ? editor.getValue() : '';
}

// ── Console output ────────────────────────────────────────────────────────────
function clearConsole() {
  el['console-output'].innerHTML = '';
  lastOutput = '';
}

function appendConsole(lines) {
  const frag = document.createDocumentFragment();
  lines.forEach(({ kind, text }) => {
    const div = document.createElement('div');
    div.className = `${kind}-line`;
    div.textContent = text;
    frag.appendChild(div);
    lastOutput += text + '\n';
  });
  el['console-output'].appendChild(frag);
  el['console-output'].scrollTop = el['console-output'].scrollHeight;
}

// ── Plot output ───────────────────────────────────────────────────────────────
function clearPlot() {
  const ctx = el['plot-canvas'].getContext('2d');
  ctx.clearRect(0, 0, el['plot-canvas'].width, el['plot-canvas'].height);
  el['plot-canvas'].classList.remove('has-plot');
  el['no-plot-msg'].style.display = '';
}

function showPlotTab() {
  el['output-tabs'].querySelectorAll('.tab-btn').forEach(b => b.classList.remove('active'));
  el['output-tabs'].querySelector('[data-tab="plot"]').classList.add('active');
  el['console-output'].classList.add('hidden');
  el['plot-output'].classList.remove('hidden');
}

function showConsoleTab() {
  el['output-tabs'].querySelectorAll('.tab-btn').forEach(b => b.classList.remove('active'));
  el['output-tabs'].querySelector('[data-tab="console"]').classList.add('active');
  el['plot-output'].classList.add('hidden');
  el['console-output'].classList.remove('hidden');
}

// ── Run handler ───────────────────────────────────────────────────────────────
async function handleRun() {
  if (!isReady()) return;

  const code = getEditorCode();
  if (!code.trim()) return;

  setStatus('running', '⚡ Running…');
  el['run-btn'].disabled = true;
  clearConsole();

  const { ok, output, hasPlot } = await runCode(code, el['plot-canvas']);

  appendConsole(output);
  setStatus('ready', '✅ R Ready');
  el['run-btn'].disabled = false;

  if (hasPlot) {
    el['no-plot-msg'].style.display = 'none';
    showPlotTab();
  } else {
    showConsoleTab();
  }
}

// ── Check handler ─────────────────────────────────────────────────────────────
async function handleCheck() {
  if (!isReady() || !module) return;
  const task = module.tasks[taskIndex];
  if (!task.test_expression) {
    appendConsole([{ kind: 'msg', text: '(No automated check for this task — ask your pair partner to review!)' }]);
    return;
  }

  checkAttempts++;

  // First run the user's code, then evaluate the test
  const code = getEditorCode();
  await runCode(code, el['plot-canvas']);

  const passed = await checkCode(task.test_expression);

  if (passed) {
    el['task-status-icon'].textContent = '✅';
    appendConsole([{ kind: 'ok', text: '✓ Check passed! Great work.' }]);

    // Record completed task
    completedTasks[taskIndex] = {
      title:       task.title,
      code:        getEditorCode(),
      completedAt: new Date().toISOString(),
    };
    el['download-btn'].classList.remove('hidden');

    // Show a placeholder immediately, then replace with AI-generated message
    const bubble = addChatBubble('assistant', '🎉 …');
    const { provider, model, apiKey } = loadSettings();
    try {
      const msg = await congratulate({ task, hintCount, checkAttempts, provider, model, apiKey });
      bubble.innerHTML = renderMarkdown(msg);
    } catch {
      bubble.innerHTML = renderMarkdown(
        `🎉 You passed **${task.title}**! Nice work. If you'd like a tougher variation to cement the idea, just ask — otherwise the next task is waiting.`
      );
    }
    el['chat-messages'].scrollTop = el['chat-messages'].scrollHeight;
  } else {
    el['task-status-icon'].textContent = '❌';
    appendConsole([{ kind: 'fail', text: '✗ Not quite — check your output and try again.' }]);
  }
}

// ── Chat helpers ──────────────────────────────────────────────────────────────
function addChatBubble(role, text) {
  const div = document.createElement('div');
  div.className = `chat-msg ${role}`;
  div.innerHTML = renderMarkdown(text);
  el['chat-messages'].appendChild(div);
  el['chat-messages'].scrollTop = el['chat-messages'].scrollHeight;
  return div;
}

function addThinkingBubble() {
  return addChatBubble('thinking', '…thinking');
}

// ── Send chat message ─────────────────────────────────────────────────────────
async function handleSend(message) {
  if (isSending || !message?.trim()) return;
  const { provider, model, apiKey } = loadSettings();

  isSending = true;
  el['send-btn'].disabled = true;
  el['chat-input'].disabled = true;

  addChatBubble('user', message);
  el['chat-input'].value = '';

  const thinking = addThinkingBubble();

  try {
    const task = module?.tasks[taskIndex] ?? { title: '', instructions: '', hints: [] };
    const { reply, updatedHistory } = await sendMessage({
      userMessage: message,
      task,
      code:        getEditorCode(),
      lastOutput,
      history:     chatHistory,
      provider,
      model,
      apiKey,
    });

    chatHistory = updatedHistory;
    thinking.remove();
    addChatBubble('assistant', reply);
  } catch (err) {
    thinking.remove();
    const errDiv = document.createElement('div');
    errDiv.className = 'chat-msg error';
    errDiv.textContent = `⚠ ${err.message}`;
    el['chat-messages'].appendChild(errDiv);
    el['chat-messages'].scrollTop = el['chat-messages'].scrollHeight;
  } finally {
    isSending = false;
    el['send-btn'].disabled = false;
    el['chat-input'].disabled = false;
    el['chat-input'].focus();
  }
}

// ── Solution download ─────────────────────────────────────────────────────────
function buildDownloadContent(format) {
  const date    = new Date().toLocaleDateString('en-CA'); // YYYY-MM-DD
  const title   = module?.title ?? 'R Solutions';
  const entries = Object.entries(completedTasks)
    .sort(([a], [b]) => Number(a) - Number(b))
    .map(([idx, { title: taskTitle, code }]) => ({ idx: Number(idx), taskTitle, code }));

  if (format === 'r') {
    const header = [
      `# ${title}`,
      `# Downloaded: ${date}`,
      `# Completed tasks: ${entries.length} of ${module?.tasks.length ?? '?'}`,
      '',
    ].join('\n');
    const body = entries.map(({ idx, taskTitle, code }) => [
      `# ${'─'.repeat(60)}`,
      `# Task ${idx + 1}: ${taskTitle}`,
      `# ${'─'.repeat(60)}`,
      '',
      code.trim(),
      '',
    ].join('\n')).join('\n');
    return { content: header + body, ext: 'R', mime: 'text/plain' };
  }

  const isQmd  = format === 'qmd';
  const yaml   = isQmd
    ? `---\ntitle: "${title}"\ndate: "${date}"\nformat: html\n---\n`
    : `---\ntitle: "${title}"\ndate: "${date}"\noutput: html_document\n---\n`;

  const body = entries.map(({ idx, taskTitle, code }) => {
    const instructions = module.tasks[idx]?.instructions ?? '';
    return [
      `## Task ${idx + 1}: ${taskTitle}`,
      '',
      instructions.trim(),
      '',
      '```{r}',
      code.trim(),
      '```',
      '',
    ].join('\n');
  }).join('\n');

  const ext = isQmd ? 'qmd' : 'Rmd';
  return { content: yaml + '\n' + body, ext, mime: 'text/plain' };
}

function triggerDownload(format) {
  const slug    = (module?.id ?? 'solutions').replace(/[^a-z0-9]/gi, '-').toLowerCase();
  const { content, ext, mime } = buildDownloadContent(format);
  const blob    = new Blob([content], { type: mime });
  const url     = URL.createObjectURL(blob);
  const a       = document.createElement('a');
  a.href        = url;
  a.download    = `${slug}-solutions.${ext}`;
  a.click();
  URL.revokeObjectURL(url);
}

// ── Panel drag-resize ─────────────────────────────────────────────────────────
function initDividerDrag() {
  const divider   = $('panel-divider');
  const taskPanel = $('task-panel');
  let dragging    = false;
  let startX, startW;

  divider.addEventListener('mousedown', e => {
    dragging = true;
    startX   = e.clientX;
    startW   = taskPanel.getBoundingClientRect().width;
    divider.classList.add('dragging');
    document.body.style.cursor = 'col-resize';
    document.body.style.userSelect = 'none';
  });

  document.addEventListener('mousemove', e => {
    if (!dragging) return;
    const delta = e.clientX - startX;
    const newW  = Math.max(320, Math.min(startW + delta, window.innerWidth - 320));
    taskPanel.style.width = `${newW}px`;
  });

  document.addEventListener('mouseup', () => {
    if (!dragging) return;
    dragging = false;
    divider.classList.remove('dragging');
    document.body.style.cursor = '';
    document.body.style.userSelect = '';
  });
}

// ── Event wiring ──────────────────────────────────────────────────────────────
function wireEvents() {
  // Editor collapse toggle — expand output panel to fill the freed space
  document.getElementById('editor-collapse-btn').addEventListener('click', () => {
    const container = document.getElementById('editor-container');
    const output    = document.getElementById('output-panel');
    const btn       = document.getElementById('editor-collapse-btn');
    const collapsed = container.classList.toggle('collapsed');
    output.classList.toggle('expanded', collapsed);
    btn.textContent = collapsed ? '▶ Editor' : '▼ Editor';
    if (!collapsed) editor?.refresh?.();
  });

  // Output collapse toggle
  document.getElementById('output-collapse-btn').addEventListener('click', () => {
    const panel     = document.getElementById('output-panel');
    const btn       = document.getElementById('output-collapse-btn');
    const collapsed = panel.classList.toggle('collapsed');
    btn.textContent = collapsed ? '▼ Output' : '▲ Output';
  });

  // Run / Reset / Check
  el['run-btn'].addEventListener('click', handleRun);

  el['reset-btn'].addEventListener('click', () => {
    if (!module) return;
    setEditorCode(module.tasks[taskIndex].starter_code ?? '');
    clearConsole();
    clearPlot();
  });

  el['check-btn'].addEventListener('click', handleCheck);

  // Task navigation
  el['prev-btn'].addEventListener('click', () => {
    if (taskIndex > 0) { taskIndex--; renderTask(); }
  });

  el['next-btn'].addEventListener('click', () => {
    if (module && taskIndex < module.tasks.length - 1) { taskIndex++; renderTask(); }
  });

  // Output tabs
  el['output-tabs'].addEventListener('click', e => {
    const btn = e.target.closest('.tab-btn');
    if (!btn) return;
    btn.dataset.tab === 'plot' ? showPlotTab() : showConsoleTab();
  });

  // Chat send button
  el['send-btn'].addEventListener('click', () => handleSend(el['chat-input'].value));

  // Ctrl/Cmd+Enter to send chat
  el['chat-input'].addEventListener('keydown', e => {
    if (e.key === 'Enter' && (e.metaKey || e.ctrlKey)) {
      e.preventDefault();
      handleSend(el['chat-input'].value);
    }
  });

  // Quick action buttons — track hint-seeking behaviour
  document.querySelectorAll('.quick-btn').forEach(btn => {
    btn.addEventListener('click', () => {
      const msg = btn.dataset.msg;
      if (msg.toLowerCase().includes('hint') || msg.toLowerCase().includes('stuck')) {
        hintCount++;
      }
      handleSend(msg);
    });
  });

  // Clear chat
  el['clear-chat-btn'].addEventListener('click', () => {
    chatHistory = [];
    el['chat-messages'].innerHTML = '';
    if (module) {
      addChatBubble('assistant',
        `Chat cleared. Still working on **${module.tasks[taskIndex].title}** — let me know if you need a hand.`);
    }
  });

  // Module selector
  el['module-select'].addEventListener('change', async () => {
    const val = el['module-select'].value;
    if (val === '__import__') {
      // Snap back to the currently loaded module so select doesn't show '— Import…'
      el['module-select'].value = Object.values(el['module-select'].options)
        .find(o => o.value !== '__import__' && module && o.text.replace(/^↗ /, '') === module.title)
        ?.value ?? el['module-select'].options[0].value;
      showImportModal();
      return;
    }
    if (!val || !isReady()) return;
    try {
      await loadModule(val);
      renderTask();
    } catch (e) {
      console.error('Module load failed:', e);
    }
  });

  // Import modal
  $('import-load-btn').addEventListener('click', handleImportLoad);
  $('import-cancel-btn').addEventListener('click', hideImportModal);
  $('import-modal').addEventListener('click', e => {
    if (e.target === $('import-modal')) hideImportModal();
  });
  $('import-url-input').addEventListener('keydown', e => {
    if (e.key === 'Enter') handleImportLoad();
    if (e.key === 'Escape') hideImportModal();
  });

  // Local file picker for student-view import
  $('import-pick-btn').addEventListener('click', () => $('import-file-input').click());
  $('import-file-input').addEventListener('change', e => {
    const file = e.target.files[0];
    if (!file) return;
    const reader = new FileReader();
    reader.onload = ev => {
      try {
        _importFileData = JSON.parse(ev.target.result);
        $('import-file-name').textContent = file.name;
        $('import-file-name').classList.add('chosen');
        $('import-url-input').value = '';   // clear URL if file chosen
      } catch {
        $('import-error').textContent = 'Could not parse JSON file.';
        $('import-error').classList.remove('hidden');
      }
    };
    reader.readAsText(file);
    e.target.value = '';
  });

  // Settings
  el['settings-btn'].addEventListener('click', () => {
    const { provider, model, apiKey } = loadSettings();
    el['ai-provider'].value   = provider;
    el['api-key-input'].value = apiKey;
    populateModelSelect(provider, model);
    el['settings-modal'].classList.remove('hidden');
  });

  // Repopulate model list when provider changes
  el['ai-provider'].addEventListener('change', () => {
    populateModelSelect(el['ai-provider'].value, '');
  });

  el['settings-save'].addEventListener('click', () => {
    saveSettings(
      el['ai-provider'].value,
      el['ai-model'].value,
      el['api-key-input'].value.trim(),
    );
    el['settings-modal'].classList.add('hidden');
  });

  el['settings-cancel'].addEventListener('click', () => {
    el['settings-modal'].classList.add('hidden');
  });

  // Close modal on backdrop click
  el['settings-modal'].addEventListener('click', e => {
    if (e.target === el['settings-modal']) el['settings-modal'].classList.add('hidden');
  });

  // Ctrl/Cmd+Enter to run code from editor
  document.addEventListener('keydown', e => {
    if ((e.metaKey || e.ctrlKey) && e.key === 'Enter' && !e.shiftKey) {
      const focus = document.activeElement;
      // Only trigger if focus is inside the code editor container
      if (el['code-editor'].contains(focus) || focus === document.body) {
        e.preventDefault();
        handleRun();
      }
    }
  });

  // Download solutions
  el['download-btn'].addEventListener('click', () => {
    const completed = Object.keys(completedTasks).length;
    const total     = module?.tasks.length ?? 0;
    $('download-summary').textContent =
      `${completed} of ${total} tasks completed. Choose a format to download your solutions.`;
    $('download-modal').classList.remove('hidden');
  });

  document.querySelectorAll('.download-format-btn').forEach(btn => {
    btn.addEventListener('click', () => {
      triggerDownload(btn.dataset.format);
      $('download-modal').classList.add('hidden');
    });
  });

  $('download-cancel-btn').addEventListener('click', () =>
    $('download-modal').classList.add('hidden'));

  $('download-modal').addEventListener('click', e => {
    if (e.target === $('download-modal')) $('download-modal').classList.add('hidden');
  });

  // Builder
  $('builder-btn').addEventListener('click', () => openBuilder(module));
  initBuilder();

  initDividerDrag();
}

// ── Bootstrap ─────────────────────────────────────────────────────────────────
async function main() {
  cacheElements();
  wireEvents();
  setLoading(true, 'Loading curriculum…');

  try {
    // loadManifest populates the selector and loads the first module
    let manifest = { modules: [{ id: 'intro-tidyverse', title: 'Introduction to the Tidyverse', file: 'curriculum/intro-tidyverse.json' }] };
    try {
      const res = await fetch('./curriculum/index.json');
      if (res.ok) manifest = await res.json();
    } catch {}
    _cachedManifest = manifest;

    const firstUrl = manifest.modules[0]?.file ?? 'curriculum/intro-tidyverse.json';
    await loadModule(firstUrl);
    rebuildModuleSelect(manifest, firstUrl);
  } catch (err) {
    setLoading(false);
    console.error('Failed to load curriculum:', err);
    return;
  }

  setLoading(true, 'Starting R environment…');
  setStatus('loading', '⏳ Loading R…');

  try {
    await initWebR((msg, progress) => {
      el['loading-message'].textContent = msg;
      setProgress(progress ?? 0, msg);
      if (msg !== 'Ready') setStatus('loading', `⏳ ${msg}`);
    });

    setLoading(false);
    setStatus('ready', '✅ R Ready');

    el['run-btn'].disabled   = false;
    el['check-btn'].disabled = false;

    renderTask();

    addChatBubble('assistant',
      `👋 Hi! I'm your R pair partner. I can see your current task and whatever code you've written.\n\nGive the task a try on your own first — I'll nudge you in the right direction if you get stuck. Good luck! 🚀`);

  } catch (err) {
    setLoading(false);
    setStatus('error', '❌ R Failed');
    addChatBubble('error',
      `Failed to start R: ${err.message}. Try refreshing the page.`);
    console.error(err);
  }
}

main();
