/**
 * builder.js — Curriculum Builder (instructor mode)
 *
 * Manages a full-screen overlay for creating and editing paircodeR
 * curriculum JSON files. Operates on a deep-copied working state so
 * the live student module is never mutated until the instructor
 * explicitly loads an export back into the app.
 */

// ── Working state ─────────────────────────────────────────────────────────────
let bModule      = null;   // deep copy of the module being edited
let activeIdx    = -1;     // currently selected task index
let starterCM    = null;   // CodeMirror for starter code
let solutionCM   = null;   // CodeMirror for solution code

// ── DOM shortcuts ─────────────────────────────────────────────────────────────
const $b = id => document.getElementById(id);

// ── Public API ────────────────────────────────────────────────────────────────
export function openBuilder(currentModule) {
  bModule = currentModule
    ? JSON.parse(JSON.stringify(currentModule))
    : emptyModule();

  $b('builder-overlay').classList.remove('hidden');
  renderModuleMeta();
  renderTaskList();
  selectTask(bModule.tasks.length > 0 ? 0 : -1);
  // Refresh CM instances after layout is visible
  setTimeout(() => { starterCM?.refresh?.(); solutionCM?.refresh?.(); }, 50);
}

export function closeBuilder() {
  flushActiveTask();
  $b('builder-overlay').classList.add('hidden');
}

// ── Module helpers ────────────────────────────────────────────────────────────
function emptyModule() {
  return {
    id: 'my-module',
    title: 'My Module',
    description: '',
    packages: [],
    tasks: [],
  };
}

function emptyTask(n) {
  return {
    id: `task-${String(n).padStart(2, '0')}`,
    title: `Task ${n}`,
    instructions: '',
    starter_code: '',
    solution_code: '',
    test_expression: 'TRUE',
    hints: [],
    ai_context: '',
  };
}

// ── Module metadata ───────────────────────────────────────────────────────────
function renderModuleMeta() {
  $b('b-mod-title').value    = bModule.title       ?? '';
  $b('b-mod-desc').value     = bModule.description ?? '';
  $b('b-mod-id').value       = bModule.id          ?? '';
  $b('b-mod-packages').value = (bModule.packages ?? []).join(', ');
}

function flushModuleMeta() {
  bModule.title       = $b('b-mod-title').value.trim();
  bModule.description = $b('b-mod-desc').value.trim();
  bModule.id          = $b('b-mod-id').value.trim();
  bModule.packages    = $b('b-mod-packages').value
    .split(',').map(s => s.trim()).filter(Boolean);
}

// ── Task list (sidebar) ───────────────────────────────────────────────────────
function renderTaskList() {
  const ul = $b('b-task-list');
  ul.innerHTML = '';

  bModule.tasks.forEach((task, i) => {
    const li = document.createElement('li');
    li.className = 'b-task-item' + (i === activeIdx ? ' active' : '');
    li.dataset.idx = i;

    const label = document.createElement('span');
    label.className = 'b-task-label';
    label.textContent = `${i + 1}. ${task.title || 'Untitled'}`;

    const controls = document.createElement('span');
    controls.className = 'b-task-controls';
    controls.innerHTML = `
      <button class="b-icon-btn up"  title="Move up"   data-idx="${i}">▲</button>
      <button class="b-icon-btn dn"  title="Move down" data-idx="${i}">▼</button>
      <button class="b-icon-btn del" title="Delete"    data-idx="${i}">✕</button>
    `;

    li.appendChild(label);
    li.appendChild(controls);
    ul.appendChild(li);

    li.addEventListener('click', e => {
      if (e.target.closest('.b-task-controls')) return;
      if (i !== activeIdx) { flushActiveTask(); selectTask(i); }
    });
  });

  ul.querySelectorAll('.b-icon-btn.up').forEach(btn =>
    btn.addEventListener('click', e => { e.stopPropagation(); moveTask(+btn.dataset.idx, -1); }));
  ul.querySelectorAll('.b-icon-btn.dn').forEach(btn =>
    btn.addEventListener('click', e => { e.stopPropagation(); moveTask(+btn.dataset.idx, +1); }));
  ul.querySelectorAll('.b-icon-btn.del').forEach(btn =>
    btn.addEventListener('click', e => { e.stopPropagation(); deleteTask(+btn.dataset.idx); }));
}

function moveTask(idx, dir) {
  flushActiveTask();
  const newIdx = idx + dir;
  if (newIdx < 0 || newIdx >= bModule.tasks.length) return;
  [bModule.tasks[idx], bModule.tasks[newIdx]] = [bModule.tasks[newIdx], bModule.tasks[idx]];
  activeIdx = newIdx;
  renderTaskList();
  renderTaskEditor(bModule.tasks[activeIdx]);
}

function deleteTask(idx) {
  if (!confirm(`Delete task "${bModule.tasks[idx].title}"?`)) return;
  bModule.tasks.splice(idx, 1);
  activeIdx = Math.min(activeIdx, bModule.tasks.length - 1);
  renderTaskList();
  if (bModule.tasks.length === 0) {
    activeIdx = -1;
    showEmptyState();
  } else {
    selectTask(activeIdx);
  }
}

// ── Task editor ───────────────────────────────────────────────────────────────
function selectTask(idx) {
  activeIdx = idx;
  renderTaskList();
  if (idx < 0 || idx >= bModule.tasks.length) { showEmptyState(); return; }
  renderTaskEditor(bModule.tasks[idx]);
}

function showEmptyState() {
  $b('b-empty-state').classList.remove('hidden');
  $b('b-task-editor').classList.add('hidden');
}

function renderTaskEditor(task) {
  $b('b-empty-state').classList.add('hidden');
  $b('b-task-editor').classList.remove('hidden');

  $b('b-task-title').value        = task.title            ?? '';
  $b('b-task-id').value           = task.id               ?? '';
  $b('b-task-instructions').value = task.instructions     ?? '';
  $b('b-task-test').value         = task.test_expression  ?? 'TRUE';
  $b('b-task-ai-context').value   = task.ai_context       ?? '';

  // CodeMirror editors
  initOrSetCM('starter',  task.starter_code  ?? '');
  initOrSetCM('solution', task.solution_code ?? '');

  // Hints
  renderHints(task.hints ?? []);
}

function initOrSetCM(which, code) {
  const containerId = `b-${which}-editor`;
  const container   = $b(containerId);

  if (which === 'starter') {
    if (!starterCM) {
      starterCM = window.CodeMirror(container, cmConfig(code));
    } else {
      starterCM.setValue(code);
    }
  } else {
    if (!solutionCM) {
      solutionCM = window.CodeMirror(container, cmConfig(code));
    } else {
      solutionCM.setValue(code);
    }
  }
}

function cmConfig(value) {
  return {
    value,
    mode:           'r',
    theme:          'dracula',
    lineNumbers:    true,
    indentWithTabs: false,
    tabSize:        2,
    lineWrapping:   true,
    autofocus:      false,
    extraKeys: { Tab: cm => cm.execCommand('indentMore') },
  };
}

// ── Hints ─────────────────────────────────────────────────────────────────────
function renderHints(hints) {
  const list = $b('b-hints-list');
  list.innerHTML = '';
  hints.forEach((hint, i) => addHintRow(hint, i));
}

function addHintRow(text = '', idx = null) {
  const list = $b('b-hints-list');
  const row  = document.createElement('div');
  row.className = 'b-hint-row';

  const input = document.createElement('input');
  input.type        = 'text';
  input.value       = text;
  input.placeholder = `Hint ${list.children.length + 1}`;

  const del = document.createElement('button');
  del.className   = 'b-icon-btn del';
  del.textContent = '✕';
  del.title       = 'Remove hint';
  del.addEventListener('click', () => row.remove());

  row.appendChild(input);
  row.appendChild(del);
  list.appendChild(row);
}

function getHints() {
  return [...$b('b-hints-list').querySelectorAll('input')]
    .map(i => i.value.trim()).filter(Boolean);
}

// ── Flush current task back to bModule ────────────────────────────────────────
function flushActiveTask() {
  if (activeIdx < 0 || activeIdx >= bModule.tasks.length) return;
  const task = bModule.tasks[activeIdx];

  task.title           = $b('b-task-title').value.trim();
  task.id              = $b('b-task-id').value.trim();
  task.instructions    = $b('b-task-instructions').value;
  task.test_expression = $b('b-task-test').value.trim();
  task.ai_context      = $b('b-task-ai-context').value.trim();
  task.starter_code    = starterCM  ? starterCM.getValue()  : task.starter_code;
  task.solution_code   = solutionCM ? solutionCM.getValue() : task.solution_code;
  task.hints           = getHints();

  // Refresh sidebar label
  const items = $b('b-task-list').querySelectorAll('.b-task-item');
  if (items[activeIdx]) {
    items[activeIdx].querySelector('.b-task-label').textContent =
      `${activeIdx + 1}. ${task.title || 'Untitled'}`;
  }
}

// ── Build final module object ─────────────────────────────────────────────────
function buildModuleJSON() {
  flushModuleMeta();
  flushActiveTask();
  return JSON.parse(JSON.stringify(bModule));
}

// ── Export ────────────────────────────────────────────────────────────────────
function exportJSON() {
  const data     = buildModuleJSON();
  const blob     = new Blob([JSON.stringify(data, null, 2)], { type: 'application/json' });
  const url      = URL.createObjectURL(blob);
  const a        = document.createElement('a');
  a.href         = url;
  a.download     = `${data.id || 'curriculum'}.json`;
  a.click();
  URL.revokeObjectURL(url);
}

// ── Import from file ──────────────────────────────────────────────────────────
function importFromFile(file) {
  const reader = new FileReader();
  reader.onload = e => {
    try {
      const data = JSON.parse(e.target.result);
      if (!Array.isArray(data.tasks)) throw new Error('Missing tasks array');
      bModule   = data;
      activeIdx = -1;
      renderModuleMeta();
      renderTaskList();
      selectTask(bModule.tasks.length > 0 ? 0 : -1);
    } catch (err) {
      alert(`Could not parse JSON: ${err.message}`);
    }
  };
  reader.readAsText(file);
}

// ── Wire events ───────────────────────────────────────────────────────────────
export function initBuilder() {
  $b('b-close-btn').addEventListener('click', closeBuilder);

  $b('b-export-btn').addEventListener('click', exportJSON);

  $b('b-new-btn').addEventListener('click', () => {
    if (bModule.tasks.length > 0 &&
        !confirm('Start a new module? Unsaved changes will be lost.')) return;
    bModule   = emptyModule();
    activeIdx = -1;
    renderModuleMeta();
    renderTaskList();
    showEmptyState();
  });

  $b('b-import-btn').addEventListener('click', () => $b('b-file-input').click());

  $b('b-file-input').addEventListener('change', e => {
    const file = e.target.files[0];
    if (file) importFromFile(file);
    e.target.value = '';
  });

  $b('b-add-task-btn').addEventListener('click', () => {
    flushActiveTask();
    const n = bModule.tasks.length + 1;
    bModule.tasks.push(emptyTask(n));
    renderTaskList();
    selectTask(bModule.tasks.length - 1);
  });

  $b('b-add-hint-btn').addEventListener('click', () => addHintRow());

  // Auto-flush module meta on change
  ['b-mod-title', 'b-mod-desc', 'b-mod-id', 'b-mod-packages'].forEach(id => {
    $b(id).addEventListener('input', flushModuleMeta);
  });

  // Auto-flush task title into sidebar label on input
  $b('b-task-title').addEventListener('input', () => {
    if (activeIdx < 0) return;
    const items = $b('b-task-list').querySelectorAll('.b-task-item');
    if (items[activeIdx]) {
      items[activeIdx].querySelector('.b-task-label').textContent =
        `${activeIdx + 1}. ${$b('b-task-title').value || 'Untitled'}`;
    }
  });
}
