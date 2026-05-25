# paircodeR

An AI-powered pair coding environment for learning R — structured tidyverse tasks with a Socratic AI partner that guides rather than answers.

## What it does

- **Split-panel layout** — task instructions + live R code editor on the left, AI pair partner chat on the right
- **Real R execution** — powered by [WebR](https://webr.r-wasm.org/) (R compiled to WebAssembly), runs entirely in the browser with no server needed
- **Tidyverse pre-loaded** — dplyr, ggplot2, tidyr, readr, purrr, stringr, forcats, tibble
- **Plot output** — ggplot2 and base R graphics render to an in-browser canvas
- **Socratic AI tutor** — the AI is instructed to ask questions and give nudges, not hand over answers; works with Claude, ChatGPT, or Gemini (bring your own API key)
- **Task checking** — automated test expressions verify solutions
- **Pluggable curriculum** — modules are plain JSON files; swap or add your own

## Quick start

1. Serve the repo root over HTTP (required for WebR's WASM files):

   ```bash
   # Python
   python -m http.server 8080

   # Node
   npx serve .
   ```

2. Open `http://localhost:8080` in a modern browser.

3. Click ⚙ (top right) to enter your API key and choose a provider.

4. Wait ~30 seconds on first load while WebR downloads the tidyverse packages (cached after that).

## Project structure

```
paircodeR/
├── index.html                  # App shell
├── css/
│   └── style.css               # All styles
├── js/
│   ├── app.js                  # Main controller
│   ├── webr-runner.js          # WebR init, execution, plot capture
│   └── tutor.js                # AI pair partner (Claude / OpenAI / Gemini)
└── curriculum/
    └── intro-tidyverse.json    # Starter module (7 tasks)
```

## Curriculum JSON format

Add new modules by dropping a JSON file in `curriculum/`:

```json
{
  "id": "my-module",
  "title": "Module Title",
  "description": "...",
  "packages": ["dplyr"],
  "tasks": [
    {
      "id": "task-01",
      "title": "Task title",
      "instructions": "Markdown instructions shown to the learner.",
      "starter_code": "# starter code\n",
      "solution_code": "# reference solution (not shown to learner)",
      "test_expression": "R expression returning TRUE if correct",
      "hints": ["hint 1", "hint 2"],
      "ai_context": "Notes for the AI about this task"
    }
  ]
}
```

Then register it in `app.js` → `loadModule()`, or add an `<option>` to `#module-select`.

## AI providers

| Provider | Key source | Notes |
|----------|-----------|-------|
| Claude (Anthropic) | console.anthropic.com | Default; best Socratic reasoning |
| ChatGPT (OpenAI) | platform.openai.com | Uses `gpt-4o` |
| Gemini (Google) | aistudio.google.com | Uses `gemini-2.0-flash` |

Keys are stored only in your browser's `localStorage`.

## Keyboard shortcuts

| Action | Shortcut |
|--------|----------|
| Run code | `Ctrl/Cmd + Enter` (editor focused) |
| Send chat | `Ctrl/Cmd + Enter` (chat input focused) |

## Roadmap

- [ ] Instructor mode — form-based task editor, export to JSON
- [ ] Multiple module selector
- [ ] Session persistence (save progress to localStorage)
- [ ] Analytics — time per task, hint count, pass/fail tracking
- [ ] GitHub Pages deployment with COOP/COEP headers for faster WebR
