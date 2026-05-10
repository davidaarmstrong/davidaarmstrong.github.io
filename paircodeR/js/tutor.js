/**
 * tutor.js
 * AI Pair Partner — supports Claude (Anthropic), OpenAI, and Gemini.
 * The user supplies their own API key; it is stored only in localStorage.
 */

const SYSTEM_PROMPT = `You are an expert R programmer acting as a Socratic pair coding partner for a learner. \
Your goal is to help learners discover solutions themselves, not to hand them answers.

TEACHING PRINCIPLES:
- Ask guiding questions before giving explanations
- Celebrate genuine attempts, even broken ones — effort deserves praise
- Give the smallest useful nudge first; only go deeper if explicitly asked
- Never write a complete working solution — partial skeletons or pseudocode at most
- Reference tidyverse concepts naturally (tibbles, pipes, tidy data, verbs like filter/mutate/summarise)
- Be warm, patient, and encouraging — learning to code is hard
- When the learner's code doesn't work, ask "what do you think might be happening here?" before explaining
- When the learner is completely stuck after a real effort, give a small concrete hint and encourage them to try it

STYLE:
- Keep responses concise — 2–4 short paragraphs max unless a concept truly needs more
- Use inline \`code\` for function names and short snippets
- Use fenced code blocks only for illustrative (incomplete) examples, never for copy-paste solutions
- Don't repeat the task instructions back verbatim`;

/** Build the context block injected at the start of each conversation turn. */
function buildContext(task, code, lastOutput) {
  const parts = [
    `**Task:** ${task.title}`,
    `**Instructions:** ${task.instructions}`,
  ];
  if (code?.trim()) {
    parts.push(`**Learner's current code:**\n\`\`\`r\n${code.trim()}\n\`\`\``);
  }
  if (lastOutput?.trim()) {
    const truncated = lastOutput.length > 600 ? lastOutput.slice(0, 600) + '…' : lastOutput;
    parts.push(`**Last console output:**\n\`\`\`\n${truncated}\n\`\`\``);
  }
  if (task.hints?.length) {
    parts.push(`**Hints available for this task (reveal progressively, never all at once):**\n${task.hints.map((h, i) => `${i + 1}. ${h}`).join('\n')}`);
  }
  return parts.join('\n\n');
}

/**
 * Send a message to the AI pair partner.
 *
 * @param {object} opts
 * @param {string}   opts.userMessage  - what the learner typed
 * @param {object}   opts.task         - current task object
 * @param {string}   opts.code         - current editor contents
 * @param {string}   opts.lastOutput   - last console output text
 * @param {Array}    opts.history      - prior message array (provider-specific format)
 * @param {string}   opts.provider     - 'claude' | 'openai' | 'gemini'
 * @param {string}   opts.apiKey       - user's API key
 * @returns {Promise<{ reply: string, updatedHistory: Array }>}
 */
export async function sendMessage({ userMessage, task, code, lastOutput, history, provider, model, apiKey }) {
  if (!apiKey?.trim()) {
    throw new Error('No API key set. Click ⚙ in the top-right to add one.');
  }

  const context = buildContext(task, code, lastOutput);

  if (provider === 'claude') {
    return callClaude({ userMessage, context, history, apiKey, model });
  }
  if (provider === 'openai' || provider === 'gemini' || provider === 'github') {
    return callOpenAICompat({ userMessage, context, history, apiKey, provider, model });
  }
  throw new Error(`Unknown AI provider: ${provider}`);
}

// ── Claude (Anthropic) ────────────────────────────────────────────────────────

async function callClaude({ userMessage, context, history, apiKey, model }) {
  // Prepend context to the first user turn, or add a context-refresh line
  // to subsequent turns so the AI always knows the current code/output state.
  const messages = history.length === 0
    ? [{ role: 'user', content: `${context}\n\n---\n\n${userMessage}` }]
    : [...history, { role: 'user', content: `[Context refresh]\n${context}\n\n---\n\n${userMessage}` }];

  const res = await fetch('https://api.anthropic.com/v1/messages', {
    method: 'POST',
    headers: {
      'Content-Type': 'application/json',
      'x-api-key': apiKey,
      'anthropic-version': '2023-06-01',
      // Required header for direct browser-to-Anthropic calls
      'anthropic-dangerous-direct-browser-access': 'true',
    },
    body: JSON.stringify({
      model: model || 'claude-sonnet-4-6',
      max_tokens: 1024,
      system: SYSTEM_PROMPT,
      messages,
    }),
  });

  if (!res.ok) {
    const err = await res.json().catch(() => ({}));
    throw new Error(err.error?.message ?? `Anthropic API error ${res.status}`);
  }

  const data  = await res.json();
  const reply = data.content[0].text;

  // Append both turns to history for future context
  const updatedHistory = [
    ...messages,
    { role: 'assistant', content: reply },
  ];
  return { reply, updatedHistory };
}

// ── OpenAI-compatible (OpenAI + Gemini) ───────────────────────────────────────

async function callOpenAICompat({ userMessage, context, history, apiKey, provider, model }) {
  const config = {
    openai: {
      url:          'https://api.openai.com/v1/chat/completions',
      defaultModel: 'gpt-4o-mini',
    },
    gemini: {
      url:          'https://generativelanguage.googleapis.com/v1beta/openai/chat/completions',
      defaultModel: 'gemini-1.5-flash',
    },
    github: {
      url:          'https://models.inference.ai.azure.com/chat/completions',
      defaultModel: 'gpt-4o-mini',
    },
  }[provider];
  config.model = model || config.defaultModel;

  // OpenAI-style message format includes the system message as first element
  const baseHistory = history.length === 0
    ? [{ role: 'system', content: SYSTEM_PROMPT }]
    : history;

  const userContent = history.length === 0
    ? `${context}\n\n---\n\n${userMessage}`
    : `[Context refresh]\n${context}\n\n---\n\n${userMessage}`;

  const messages = [...baseHistory, { role: 'user', content: userContent }];

  const res = await fetch(config.url, {
    method: 'POST',
    headers: {
      'Content-Type': 'application/json',
      'Authorization': `Bearer ${apiKey}`,
    },
    body: JSON.stringify({ model: config.model, messages, max_tokens: 1024 }),
  });

  if (!res.ok) {
    const err = await res.json().catch(() => ({}));
    const msg = err.error?.message ?? err.message ?? JSON.stringify(err);
    throw new Error(`${provider} ${res.status}: ${msg}`);
  }

  const data  = await res.json();
  const reply = data.choices[0].message.content;

  const updatedHistory = [
    ...messages,
    { role: 'assistant', content: reply },
  ];
  return { reply, updatedHistory };
}
