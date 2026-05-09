/**
 * webr-runner.js
 * Manages the WebR (R-in-WASM) lifecycle: init, package loading, code execution,
 * text output capture, and canvas plot capture.
 */

import { WebR, ChannelType } from 'https://webr.r-wasm.org/latest/webr.mjs';

let webR    = null;
let shelter = null;
let ready   = false;

// Packages to pre-load (core tidyverse + tibble)
const PACKAGES = [
  'dplyr', 'ggplot2', 'tidyr', 'readr',
  'purrr', 'stringr', 'forcats', 'tibble',
];

// Progress fractions: init=5%, install each pkg=~7%, load each pkg=~3%, finish=5%
const INSTALL_FRAC = 0.56 / PACKAGES.length;  // 56% of bar for installs
const LOAD_FRAC    = 0.30 / PACKAGES.length;  // 30% for library() calls

/**
 * Initialize WebR and load tidyverse packages.
 * @param {function(string, number): void} onStatus
 *   Called with (message, progress) where progress is 0–1.
 */
export async function init(onStatus = () => {}) {
  onStatus('Starting R environment…', 0.02);

  webR = new WebR({ channelType: ChannelType.PostMessage });
  await webR.init();
  onStatus('R started — installing packages…', 0.05);

  // Install one package at a time so we can report progress
  for (let i = 0; i < PACKAGES.length; i++) {
    const pkg = PACKAGES[i];
    onStatus(
      `Installing ${pkg} (${i + 1} / ${PACKAGES.length})…`,
      0.05 + i * INSTALL_FRAC,
    );
    await webR.installPackages([pkg], { quiet: true });
  }

  // Load each package individually for granular feedback
  for (let i = 0; i < PACKAGES.length; i++) {
    const pkg = PACKAGES[i];
    onStatus(
      `Loading ${pkg}…`,
      0.61 + i * LOAD_FRAC,
    );
    await webR.evalRVoid(`suppressMessages(suppressWarnings(library(${pkg})))`);
  }

  shelter = await new webR.Shelter();
  ready = true;
  onStatus('Ready', 1.0);
}

/**
 * Run R code, capture text output, and optionally render plots to a canvas.
 *
 * @param {string}            code         - R source to evaluate
 * @param {HTMLCanvasElement} [plotCanvas] - target canvas for plot output
 * @returns {{ ok: boolean, output: Array<{kind:string, text:string}>, hasPlot: boolean }}
 */
const PLOT_FILE = '/tmp/.webr_plot.svg';

export async function run(code, plotCanvas = null) {
  if (!ready) throw new Error('WebR not initialised');

  // Clear canvas
  if (plotCanvas) {
    plotCanvas.getContext('2d').clearRect(0, 0, plotCanvas.width, plotCanvas.height);
    plotCanvas.classList.remove('has-plot');
  }

  // Wrap user code with svg() open/close inside the SAME captureR call so the
  // device is guaranteed to be open when ggplot/base-R renders.
  let codeToRun = code;
  if (plotCanvas) {
    const w = (plotCanvas.width  / 96).toFixed(2);
    const h = (plotCanvas.height / 96).toFixed(2);
    codeToRun = `svg('${PLOT_FILE}', width=${w}, height=${h})\n${code}\nif (dev.cur() > 1) invisible(dev.off())`;
  }

  let capture;
  try {
    capture = await shelter.captureR(codeToRun, {
      withAutoprint: true,
      captureStreams: true,
      captureConditions: false,
    });
  } catch (e) {
    try { await webR.evalRVoid('if (dev.cur() > 1) dev.off()'); } catch {}
    return {
      ok: false,
      output: [{ kind: 'err', text: String(e.message ?? e) }],
      hasPlot: false,
    };
  }

  // Map text output
  const output = (capture.output ?? []).map(line => {
    switch (line.type) {
      case 'stdout':  return { kind: 'out',  text: line.data };
      case 'stderr':  return { kind: 'err',  text: line.data };
      case 'message': return { kind: 'msg',  text: line.data };
      case 'warning': return { kind: 'warn', text: `Warning: ${line.data?.message ?? line.data}` };
      case 'error':   return { kind: 'err',  text: line.data?.message ?? String(line.data) };
      default:        return null;
    }
  }).filter(Boolean);

  // Read the PNG back from the WebR virtual filesystem and draw to canvas
  let hasPlot = false;
  if (plotCanvas) {
    try {
      const bytes   = await webR.FS.readFile(PLOT_FILE);
      const svgText = new TextDecoder().decode(bytes);

      if (svgText.length > 500) {
        const url = 'data:image/svg+xml;charset=utf-8,' + encodeURIComponent(svgText);
        const img = new Image();
        img.src = url;
        await new Promise(resolve => { img.onload = resolve; img.onerror = resolve; });
        plotCanvas.getContext('2d').clearRect(0, 0, plotCanvas.width, plotCanvas.height);
        plotCanvas.getContext('2d').drawImage(img, 0, 0, plotCanvas.width, plotCanvas.height);
        plotCanvas.classList.add('has-plot');
        hasPlot = true;
      }
    } catch (e) {
      console.warn('[webr] plot read failed:', e.message);
    }
  }

  // Drain any leftover messages from the queue
  try { await webR.flush(); } catch {}

  return { ok: true, output, hasPlot };
}

/**
 * Evaluate a test expression (must return a single logical TRUE/FALSE).
 * Used for task checking.
 *
 * @param {string} testExpr - R expression returning TRUE or FALSE
 * @returns {Promise<boolean>}
 */
export async function check(testExpr) {
  if (!ready) return false;
  try {
    const capture = await shelter.captureR(testExpr, {
      withAutoprint: false,
      captureStreams: false,
      captureConditions: false,
    });
    const val = await capture.result.toJs();
    return Array.isArray(val?.values) ? val.values[0] === true : false;
  } catch {
    return false;
  }
}

export const isReady = () => ready;
