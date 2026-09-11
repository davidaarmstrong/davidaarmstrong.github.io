// Minimal dense linear algebra -- just enough for IRLS logistic regression
// and multivariate-normal simulation. Matrices are arrays of arrays
// (row-major); vectors are plain arrays. No external dependency: the
// model sizes here (dozens of coefficients, tens of thousands of rows) are
// small enough that a hand-rolled Gauss-Jordan inverse and Cholesky
// decomposition are plenty fast in-browser.

export function zeros(rows, cols) {
  return Array.from({ length: rows }, () => new Array(cols).fill(0));
}

export function transpose(A) {
  const rows = A.length;
  const cols = A[0].length;
  const T = zeros(cols, rows);
  for (let i = 0; i < rows; i++) for (let j = 0; j < cols; j++) T[j][i] = A[i][j];
  return T;
}

export function matMul(A, B) {
  const n = A.length;
  const k = B.length;
  const m = B[0].length;
  const C = zeros(n, m);
  for (let i = 0; i < n; i++) {
    const Ai = A[i];
    for (let p = 0; p < k; p++) {
      const a = Ai[p];
      if (a === 0) continue;
      const Bp = B[p];
      const Ci = C[i];
      for (let j = 0; j < m; j++) Ci[j] += a * Bp[j];
    }
  }
  return C;
}

export function matVec(A, x) {
  return A.map((row) => row.reduce((s, a, j) => s + a * x[j], 0));
}

// X' %*% diag(w) %*% X, computed without materializing the diagonal matrix.
export function crossprodWeighted(X, w) {
  const n = X.length;
  const p = X[0].length;
  const XtWX = zeros(p, p);
  for (let i = 0; i < n; i++) {
    const wi = w[i];
    if (wi === 0) continue;
    const xi = X[i];
    for (let a = 0; a < p; a++) {
      const xa = xi[a] * wi;
      if (xa === 0) continue;
      for (let b = a; b < p; b++) {
        XtWX[a][b] += xa * xi[b];
      }
    }
  }
  for (let a = 0; a < p; a++) for (let b = 0; b < a; b++) XtWX[a][b] = XtWX[b][a];
  return XtWX;
}

// X' %*% diag(w) %*% z
export function crossprodWeightedVec(X, w, z) {
  const n = X.length;
  const p = X[0].length;
  const out = new Array(p).fill(0);
  for (let i = 0; i < n; i++) {
    const wz = w[i] * z[i];
    if (wz === 0) continue;
    const xi = X[i];
    for (let a = 0; a < p; a++) out[a] += xi[a] * wz;
  }
  return out;
}

// Gauss-Jordan inverse with partial pivoting. Throws on a singular matrix
// (e.g. perfect separation or a redundant/collinear column set).
export function inverse(A) {
  const n = A.length;
  const M = A.map((row, i) => [...row, ...Array.from({ length: n }, (_, j) => (i === j ? 1 : 0))]);
  for (let col = 0; col < n; col++) {
    let pivot = col;
    let maxAbs = Math.abs(M[col][col]);
    for (let r = col + 1; r < n; r++) {
      if (Math.abs(M[r][col]) > maxAbs) { maxAbs = Math.abs(M[r][col]); pivot = r; }
    }
    if (maxAbs < 1e-12) {
      throw new Error("Matrix is singular or nearly singular -- the model may have collinear or redundant predictors (e.g. a variable with no variation in this subset).");
    }
    if (pivot !== col) { const tmp = M[col]; M[col] = M[pivot]; M[pivot] = tmp; }
    const pv = M[col][col];
    for (let j = 0; j < 2 * n; j++) M[col][j] /= pv;
    for (let r = 0; r < n; r++) {
      if (r === col) continue;
      const factor = M[r][col];
      if (factor === 0) continue;
      for (let j = 0; j < 2 * n; j++) M[r][j] -= factor * M[col][j];
    }
  }
  return M.map((row) => row.slice(n));
}

// Lower-triangular Cholesky factor L such that L %*% t(L) = A.
export function cholesky(A) {
  const n = A.length;
  const L = zeros(n, n);
  for (let i = 0; i < n; i++) {
    for (let j = 0; j <= i; j++) {
      let sum = A[i][j];
      for (let k = 0; k < j; k++) sum -= L[i][k] * L[j][k];
      if (i === j) {
        if (sum <= 0) sum = 1e-10; // guard tiny negative rounding on the diagonal
        L[i][j] = Math.sqrt(sum);
      } else {
        L[i][j] = sum / L[j][j];
      }
    }
  }
  return L;
}
