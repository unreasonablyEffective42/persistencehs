#!/usr/bin/env python3
import argparse
import json

import numpy as np


def knn1_distances(x: np.ndarray, block: int = 1024) -> np.ndarray:
    n = x.shape[0]
    out = np.empty(n, dtype=np.float64)
    for i in range(0, n, block):
        j = min(i + block, n)
        xb = x[i:j]  # (b, d)
        # squared Euclidean distances for this block against all points
        d2 = ((xb[:, None, :] - x[None, :, :]) ** 2).sum(axis=2)
        rows = np.arange(j - i)
        d2[rows, i + rows] = np.inf
        out[i:j] = np.sqrt(np.min(d2, axis=1))
    return out


def main() -> None:
    parser = argparse.ArgumentParser(
        description="Normalize point tuples and suggest epsilon ranges."
    )
    parser.add_argument("--in-bin", required=True, help="Input float32 binary (N x 6)")
    parser.add_argument("--cols", type=int, default=6)
    parser.add_argument("--mode", choices=["strict-global", "zscore"], default="strict-global")
    parser.add_argument("--out-bin", required=True)
    parser.add_argument("--out-csv", required=True)
    parser.add_argument("--out-json", required=True)
    args = parser.parse_args()

    raw = np.fromfile(args.in_bin, dtype=np.float32)
    if raw.size % args.cols != 0:
        raise ValueError(f"Input size {raw.size} not divisible by cols={args.cols}")
    x = raw.reshape(-1, args.cols).astype(np.float64)
    labels = ["lat", "lon", "t2m", "r2", "wdir10", "sp"][: args.cols]

    center = x.mean(axis=0)
    xc = x - center

    if args.mode == "strict-global":
        # One global scalar preserves all pairwise distances up to a constant factor.
        scale = float(np.sqrt(np.mean(xc**2)))
        if scale == 0.0:
            scale = 1.0
        xn = xc / scale
        per_dim_scale = [scale] * args.cols
    else:
        s = x.std(axis=0)
        s[s == 0] = 1.0
        xn = xc / s
        per_dim_scale = s.tolist()

    xn32 = xn.astype(np.float32)
    xn32.tofile(args.out_bin)
    np.savetxt(
        args.out_csv,
        xn32,
        delimiter=",",
        header=",".join(labels),
        comments="",
        fmt="%.7f",
    )

    nn = knn1_distances(xn.astype(np.float32))
    qvals = [0.05, 0.1, 0.2, 0.35, 0.5, 0.7, 0.85, 0.95]
    eps = {str(q): float(np.quantile(nn, q)) for q in qvals}

    meta = {
        "source": args.in_bin,
        "rows": int(x.shape[0]),
        "cols": int(x.shape[1]),
        "mode": args.mode,
        "point_order": labels,
        "center": center.tolist(),
        "scale": per_dim_scale,
        "note": "strict-global preserves pairwise distances up to one constant factor.",
        "epsilon_suggestions_from_nn1_quantiles": eps,
        "nn1_summary": {
            "min": float(nn.min()),
            "max": float(nn.max()),
            "median": float(np.median(nn)),
            "mean": float(nn.mean()),
        },
        "out_bin": args.out_bin,
        "out_csv": args.out_csv,
    }
    with open(args.out_json, "w", encoding="utf-8") as f:
        json.dump(meta, f, indent=2)

    print(json.dumps(meta, indent=2))


if __name__ == "__main__":
    main()
