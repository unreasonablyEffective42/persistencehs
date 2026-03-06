#!/usr/bin/env python3
import argparse
import json
import os
from typing import List

import numpy as np


def load_f32(path: str, shape: tuple[int, int]) -> np.ndarray:
    arr = np.fromfile(path, dtype=np.float32)
    expected = shape[0] * shape[1]
    if arr.size != expected:
        raise ValueError(f"{path}: expected {expected} float32 values, got {arr.size}")
    return arr.reshape(shape)


def main() -> None:
    parser = argparse.ArgumentParser(
        description="Build Point tuples in the order: lat, lon, then remaining variables."
    )
    parser.add_argument(
        "--manifest",
        default="extracted/manifest.json",
        help="Path to extraction manifest.json",
    )
    parser.add_argument(
        "--vars",
        nargs="*",
        default=["t2m", "r2", "wdir10", "sp"],
        help="Variable order after lat/lon",
    )
    parser.add_argument(
        "--out-bin",
        default="extracted/points_lat_lon_rest.f32",
        help="Output binary row-major float32 matrix (N x D)",
    )
    parser.add_argument(
        "--out-meta",
        default="extracted/points_lat_lon_rest.json",
        help="Output metadata json",
    )
    parser.add_argument(
        "--sample-csv",
        default="extracted/points_sample.csv",
        help="Sample CSV output for inspection",
    )
    parser.add_argument(
        "--sample-stride",
        type=int,
        default=25,
        help="Stride used to subsample grid for CSV output",
    )
    args = parser.parse_args()

    with open(args.manifest, "r", encoding="utf-8") as f:
        manifest = json.load(f)

    base_dir = os.path.dirname(args.manifest) or "."
    shape = tuple(manifest["shape"])
    lat = load_f32(os.path.join(base_dir, manifest["latitude_file"]), shape)
    lon = load_f32(os.path.join(base_dir, manifest["longitude_file"]), shape)

    selected: List[str] = []
    arrays = [lat, lon]
    for name in args.vars:
        fname = manifest["variables"].get(name)
        if fname is None:
            continue
        arrays.append(load_f32(os.path.join(base_dir, fname), shape))
        selected.append(name)

    grid = np.stack(arrays, axis=-1)  # (y, x, d)
    points = grid.reshape(-1, grid.shape[-1])  # (N, d)
    points.astype(np.float32, copy=False).tofile(args.out_bin)

    meta = {
        "source_manifest": args.manifest,
        "point_order": ["lat", "lon", *selected],
        "dtype": "float32",
        "rows": int(points.shape[0]),
        "cols": int(points.shape[1]),
        "grid_shape": list(shape),
        "binary_file": args.out_bin,
        "row_major": True,
    }
    with open(args.out_meta, "w", encoding="utf-8") as f:
        json.dump(meta, f, indent=2)

    ys = np.arange(0, shape[0], max(1, args.sample_stride))
    xs = np.arange(0, shape[1], max(1, args.sample_stride))
    sample = grid[np.ix_(ys, xs)].reshape(-1, grid.shape[-1])
    header = ",".join(meta["point_order"])
    np.savetxt(args.sample_csv, sample, delimiter=",", header=header, comments="", fmt="%.6f")

    print(json.dumps(meta, indent=2))
    print(f"sample_rows={sample.shape[0]} sample_csv={args.sample_csv}")


if __name__ == "__main__":
    main()
