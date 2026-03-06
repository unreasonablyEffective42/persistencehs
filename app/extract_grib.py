#!/usr/bin/env python3
import argparse
import json
import os

import cfgrib
import numpy as np
import xarray as xr


def main() -> None:
    parser = argparse.ArgumentParser(description="Extract selected variables from a GRIB file.")
    parser.add_argument("input", help="Path to GRIB file")
    parser.add_argument("--out-dir", default="extracted", help="Output directory")
    parser.add_argument(
        "--vars",
        nargs="*",
        default=["sp", "wdir10"],
        help="Variables to export (default: sp wdir10)",
    )
    args = parser.parse_args()

    os.makedirs(args.out_dir, exist_ok=True)
    manifest = {
        "input_file": args.input,
        "shape": None,
        "dtype": "float32",
        "order": "C",
        "variables": {},
        "groups": [],
    }
    datasets = cfgrib.open_datasets(args.input)
    lat_written = False
    lon_written = False

    for i, ds in enumerate(datasets):
        group_meta = {
            "index": i,
            "dims": dict(ds.sizes),
            "coords": {},
            "variables": {},
        }

        if "heightAboveGround" in ds.coords:
            group_meta["coords"]["heightAboveGround"] = float(ds["heightAboveGround"].values)
        if "surface" in ds.coords:
            group_meta["coords"]["surface"] = float(ds["surface"].values)

        for name in ds.data_vars:
            if args.vars and name not in args.vars:
                continue
            arr = ds[name].values.astype(np.float32, copy=False)
            arr = np.nan_to_num(arr, nan=np.float32(np.nan))
            out_file = f"{name}.bin"
            arr.tofile(os.path.join(args.out_dir, out_file))
            manifest["variables"][name] = out_file
            group_meta["variables"][name] = out_file
            if manifest["shape"] is None:
                manifest["shape"] = list(arr.shape)

        if (not lat_written) and ("latitude" in ds.coords):
            arr = ds["latitude"].values.astype(np.float32, copy=False)
            arr.tofile(os.path.join(args.out_dir, "latitude.bin"))
            manifest["latitude_shape"] = list(arr.shape)
            manifest["latitude_file"] = "latitude.bin"
            lat_written = True
        if (not lon_written) and ("longitude" in ds.coords):
            arr = ds["longitude"].values.astype(np.float32, copy=False)
            arr.tofile(os.path.join(args.out_dir, "longitude.bin"))
            manifest["longitude_shape"] = list(arr.shape)
            manifest["longitude_file"] = "longitude.bin"
            lon_written = True

        if group_meta["variables"]:
            manifest["groups"].append(group_meta)

    with open(os.path.join(args.out_dir, "manifest.json"), "w", encoding="utf-8") as f:
        json.dump(manifest, f, indent=2)

    print(json.dumps(manifest, indent=2))


if __name__ == "__main__":
    main()
