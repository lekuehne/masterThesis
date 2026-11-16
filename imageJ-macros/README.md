# Microfluidics Fluorescence Image Analysis Pipeline

Three independent Fiji/ImageJ macros for processing fluorescence TIFFs of microfluidic chips, plus one launcher script.

| File | Purpose |
|------|---------|
| `1_align.ijm` | Interactive alignment to a reference line |
| `2_bg_subtract.ijm` | Rolling-ball background subtraction (automatic, can run headless) |
| `3_measure_roi.ijm` | ROI check + measurement → CSV |
| `run_pipeline.sh` | Launcher: `./run_pipeline.sh <align\|bgsub\|measure> [options]` |

No extra plugin is needed. Alignment is implemented inside the macro (rigid rotation + translation).

## Data layout

```
012-t3/                      ← --input  (012 = experiment, t3 = time point)
├── anyname/                 ← sub-folders: free naming, any depth
│   ├── 7_ch00.tif           ← 7 = chip/condition, ch00 = channel
│   └── 7_ch01.tif
├── aligned/                 ← step 1 output (same sub-folder tree)
│   └── reference_line.roi
├── bg_subtracted/           ← step 2 output (same sub-folder tree)
├── results.csv              ← step 3 output
└── realign.txt              ← files flagged in step 3 (optional)
```
All TIFFs below `--input` are found recursively; `aligned/` and `bg_subtracted/` are ignored when scanning the raw data.

## Usage

```bash
chmod +x run_pipeline.sh

./run_pipeline.sh align   -i /data/012-t3 -R /data/012-t3/anyname/7_ch00.tif
./run_pipeline.sh bgsub   -i /data/012-t3 -b 7
./run_pipeline.sh measure -i /data/012-t3 -r /data/RoiSet.zip
```
Add `-f /path/to/Fiji` if Fiji is not found automatically. Run `./run_pipeline.sh --help` for all options.
Each macro also runs from the Fiji GUI (edit the defaults at the top of the file).

### Step 1 – align
1. A **reference line** is drawn once, on `--ref-image` (default: first image found) and saved to `aligned/reference_line.roi`.
   Pass `--ref-line file.roi` to reuse a saved reference line without drawing it again.
2. For every other image, draw a line along the **same structural edge**. The image is rotated so the lines are parallel and translated so the line midpoints coincide.
   Line direction does not matter. Translation is rounded to whole pixels, so only the rotation resamples the data.

### Step 2 – background subtraction
`Subtract Background... rolling=<r>` on every file in `aligned/`, written to `bg_subtracted/`. Runs headless by default (`--gui` to disable).

### Step 3 – measure
For each channel set the windows open non-blocking, so you can zoom and edit the ROIs. Adjustments apply to all channels of the set and never change `RoiSet.zip`.
Hotkeys (image window in focus): **F2** next ROI, **F1** previous ROI. A key press stores your last move, selects the next ROI and zooms to it; move it with the arrow keys. To shift all ROIs at once use ROI Manager → More » Translate. Keys can be changed at the top of `3_measure_roi.ijm`.
Re-running step 3 on a finished folder also rebuilds the metadata columns of an existing CSV.

For each image the ROIs are overlaid and you choose **Measure**, **Skip**, **Flag re-align** or **Abort** (results so far are saved).

**Re-aligning flagged files**
```bash
./run_pipeline.sh align   -i /data/012-t3 --only /data/012-t3/realign.txt
./run_pipeline.sh bgsub   -i /data/012-t3 --only /data/012-t3/realign.txt
./run_pipeline.sh measure -i /data/012-t3 -r RoiSet.zip --only /data/012-t3/realign.txt -o redo.csv
```
(With `--only` and no reference option, step 1 reuses `aligned/reference_line.roi`.)

## Resuming interrupted runs

By default, work that is already done is omitted, so an aborted or crashed run is simply restarted with the same command:

- **align**: a channel set is omitted if all its files already exist in `aligned/` (or were skipped by you earlier, see `aligned/skipped.txt`). An existing `aligned/reference_line.roi` is re-used so the reference stays identical.
- **bgsub**: files already in `bg_subtracted/` are omitted.
- **measure**: the CSV is saved after every set. On restart the existing CSV is loaded and sets already in it are omitted. Sets you skipped or flagged are asked again.

`--overwrite` redoes everything (and draws a new reference line). Files listed with `--only` are always redone. If the last file was being written when Fiji was killed, delete that one output file and rerun.

## Output CSV

One row per image × ROI.

| Column | Source (file name `012-t1_TileScan 6_19_ch01.tif`) |
|--------|--------|
| `Experiment` | `012` |
| `Timepoint` | `1` |
| `TileScan` | `6` (space after "TileScan" optional) |
| `Chip` | `19`; any text without `_` works, e.g. `B`, `PB` |
| `Channel` | `01` |
| `File`, `Subfolder`, `ROI_index` | traceability |
| `Label`, `Area`, `Mean`, `StdDev`, `Min`, `Max`, `IntDen`, `Median` | ImageJ measurements |

`NA` is written if a name does not follow the convention (a warning is logged). Excel may strip the leading zeros of `012`/`00` on import; Python/R keep them if read as strings.

## Servers
Steps `align` and `measure` need a display (`ssh -X`, or `Xvfb :99 &` + `export DISPLAY=:99`). `bgsub` runs headless.
