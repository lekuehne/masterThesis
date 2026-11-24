// ============================================================
//  3_measure_roi.ijm  –  STEP 3/3: ROI measurement
//
//  Input : <input_dir>/bg_subtracted/**/*.tif  + RoiSet.zip
//  Output: CSV, one row per (image x ROI), with the columns
//          File, Subfolder, ROI_index, Experiment, Timepoint, TileScan,
//          Chip, Channel plus the standard ImageJ measurements.
//
//  Metadata (all from the FILE NAME):
//    <experiment>-t<timepoint>_TileScan<no>_<chip>_ch<channel>.tif
//    e.g. 012-t1_TileScan 6_19_ch01.tif -> Experiment 012, Timepoint 1,
//                                          TileScan 6, Chip 19, Channel 01
//         011-t0_TileScan 4_PB_ch00.tif -> ... TileScan 4, Chip PB, Channel 00
//    - Chip may be any text without an underscore (19, B, PB, 3a, ...)
//    - the space in "TileScan 4" is optional ("TileScan4" also works)
//    - underscores separate the properties; upper/lower case is ignored
//
//  CSV / RESUME: an existing output CSV is NEVER overwritten. It is read
//  (File + Subfolder columns) to find sets that are already measured, and
//  new rows are APPENDED in the column layout of the existing file.
//  With overwrite=true the old file is renamed to <name>.csv.bak first.
//
//  Args ("key=value|key=value"):
//    input_dir   base folder (e.g. .../012-t3)
//    roi_set     RoiSet.zip
//    output_csv  default <input_dir>/results.csv
//    only_list   (optional) text file with relative paths to measure
//    overwrite   true = start a new CSV (old one is kept as .bak). Default:
//                append to an existing CSV and omit sets already in it
//    quit        true = quit Fiji when done
//
//  CHANNEL SETS: files that differ only by the channel ending
//  (7_ch00.tif, 7_ch01.tif, ... in the same folder) are one sample.
//  They are opened together with the ROIs overlaid on every channel,
//  and ONE decision applies to the whole set.
//
//  ADJUSTING ROIs: for every set you can move the ROIs (ROI Manager:
//  select ROI, drag it on the image, click "Update"; or use the hotkeys
//  below). The adjusted ROIs
//  are used for ALL channels of the current set only. RoiSet.zip is never
//  modified; every new set starts again from the original RoiSet.zip.
//
//  HOTKEYS (work while an image window has focus; installed automatically):
//    F2 = next ROI, F1 = previous ROI. The key press stores the position of
//    the ROI you just moved, selects the next/previous ROI, and zooms/centres
//    on it. Move it with the arrow keys (or drag), press F2 again.
//    Stay in the SAME image window while stepping (use the clearest channel).
//    Change the keys in KEY_NEXT / KEY_PREV below.
//
//  Per set you choose: Measure / Adjust ROIs again / Skip / Flag for
//  re-align / Abort.
//  "Flag" appends all files of the set to <input_dir>/realign.txt, which can be
//  passed as only_list to steps 1 and 2 (then re-run step 3 with it).
// ============================================================

var found = newArray(0);
// result of parseName()
var p_ok = false;  var p_exp = "NA";  var p_time = "NA";
var p_tile = "NA"; var p_chip = "NA"; var p_ch = "NA";

input_dir  = "/path/to/012-t3/";
roi_set    = "/path/to/RoiSet.zip";
output_csv = "";
only_list  = "";
do_quit    = false;
overwrite  = false;
KEY_NEXT   = "F2";       // hotkey: next ROI
KEY_PREV   = "F1";       // hotkey: previous ROI
CENTER_X   = false;      // hotkeys: false = only centre the ROI vertically (y), keep x

arg = getArgument();
if (arg != "") {
    input_dir  = argValue(arg, "input_dir",  input_dir);
    roi_set    = argValue(arg, "roi_set",    roi_set);
    output_csv = argValue(arg, "output_csv", output_csv);
    only_list  = argValue(arg, "only_list",  only_list);
    quit_arg = argValue(arg, "quit", "false");
    do_quit   = (quit_arg == "true");
    ow_arg     = argValue(arg, "overwrite", "false");
    overwrite  = (ow_arg == "true");
}

input_dir = normDir(input_dir);
bg_dir    = input_dir + "bg_subtracted/";
if (output_csv == "") output_csv = input_dir + "results.csv";
realign_file = input_dir + "realign.txt";

if (!File.exists(bg_dir))  exit("Run step 2 first – not found: " + bg_dir);
if (!File.exists(roi_set)) exit("ROI set not found: " + roi_set);

scan(bg_dir, "", "", "");
files = applyOnly(found, only_list);
n = files.length;
if (n == 0) exit("No TIFF files found in: " + bg_dir);

installHotkeys(KEY_NEXT, KEY_PREV, CENTER_X);

print("=== STEP 3: ROI measurement ===");
print("Input     : " + bg_dir);
print("ROI set   : " + roi_set);
print("Output    : " + output_csv);
print("Files     : " + n);

run("Set Measurements...",
    "area mean standard min integrated median display redirect=None decimal=4");
run("Clear Results");

// ── Existing CSV: read ONLY to find sets that are already measured ─
if (only_list != "" && !overwrite && File.exists(output_csv))
    exit("Output file already exists: " + output_csv +
         "\nUse a different output (-o) for the --only run, or --overwrite.");
if (overwrite && File.exists(output_csv)) {
    bak = output_csv + ".bak";
    if (File.exists(bak)) File.delete(bak);
    File.rename(output_csv, bak);
    print("Old CSV kept as: " + bak);
}
resume = (File.exists(output_csv));
done_txt = "\n";
cols = newArray(0);
csv_rows = 0;
if (resume) {
    csv_lines = split(File.openAsString(output_csv), "\n");
    if (csv_lines.length < 1) exit("Existing CSV is empty: " + output_csv);
    cols = split(replace(csv_lines[0], "\r", ""), "(,)");
    fi = -1;  si = -1;
    for (c = 0; c < cols.length; c++) {
        if (String.trim(cols[c]) == "File") fi = c;
        if (String.trim(cols[c]) == "Subfolder") si = c;
    }
    if (fi < 0) exit("Existing CSV has no 'File' column - it will not be touched: " + output_csv);
    for (k = 1; k < csv_lines.length; k++) {
        tok = split(replace(csv_lines[k], "\r", ""), "(,)");
        if (tok.length <= fi) continue;
        relold = String.trim(tok[fi]);
        if (si >= 0 && si < tok.length && String.trim(tok[si]) != "")
            relold = String.trim(tok[si]) + "/" + relold;
        done_txt = done_txt + relold + "\n";
        csv_rows++;
    }
    print("Existing CSV: " + csv_rows + " rows read, they are kept unchanged. New rows are appended.");
}

// ── Group channel files: same folder + same name except _chNN ─
keys = newArray(n);
for (i = 0; i < n; i++)
    keys[i] = replace(files[i], "(?i)_ch[0-9]+\\.tiff?$", "");

done = newArray(n);
n_sets = 0;
rows_written = 0;
abort = false;
for (i = 0; i < n && !abort; i++) {
    if (done[i] == 1) continue;

    // collect all channels of this sample
    members = newArray(0);
    for (j = i; j < n; j++) {
        if (keys[j] == keys[i]) {
            members = Array.concat(members, files[j]);
            done[j] = 1;
        }
    }
    n_sets++;

    if (resume) {
        all_done = true;
        for (m = 0; m < members.length; m++)
            if (indexOf(done_txt, "\n" + members[m] + "\n") < 0) all_done = false;
        if (all_done) {
            print("[set " + n_sets + "] already measured, omitted: " + keys[i]);
            continue;
        }
    }
    paths = newArray(members.length);
    list_txt = "";
    for (m = 0; m < members.length; m++) {
        paths[m] = bg_dir + members[m];
        list_txt = list_txt + "\n   " + members[m];
    }

    // open all channels; start from the ORIGINAL ROI set for every set
    ids = openAll(paths);
    roiManager("reset");
    roiManager("Open", roi_set);

    adjust = true;
    while (adjust) {
        // Phase 1: NON-modal window -> zoom, scroll and edit ROIs freely
        showLive(ids);
        selectImage(ids[0]);
        waitForUser("Inspect / adjust ROIs  [set " + n_sets + "]",
            "Files in this set:" + list_txt + "\n\n" +
            "- Zoom and scroll freely in the image windows.\n" +
            "- " + KEY_NEXT + " / " + KEY_PREV + " (image window in focus) = next / previous ROI:\n" +
            "  stores your move, selects the next ROI, zooms to it.\n" +
            "  Move the selected ROI with the ARROW KEYS or by dragging.\n" +
            "  Stay in the same image window while stepping.\n" +
            "- Alternative: select a ROI in the ROI Manager, drag, click 'Update'.\n" +
            "- Shift ALL ROIs at once: ROI Manager > More >> Translate...\n" +
            "- Changes apply to ALL channels of this set only;\n" +
            "  RoiSet.zip is not modified.\n\n" +
            "Click OK when finished (OK without changes keeps the original ROIs).");

        // store the move of the ROI that is still being edited
        if (roiManager("index") >= 0 && selectionType() >= 0) roiManager("update");

        // Phase 2: show the (possibly adjusted) ROIs on every channel + decide
        roiManager("Deselect");
        showOverlay(ids);
        Dialog.create("ROI check  [set " + n_sets + "]");
        Dialog.addMessage("Files in this set:" + list_txt + "\n\n" +
            "The current ROIs are shown on every channel.\n" +
            "  Measure          → measure ALL channels and continue\n" +
            "  Adjust ROIs again→ back to inspecting / moving the ROIs\n" +
            "  Skip             → omit this set\n" +
            "  Flag re-align    → omit and write the set to realign.txt\n" +
            "  Abort            → stop and save results so far");
        Dialog.addChoice("Action:",
            newArray("Measure", "Adjust ROIs again", "Skip", "Flag re-align", "Abort"), "Measure");
        Dialog.show();
        action = Dialog.getChoice();
        adjust = (action == "Adjust ROIs again");
    }

    if (action == "Measure") {
        run("Clear Results");                        // table holds only the rows of this set
        for (m = 0; m < members.length; m++) {
            rel   = members[m];
            fname = File.getName(rel);
            sub   = "";
            if (indexOf(rel, "/") >= 0) sub = substring(rel, 0, lastIndexOf(rel, "/"));

            parseName(fname);
            if (!p_ok)
                print("  WARNING: file name does not match <exp>-t<n>_TileScan <n>_<chip>_ch<nn>.tif : " + fname);

            selectImage(ids[m]);
            run("Select None");
            first = nResults;
            roiManager("Deselect");
            roiManager("Measure");
            for (r = first; r < nResults; r++) {
                setResult("File", r, fname);
                setResult("Subfolder", r, sub);
                setResult("ROI_index", r, r - first + 1);
                setResult("Experiment", r, p_exp);
                setResult("Timepoint", r, p_time);
                setResult("TileScan", r, p_tile);
                setResult("Chip", r, p_chip);
                setResult("Channel", r, p_ch);
            }
            print("[set " + n_sets + "] measured " + (nResults - first) + " ROIs: " + rel);
        }
        updateResults();
        // write after every set (safe against crashes); never overwrite existing data
        if (!File.exists(output_csv)) {
            saveAs("Results", output_csv);           // new file in ImageJ's own layout
            csv_lines = split(File.openAsString(output_csv), "\n");
            cols = split(replace(csv_lines[0], "\r", ""), "(,)");
        } else {
            appendRows(output_csv, cols, csv_rows);  // existing file: append, same columns
        }
        csv_rows = csv_rows + nResults;
        rows_written = rows_written + nResults;
    } else if (action == "Flag re-align") {
        for (m = 0; m < members.length; m++) File.append(members[m], realign_file);
        print("[set " + n_sets + "] FLAGGED for re-alignment: " + keys[i]);
    } else if (action == "Abort") {
        abort = true;
    } else {
        print("[set " + n_sets + "] skipped: " + keys[i]);
    }
    closeAll(ids);
    roiManager("reset");
}

print("");
print("=== Step 3 complete: " + rows_written + " new rows written to " + output_csv + " ===");
if (File.exists(realign_file)) print("Files flagged for re-alignment: " + realign_file);
if (do_quit) { run("Close All"); run("Clear Results"); run("Quit"); }

// Write a small macro file with the hotkeys and install it (Plugins > Macros > Install)
function installHotkeys(kn, kp, centerX) {
    t = "macro \"Next ROI [" + kn + "]\" { stepRoi(1); }\n" +
        "macro \"Previous ROI [" + kp + "]\" { stepRoi(-1); }\n" +
        "function stepRoi(d) {\n" +
        "  n = roiManager(\"count\");\n" +
        "  if (n == 0) return;\n" +
        "  id = getImageID();\n" +
        "  idx = roiManager(\"index\");\n" +
        "  if (idx >= 0 && selectionType() >= 0) roiManager(\"update\");\n" +
        "  if (idx < 0) { idx = 0; if (d < 0) idx = n - 1; }\n" +
        "  else idx = idx + d;\n" +
        "  if (idx < 0) idx = 0;\n" +
        "  if (idx > n - 1) idx = n - 1;\n" +
        "  roiManager(\"select\", idx);\n" +
        "  selectImage(id);\n" +
        "  // keep the current zoom, only move the view\n" +
        "  getSelectionBounds(bx, by, bw, bh);\n" +
        "  getDisplayedArea(vx, vy, vw, vh);\n" +
        "  zoom = getZoom() * 100;\n" +
        "  cy = by + bh / 2;\n" +
        "  cx = vx + vw / 2;\n";
    if (centerX) t = t + "  cx = bx + bw / 2;\n";
    t = t +
        "  run(\"Set... \", \"zoom=\" + d2s(zoom, 2) + \" x=\" + round(cx) + \" y=\" + round(cy));\n" +
        "  selectWindow(getTitle());\n" +
        "  showStatus(\"ROI \" + (idx + 1) + \" / \" + n);\n" +
        "}\n";
    path = getDirectory("temp") + "roi_hotkeys.ijm";
    File.saveString(t, path);
    run("Install...", "install=[" + path + "]");
}

// Parse <exp>-t<time>_TileScan <no>_<chip>_ch<channel>.tif into the globals p_*
function parseName(fname) {
    p_ok = false;  p_exp = "NA";  p_time = "NA";  p_tile = "NA";  p_chip = "NA";  p_ch = "NA";
    c = lastIndexOf(toLowerCase(fname), "_ch");
    dot = lastIndexOf(fname, ".");
    if (c < 0 || dot < c) return;
    chs = substring(fname, c + 3, dot);
    parts = split(substring(fname, 0, c), "_");
    if (parts.length != 3) return;                       // exp-t_TileScan_chip
    p0 = toLowerCase(parts[0]);
    d = indexOf(p0, "-t");
    if (d < 1) return;
    e = substring(p0, 0, d);
    t = substring(p0, d + 2);
    ts = toLowerCase(parts[1]);
    if (!startsWith(ts, "tile")) return;
    ts = replace(ts, "[^0-9]", "");                      // "tilescan 4" -> "4"
    if (!matches(e, "[0-9]+") || !matches(t, "[0-9]+") ||
        !matches(ts, "[0-9]+") || !matches(chs, "[0-9]+")) return;
    p_exp = e;  p_time = t;  p_tile = ts;  p_chip = parts[2];  p_ch = chs;
    p_ok = true;
}

// Append the rows of the Results table to an existing CSV, using ITS column order
function appendRows(path, cols, startIndex) {
    out = "";
    for (r = 0; r < nResults; r++) {
        row = "";
        for (c = 0; c < cols.length; c++) {
            h = String.trim(cols[c]);
            if (h == "") v = "" + (startIndex + r + 1);          // row-number column
            else if (h == "Label") v = getResultLabel(r);
            else {
                v = getResultString(h, r);
                if (v == "null") v = "";                        // column not in this table
            }
            if (c > 0) row = row + ",";
            row = row + v;
        }
        if (r > 0) out = out + "\n";
        out = out + row;
    }
    if (out != "") File.append(out, path);
}

// Live display of the ROI Manager's ROIs on every image (no static overlay)
function showLive(ids) {
    for (k = 0; k < ids.length; k++) {
        selectImage(ids[k]);
        run("Remove Overlay");
        roiManager("Show All with labels");
    }
}

// Static overlay copy of the current ROI Manager ROIs on every image
function showOverlay(ids) {
    for (k = 0; k < ids.length; k++) {
        selectImage(ids[k]);
        roiManager("Show None");
        run("Remove Overlay");
        run("From ROI Manager");
        Overlay.drawLabels(true);
    }
}

// Open all files (display-only contrast), tile the windows, return the image IDs.
function openAll(paths) {
    ids = newArray(0);
    for (k = 0; k < paths.length; k++) {
        open(paths[k]);
        run("Enhance Contrast", "saturated=0.35");
        ids = Array.concat(ids, getImageID());
    }
    if (ids.length > 1) run("Tile");
    return ids;
}

function closeAll(ids) {
    for (k = 0; k < ids.length; k++) {
        selectImage(ids[k]);
        close();
    }
}

// ── shared helpers (identical in all three macros) ───────────
function argValue(argstr, key, defval) {
    parts = split(argstr, "|");
    for (k = 0; k < parts.length; k++) {
        p = parts[k];
        e = indexOf(p, "=");
        if (e > 0 && substring(p, 0, e) == key) return substring(p, e + 1);
    }
    return defval;
}

function normDir(d) {
    d = replace(d, "\\\\", "/");
    if (!endsWith(d, "/")) d = d + "/";
    return d;
}

function scan(base, rel, skip1, skip2) {
    names = getFileList(base + rel);
    for (k = 0; k < names.length; k++) {
        nm = names[k];
        if (startsWith(nm, ".")) continue;
        if (endsWith(nm, "/")) {
            d = substring(nm, 0, lengthOf(nm) - 1);
            if (rel == "" && (d == skip1 || d == skip2)) continue;
            scan(base, rel + nm, skip1, skip2);
        } else {
            lnm = toLowerCase(nm);
            if (endsWith(lnm, ".tif") || endsWith(lnm, ".tiff"))
                found = Array.concat(found, rel + nm);
        }
    }
}

function applyOnly(list, onlyfile) {
    if (onlyfile == "") return list;
    if (!File.exists(onlyfile)) exit("only_list file not found: " + onlyfile);
    wanted = split(File.openAsString(onlyfile), "\n");
    out = newArray(0);
    for (k = 0; k < list.length; k++) {
        for (j = 0; j < wanted.length; j++) {
            if (String.trim(replace(wanted[j], "\r", "")) == list[k]) {
                out = Array.concat(out, list[k]);
                j = wanted.length;
            }
        }
    }
    return out;
}
