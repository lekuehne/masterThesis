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
//  Args ("key=value|key=value"):
//    input_dir   base folder (e.g. .../012-t3)
//    roi_set     RoiSet.zip
//    output_csv  default <input_dir>/results.csv
//    only_list   (optional) text file with relative paths to measure
//    overwrite   true = start a new CSV (default: an existing CSV is loaded
//                and sets already in it are omitted -> resume after abort)
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

input_dir  = "/path/to/012-t3/";
roi_set    = "/path/to/RoiSet.zip";
output_csv = "";
only_list  = "";
do_quit    = false;
overwrite  = false;
KEY_NEXT   = "F2";       // hotkey: next ROI
KEY_PREV   = "F1";       // hotkey: previous ROI

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

// ── File name pattern: <exp>-t<time>_TileScan <no>_<chip>_ch<channel>.tif ─
// (?i) = ignore case. Chip = anything except "_" (so letters work too).
fn_pat  = "(?i)[0-9]+-t[0-9]+_tile ?scan[ _]?[0-9]+_[^_]+_ch[0-9]+\\.tiff?";
cap_pat = "(?i)^([0-9]+)-t([0-9]+)_tile ?scan[ _]?([0-9]+)_([^_]+)_ch([0-9]+)\\.tiff?$";

installHotkeys(KEY_NEXT, KEY_PREV);

print("=== STEP 3: ROI measurement ===");
print("Input     : " + bg_dir);
print("ROI set   : " + roi_set);
print("Output    : " + output_csv);
print("Files     : " + n);

run("Set Measurements...",
    "area mean standard min integrated median display redirect=None decimal=4");
run("Clear Results");

// ── Resume: load an existing CSV and omit sets that are already in it ─
resume = (!overwrite && only_list == "" && File.exists(output_csv));
if (only_list != "" && !overwrite && File.exists(output_csv))
    exit("Output file already exists: " + output_csv +
         "\nUse a different output (-o) for the --only run, or --overwrite.");
done_txt = "\n";
if (resume) {
    run("Results... ", "open=[" + output_csv + "]");
    n_old = nResults;
    for (r = 0; r < n_old; r++) {
        fn = getResultString("File", r);
        sb = getResultString("Subfolder", r);
        relold = fn;
        if (sb != "") relold = sb + "/" + fn;
        done_txt = done_txt + relold + "\n";
        // (re)build metadata columns from the file name; as text, so that
        // "012"/"00" survive and old 'NA' rows are repaired without re-measuring
        if (matches(fn, fn_pat)) {
            setResult("Experiment", r, replace(fn, cap_pat, "$1"));
            setResult("Timepoint",  r, replace(fn, cap_pat, "$2"));
            setResult("TileScan",   r, replace(fn, cap_pat, "$3"));
            setResult("Chip",       r, replace(fn, cap_pat, "$4"));
            setResult("Channel",    r, replace(fn, cap_pat, "$5"));
        }
    }
    updateResults();
    print("Resuming: " + n_old + " existing rows loaded from " + output_csv);
}

// ── Group channel files: same folder + same name except _chNN ─
keys = newArray(n);
for (i = 0; i < n; i++)
    keys[i] = replace(files[i], "(?i)_ch[0-9]+\\.tiff?$", "");

done = newArray(n);
n_sets = 0;
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
        for (m = 0; m < members.length; m++) {
            rel   = members[m];
            fname = File.getName(rel);
            sub   = "";
            if (indexOf(rel, "/") >= 0) sub = substring(rel, 0, lastIndexOf(rel, "/"));

            // Metadata from the file name (all stored as text: keeps "012", "00", "PB")
            experiment = "NA";  tp = "NA";  tilescan = "NA";  chip = "NA";  channel = "NA";
            if (matches(fname, fn_pat)) {
                experiment = replace(fname, cap_pat, "$1");
                tp         = replace(fname, cap_pat, "$2");
                tilescan   = replace(fname, cap_pat, "$3");
                chip       = replace(fname, cap_pat, "$4");
                channel    = replace(fname, cap_pat, "$5");
            } else {
                print("  WARNING: file name does not match <exp>-t<n>_TileScan <n>_<chip>_ch<nn>.tif : " + fname);
            }

            selectImage(ids[m]);
            run("Select None");
            first = nResults;
            roiManager("Deselect");
            roiManager("Measure");
            for (r = first; r < nResults; r++) {
                setResult("File", r, fname);
                setResult("Subfolder", r, sub);
                setResult("ROI_index", r, r - first + 1);
                setResult("Experiment", r, experiment);
                setResult("Timepoint", r, tp);
                setResult("TileScan", r, tilescan);
                setResult("Chip", r, chip);
                setResult("Channel", r, channel);
            }
            print("[set " + n_sets + "] measured " + (nResults - first) + " ROIs: " + rel);
        }
        updateResults();
        saveAs("Results", output_csv);               // save after every set (safe against crashes)
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

updateResults();
if (nResults > 0) saveAs("Results", output_csv);
print("");
print("=== Step 3 complete: " + nResults + " rows saved to " + output_csv + " ===");
if (File.exists(realign_file)) print("Files flagged for re-alignment: " + realign_file);
if (do_quit) { run("Close All"); run("Clear Results"); run("Quit"); }

// Write a small macro file with the hotkeys and install it (Plugins > Macros > Install)
function installHotkeys(kn, kp) {
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
        "  run(\"To Selection\");\n" +
        "  run(\"Out [-]\");\n" +
        "  run(\"Out [-]\");\n" +
        "  selectWindow(getTitle());\n" +
        "  showStatus(\"ROI \" + (idx + 1) + \" / \" + n);\n" +
        "}\n";
    path = getDirectory("temp") + "roi_hotkeys.ijm";
    File.saveString(t, path);
    run("Install...", "install=[" + path + "]");
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
