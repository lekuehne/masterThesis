// ============================================================
//  1_align.ijm  –  STEP 1/3: alignment by reference line
//
//  One reference line (drawn once, or loaded from a .roi file) defines
//  the target orientation/position. For every sample you draw a line
//  along the same structural edge; the image is rotated + translated
//  so that its line lies on top of the reference line.
//
//  CHANNEL SETS: files that differ only by the channel ending
//  (7_ch00.tif, 7_ch01.tif, ... in the same folder) are one sample.
//  They are opened together; draw the line on whichever channel shows
//  the structure best and it is applied to ALL channels of the set.
//
//  Skipping: click OK without a line, then choose 'Skip this file'
//            (skips the whole channel set).
//            Skipped files are not written; they are listed in
//            <input_dir>/aligned/skipped.txt
//
//  Input : <input_dir>/**/*.tif   (recursive; "aligned" and
//          "bg_subtracted" sub-folders are ignored)
//  Output: <input_dir>/aligned/<same sub-folder structure>/*.tif
//          <input_dir>/aligned/reference_line.roi
//
//  Args ("key=value|key=value"):
//    input_dir  base folder (e.g. .../012-t3)
//    ref_image  (optional) image on which the reference line is drawn.
//               Default: first image found.
//    ref_line   (optional) saved .roi (straight line) -> no drawing needed
//    only_list  (optional) text file with relative paths, one per line;
//               only those files are processed (for re-alignment)
//    overwrite  true = redo everything and draw a new reference line.
//               Default: sets whose output already exists are omitted and
//               an existing aligned/reference_line.roi is re-used.
//    quit       true = quit Fiji when done
// ============================================================

var found = newArray(0);

// ── Arguments (defaults are for interactive use in Fiji) ─────
input_dir = "/path/to/012-t3/";
ref_image = "";
ref_line  = "";
only_list = "";
do_quit   = false;
overwrite = false;

arg = getArgument();
if (arg != "") {
    input_dir = argValue(arg, "input_dir", input_dir);
    ref_image = argValue(arg, "ref_image", ref_image);
    ref_line  = argValue(arg, "ref_line",  ref_line);
    only_list = argValue(arg, "only_list", only_list);
    quit_arg = argValue(arg, "quit", "false");
    do_quit   = (quit_arg == "true");
    ow_arg    = argValue(arg, "overwrite", "false");
    overwrite = (ow_arg == "true");
}
// Explicitly listed files (only_list) are always redone; otherwise
// sets whose output already exists are omitted (resume after abort).
force = (overwrite || only_list != "");

input_dir   = normDir(input_dir);
aligned_dir = input_dir + "aligned/";

// ── Find files ───────────────────────────────────────────────
scan(input_dir, "", "aligned", "bg_subtracted");
files = applyOnly(found, only_list);
n = files.length;
if (n == 0) exit("No TIFF files found in: " + input_dir);

print("=== STEP 1: Alignment ===");
print("Input   : " + input_dir);
print("Files   : " + n);
print("Output  : " + aligned_dir);

if (!File.exists(aligned_dir)) File.makeDirectory(aligned_dir);

// ── Group channel files: same folder + same name except _chNN ─
keys = newArray(n);
for (i = 0; i < n; i++)
    keys[i] = replace(files[i], "(?i)_ch[0-9]+\\.tiff?$", "");

// ── Obtain reference line (rx1,ry1)-(rx2,ry2) ───────────────
ref_path = "";      // path of the reference image if it is part of the batch
ref_file = ref_line;
if (ref_file == "" && !overwrite && File.exists(aligned_dir + "reference_line.roi")) {
    ref_file = aligned_dir + "reference_line.roi";   // re-use earlier line (resume)
    print("Re-using existing reference line (use --overwrite to draw a new one)");
}

if (ref_file != "") {
    if (!File.exists(ref_file)) exit("Reference line file not found: " + ref_file);
    open(input_dir + files[0]);
    open(ref_file);                                  // restores the selection
    if (selectionType() != 5) exit("Reference ROI is not a straight line: " + ref_file);
    getLine(rx1, ry1, rx2, ry2, lw);
    close();
    if (ref_image != "") ref_path = ref_image;       // its set is copied unchanged
    print("Reference line loaded from: " + ref_file);
} else {
    if (ref_image == "") ref_image = input_dir + files[0];
    if (!File.exists(ref_image)) exit("Reference image not found: " + ref_image);
    ref_path = ref_image;

    // open the reference image together with its channel siblings
    ref_paths = newArray(ref_image);
    refrel = "";
    if (startsWith(ref_image, input_dir)) refrel = substring(ref_image, lengthOf(input_dir));
    ref_key = "";
    for (j = 0; j < n; j++) if (files[j] == refrel) ref_key = keys[j];
    if (ref_key != "") {
        for (j = 0; j < n; j++)
            if (keys[j] == ref_key && input_dir + files[j] != ref_image)
                ref_paths = Array.concat(ref_paths, input_dir + files[j]);
    }
    ids = openAll(ref_paths);
    idx = promptForLine("REFERENCE LINE",
        "Draw the REFERENCE line along one edge of the structure\n" +
        "(on the channel where it is best visible).\n" +
        "All other samples will be aligned to this line.", false, ids);
    selectImage(ids[idx]);
    getLine(rx1, ry1, rx2, ry2, lw);
    saveAs("Selection", aligned_dir + "reference_line.roi");
    closeAll(ids);
    print("Reference image: " + ref_image + "  (+ " + (ref_paths.length - 1) + " channel sibling(s))");
}
print("Reference line: (" + d2s(rx1,1) + "," + d2s(ry1,1) + ") -> (" + d2s(rx2,1) + "," + d2s(ry2,1) + ")");
print("");

// Fill empty corners after rotation with 0
run("Colors...", "foreground=white background=black selection=yellow");

// ── Align every channel set ──────────────────────────────────
done = newArray(n);
n_sets = 0;
n_omitted = 0;
skipped_txt = "";
if (File.exists(aligned_dir + "skipped.txt"))
    skipped_txt = "\n" + replace(File.openAsString(aligned_dir + "skipped.txt"), "\r", "") + "\n";
for (i = 0; i < n; i++) {
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

    // already done (output exists, or set was skipped by the user earlier)?
    if (!force) {
        all_done = true;
        for (m = 0; m < members.length; m++) {
            listed = indexOf(skipped_txt, "\n" + members[m] + "\n") >= 0;
            if (!File.exists(aligned_dir + members[m]) && !listed) all_done = false;
        }
        if (all_done) {
            n_omitted++;
            print("[set " + n_sets + "] already done, omitted: " + keys[i]);
            continue;
        }
    }
    for (m = 0; m < members.length; m++) ensureParentDirs(aligned_dir, members[m]);

    // set containing the reference image: copy unchanged
    is_ref = false;
    for (m = 0; m < members.length; m++)
        if (input_dir + members[m] == ref_path) is_ref = true;
    if (is_ref) {
        for (m = 0; m < members.length; m++)
            File.copy(input_dir + members[m], aligned_dir + members[m]);
        print("[set " + n_sets + "] reference set copied unchanged: " + keys[i]);
        continue;
    }

    // open all channels at once
    paths = newArray(members.length);
    list_txt = "";
    for (m = 0; m < members.length; m++) {
        paths[m] = input_dir + members[m];
        list_txt = list_txt + "\n   " + members[m];
    }
    ids = openAll(paths);

    idx = promptForLine("ALIGN set " + n_sets + ": " + keys[i],
        "Files in this set:" + list_txt + "\n\n" +
        "Draw a LINE along the SAME edge as in the reference image,\n" +
        "on the channel where it is best visible.\n" +
        "The line is applied to ALL channels of this set.", true, ids);

    if (idx < 0) {                                   // skipped: nothing is written
        closeAll(ids);
        for (m = 0; m < members.length; m++) File.append(members[m], aligned_dir + "skipped.txt");
        print("[set " + n_sets + "] SKIPPED: " + keys[i] + " (" + members.length + " file(s))");
        continue;
    }

    selectImage(ids[idx]);
    getLine(tx1, ty1, tx2, ty2, lw);
    print("[set " + n_sets + "] " + keys[i] + "  (line drawn on " + members[idx] + ")");

    for (m = 0; m < members.length; m++) {
        selectImage(ids[m]);
        alignToRef(rx1, ry1, rx2, ry2, tx1, ty1, tx2, ty2);
        saveAs("Tiff", aligned_dir + members[m]);
        close();
        print("    saved: " + members[m]);
    }
}

print("");
print("=== Step 1 complete: " + n + " file(s) in " + n_sets + " set(s), " + n_omitted + " already done and omitted ===");
if (do_quit) { run("Close All"); run("Quit"); }

// ============================================================
//  Functions
// ============================================================

// Open all files, apply display-only contrast, tile the windows.
// Returns the array of image IDs.
function openAll(paths) {
    ids = newArray(0);
    for (k = 0; k < paths.length; k++) {
        open(paths[k]);
        run("Enhance Contrast", "saturated=0.35");   // display only
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

// Index (in ids) of the first image carrying a straight-line selection, else -1.
function findLine(ids) {
    for (k = 0; k < ids.length; k++) {
        selectImage(ids[k]);
        if (selectionType() == 5) return k;          // 5 = straight line
    }
    return -1;
}

// Wait until the user has drawn a straight line on ANY of the open images.
// Returns the index of the image holding the line, or -1 if the user chose
// to skip the set (only possible if allowSkip is true).
function promptForLine(title, msg, allowSkip, ids) {
    setTool("line");
    extra = "";
    if (allowSkip) extra = "\n\nTo SKIP this set: click OK without drawing a line.";
    waitForUser(title, msg + extra + "\n\nClick OK when the line is in place.");
    idx = findLine(ids);
    while (idx < 0) {
        if (allowSkip) {
            Dialog.create("No straight line detected");
            Dialog.addMessage("There is no straight line on any image.\n" + title);
            Dialog.addChoice("Action:", newArray("Draw line again", "Skip this set"), "Draw line again");
            Dialog.show();
            if (Dialog.getChoice() == "Skip this set") return -1;
        }
        setTool("line");
        waitForUser("Draw a straight line",
            "Please draw a STRAIGHT LINE with the line tool, then click OK.");
        idx = findLine(ids);
    }
    return idx;
}

// Rigid alignment of the ACTIVE image: rotate so that its line (t) is
// parallel to the reference line (r), then translate so that the line
// midpoints coincide. Lines are treated as undirected.
function alignToRef(rx1, ry1, rx2, ry2, tx1, ty1, tx2, ty2) {
    rdx = rx2 - rx1;  rdy = ry2 - ry1;
    tdx = tx2 - tx1;  tdy = ty2 - ty1;
    if (rdx*tdx + rdy*tdy < 0) {               // drawn in opposite direction -> flip
        tmp = tx1; tx1 = tx2; tx2 = tmp;
        tmp = ty1; ty1 = ty2; ty2 = tmp;
        tdx = -tdx; tdy = -tdy;
    }
    // clockwise rotation (ImageJ convention, y axis points down)
    phi = (atan2(rdy, rdx) - atan2(tdy, tdx)) * 180 / PI;
    a   = phi * PI / 180;

    // Rotation centre used by ImageJ = (w-1)/2, (h-1)/2
    cx = (getWidth()  - 1) / 2;
    cy = (getHeight() - 1) / 2;
    mx = (tx1 + tx2) / 2 - cx;                 // target midpoint relative to centre
    my = (ty1 + ty2) / 2 - cy;
    nx = cx + cos(a)*mx - sin(a)*my;           // where that midpoint lands after rotation
    ny = cy + sin(a)*mx + cos(a)*my;
    // integer shift -> only ONE interpolation step (rotation) touches the data
    dx = round((rx1 + rx2) / 2 - nx);
    dy = round((ry1 + ry2) / 2 - ny);

    run("Select None");                        // rotate/translate the whole image
    if (abs(phi) > 0.001)
        run("Rotate... ", "angle=" + d2s(phi, 5) + " grid=1 interpolation=Bilinear");
    if (dx != 0 || dy != 0)
        run("Translate...", "x=" + dx + " y=" + dy + " interpolation=None");
    print("    rotation = " + d2s(phi, 3) + " deg, shift = (" + dx + ", " + dy + ") px");
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

// Recursively collect TIFFs (paths relative to base) into global 'found'
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
            ln = toLowerCase(nm);
            if (endsWith(ln, ".tif") || endsWith(ln, ".tiff"))
                found = Array.concat(found, rel + nm);
        }
    }
}

// Create all parent directories of 'relfile' below 'root'
function ensureParentDirs(root, relfile) {
    parts = split(relfile, "/");
    path = root;
    for (k = 0; k < parts.length - 1; k++) {
        path = path + parts[k] + "/";
        if (!File.exists(path)) File.makeDirectory(path);
    }
}

// Restrict list to entries named in a text file (one relative path per line)
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
