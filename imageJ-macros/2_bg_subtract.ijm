// ============================================================
//  2_bg_subtract.ijm  –  STEP 2/3: rolling-ball background subtraction
//  Fully automated (can run headless).
//
//  Input : <input_dir>/aligned/**/*.tif
//  Output: <input_dir>/bg_subtracted/<same sub-folder structure>/*.tif
//
//  Args ("key=value|key=value"):
//    input_dir  base folder (e.g. .../012-t3)
//    bg_radius  rolling-ball radius in pixels (default 7)
//    only_list  (optional) text file with relative paths to process
//    overwrite  true = redo all files (default: existing outputs are omitted)
//    quit       true = quit Fiji when done
// ============================================================

var found = newArray(0);

input_dir = "/path/to/012-t3/";
bg_radius = 7;
only_list = "";
do_quit   = false;
overwrite = false;

arg = getArgument();
if (arg != "") {
    input_dir = argValue(arg, "input_dir", input_dir);
    bg_arg    = argValue(arg, "bg_radius", "7");
    bg_radius = parseFloat(bg_arg);
    only_list = argValue(arg, "only_list", only_list);
    quit_arg = argValue(arg, "quit", "false");
    do_quit   = (quit_arg == "true");
    ow_arg    = argValue(arg, "overwrite", "false");
    overwrite = (ow_arg == "true");
}
// files listed in only_list are always redone; otherwise existing outputs are omitted
force = (overwrite || only_list != "");

input_dir   = normDir(input_dir);
aligned_dir = input_dir + "aligned/";
bg_dir      = input_dir + "bg_subtracted/";

if (!File.exists(aligned_dir)) exit("Run step 1 first – not found: " + aligned_dir);
scan(aligned_dir, "", "", "");
files = applyOnly(found, only_list);
n = files.length;
if (n == 0) exit("No TIFF files found in: " + aligned_dir);

print("=== STEP 2: Background subtraction (rolling ball r=" + bg_radius + ") ===");
print("Input : " + aligned_dir);
print("Output: " + bg_dir);
print("Files : " + n);

if (!File.exists(bg_dir)) File.makeDirectory(bg_dir);
setBatchMode(true);

n_omitted = 0;
for (i = 0; i < n; i++) {
    rel = files[i];
    if (!force && File.exists(bg_dir + rel)) {
        n_omitted++;
        print("[" + (i+1) + "/" + n + "] already done, omitted: " + rel);
        continue;
    }
    ensureParentDirs(bg_dir, rel);
    open(aligned_dir + rel);
    run("Subtract Background...", "rolling=" + bg_radius);
    saveAs("Tiff", bg_dir + rel);
    close();
    print("[" + (i+1) + "/" + n + "] " + rel);
}

setBatchMode(false);
print("=== Step 2 complete: " + (n - n_omitted) + " processed, " + n_omitted + " already done and omitted ===");
if (do_quit) run("Quit");

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
            ln = toLowerCase(nm);
            if (endsWith(ln, ".tif") || endsWith(ln, ".tiff"))
                found = Array.concat(found, rel + nm);
        }
    }
}

function ensureParentDirs(root, relfile) {
    parts = split(relfile, "/");
    path = root;
    for (k = 0; k < parts.length - 1; k++) {
        path = path + parts[k] + "/";
        if (!File.exists(path)) File.makeDirectory(path);
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
