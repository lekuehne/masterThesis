#!/usr/bin/env bash
# ==============================================================
#  run_pipeline.sh  –  launcher for the 3-step microfluidics pipeline
#
#  Usage:
#    ./run_pipeline.sh <step> [OPTIONS]
#
#  Steps:
#    align     1_align.ijm        interactive: reference-line alignment
#    bgsub     2_bg_subtract.ijm  automatic:   rolling-ball background subtraction
#    measure   3_measure_roi.ijm  interactive: ROI check + measurement -> CSV
#
#  Common options:
#    -f, --fiji       PATH  Fiji executable (default: auto-detect)
#    -i, --input      DIR   Base folder, e.g. /data/012-t3   (required)
#        --only       FILE  Only process files listed in FILE (relative paths,
#                           one per line), e.g. <input>/realign.txt
#        --overwrite        Redo everything. Default: outputs that already exist are
#                           omitted, so an interrupted run simply resumes.
#    -m, --macro-dir  DIR   Folder with the .ijm files [default: next to script]
#
#  align:
#    -R, --ref-image  FILE  Image on which the reference line is drawn
#                           [default: first image found]
#    -L, --ref-line   FILE  Saved straight-line .roi -> no drawing of a reference
#
#  bgsub:
#    -b, --bg-radius  NUM   Rolling-ball radius [default: 7]
#        --gui              Run with Fiji GUI instead of --headless
#
#  measure:
#    -r, --roi        FILE  RoiSet.zip (required)
#    -o, --output     FILE  Output CSV [default: <input>/results.csv]
#
#  Examples:
#    ./run_pipeline.sh align   -i /data/012-t3 -R /data/012-t3/ref/1_ch00.tif
#    ./run_pipeline.sh bgsub   -i /data/012-t3 -b 7
#    ./run_pipeline.sh measure -i /data/012-t3 -r /data/RoiSet.zip
#
#  Re-aligning flagged files (written by step 3 to <input>/realign.txt):
#    ./run_pipeline.sh align   -i /data/012-t3 --only /data/012-t3/realign.txt
#    ./run_pipeline.sh bgsub   -i /data/012-t3 --only /data/012-t3/realign.txt
#    ./run_pipeline.sh measure -i /data/012-t3 -r RoiSet.zip --only /data/012-t3/realign.txt -o redo.csv
# ==============================================================

set -euo pipefail

STEP=""; FIJI_BIN=""; INPUT_DIR=""; ROI_SET=""; OUTPUT_CSV=""
OVERWRITE=false; BG_RADIUS=7; REF_IMAGE=""; REF_LINE=""; ONLY_LIST=""; GUI=0
MACRO_DIR="$(dirname "$(realpath "$0")")"

RED='\033[0;31m'; GREEN='\033[0;32m'; CYAN='\033[0;36m'; BOLD='\033[1m'; RESET='\033[0m'
usage() { sed -n '3,50p' "$0" | sed 's/^#  \?//'; exit 0; }
error() { echo -e "${RED}ERROR: $*${RESET}" >&2; exit 1; }
info()  { echo -e "${CYAN}[INFO]${RESET} $*"; }
ok()    { echo -e "${GREEN}[ OK ]${RESET} $*"; }

[[ $# -eq 0 ]] && usage
case "$1" in
    align|bgsub|measure) STEP="$1"; shift ;;
    -h|--help) usage ;;
    *) error "First argument must be one of: align, bgsub, measure (see --help)" ;;
esac

while [[ $# -gt 0 ]]; do
    case "$1" in
        -f|--fiji)       FIJI_BIN="$2";   shift 2 ;;
        -i|--input)      INPUT_DIR="$2";  shift 2 ;;
        -r|--roi)        ROI_SET="$2";    shift 2 ;;
        -o|--output)     OUTPUT_CSV="$2"; shift 2 ;;
        -b|--bg-radius)  BG_RADIUS="$2";  shift 2 ;;
        -R|--ref-image)  REF_IMAGE="$2";  shift 2 ;;
        -L|--ref-line)   REF_LINE="$2";   shift 2 ;;
        --only)          ONLY_LIST="$2";  shift 2 ;;
        -m|--macro-dir)  MACRO_DIR="$2";  shift 2 ;;
        --gui)           GUI=1;           shift ;;
        --overwrite)     OVERWRITE=true;  shift ;;
        -h|--help)       usage ;;
        *) error "Unknown option: $1 (use --help)" ;;
    esac
done

# ── Fiji binary ───────────────────────────────────────────────
if [[ -z "$FIJI_BIN" ]]; then
    for candidate in \
        "$(command -v fiji 2>/dev/null || true)" \
        "$(command -v ImageJ 2>/dev/null || true)" \
        /opt/fiji/Fiji.app/ImageJ-linux64 \
        /Applications/Fiji.app/Contents/MacOS/ImageJ-macosx \
        "$HOME/Fiji.app/ImageJ-linux64" \
        "$HOME/Fiji.app/Contents/MacOS/ImageJ-macosx"
    do
        if [[ -n "$candidate" && -x "$candidate" ]]; then FIJI_BIN="$candidate"; break; fi
    done
fi
[[ -z "$FIJI_BIN" ]] && error "Fiji executable not found. Use --fiji /path/to/Fiji"
[[ -x "$FIJI_BIN" ]] || error "Fiji binary not executable: $FIJI_BIN"

# ── Validate / normalise ──────────────────────────────────────
[[ -z "$INPUT_DIR" ]] && error "Input directory required (--input)"
[[ -d "$INPUT_DIR" ]] || error "Input directory not found: $INPUT_DIR"
INPUT_DIR="$(realpath "$INPUT_DIR")/"

abs_file() { [[ -f "$1" ]] || error "$2 not found: $1"; realpath "$1"; }
[[ -n "$ONLY_LIST"  ]] && ONLY_LIST="$(abs_file "$ONLY_LIST" "--only file")"

case "$STEP" in
    align)
        MACRO="$MACRO_DIR/1_align.ijm"; SRC="$INPUT_DIR"
        [[ -n "$REF_IMAGE" ]] && REF_IMAGE="$(abs_file "$REF_IMAGE" "Reference image")"
        [[ -n "$REF_LINE"  ]] && REF_LINE="$(abs_file "$REF_LINE" "Reference line ROI")"
        ARG_STR="input_dir=${INPUT_DIR}|ref_image=${REF_IMAGE}|ref_line=${REF_LINE}|only_list=${ONLY_LIST}|overwrite=${OVERWRITE}|quit=true"
        FIJI_FLAGS=(--console)
        ;;
    bgsub)
        MACRO="$MACRO_DIR/2_bg_subtract.ijm"; SRC="${INPUT_DIR}aligned"
        ARG_STR="input_dir=${INPUT_DIR}|bg_radius=${BG_RADIUS}|only_list=${ONLY_LIST}|overwrite=${OVERWRITE}|quit=true"
        FIJI_FLAGS=(--console)
        [[ $GUI -eq 0 ]] && FIJI_FLAGS=(--headless)
        ;;
    measure)
        MACRO="$MACRO_DIR/3_measure_roi.ijm"; SRC="${INPUT_DIR}bg_subtracted"
        [[ -z "$ROI_SET" ]] && error "ROI set required for 'measure' (--roi)"
        ROI_SET="$(abs_file "$ROI_SET" "ROI set")"
        [[ -z "$OUTPUT_CSV" ]] && OUTPUT_CSV="${INPUT_DIR}results.csv"
        ARG_STR="input_dir=${INPUT_DIR}|roi_set=${ROI_SET}|output_csv=${OUTPUT_CSV}|only_list=${ONLY_LIST}|overwrite=${OVERWRITE}|quit=true"
        FIJI_FLAGS=(--console)
        ;;
esac

[[ -f "$MACRO" ]] || error "Macro not found: $MACRO"
[[ -d "$SRC"   ]] || error "Source folder not found: $SRC (previous step run?)"

# Count TIFFs (recursive; skip output folders when scanning the raw data)
N_TIFFS=$(find "$SRC" -type f \( -iname '*.tif' -o -iname '*.tiff' \) \
          -not -path '*/aligned/*' -not -path '*/bg_subtracted/*' | wc -l)
[[ "$N_TIFFS" -eq 0 && "$STEP" != "align" ]] && \
    N_TIFFS=$(find "$SRC" -type f \( -iname '*.tif' -o -iname '*.tiff' \) | wc -l)

echo ""
echo -e "${BOLD}=== Microfluidics pipeline: ${STEP} ===${RESET}"
echo -e "  Fiji   : ${CYAN}${FIJI_BIN}${RESET}"
echo -e "  Input  : ${CYAN}${INPUT_DIR}${RESET}"
echo -e "  Macro  : ${CYAN}${MACRO}${RESET}"
[[ -n "$ONLY_LIST" ]] && echo -e "  Only   : ${CYAN}${ONLY_LIST}${RESET}"
info "Found ${N_TIFFS} TIFF files below ${SRC}"
[[ "$N_TIFFS" -eq 0 ]] && error "No TIFF files found."
echo ""

# NOTE: 'align' and 'measure' need a GUI (use X forwarding or Xvfb on servers).
# Fiji returns exit code 0 even when the macro fails, so also scan its output.
LOG_FILE="$(mktemp)"
trap 'rm -f "$LOG_FILE"' EXIT
set +e
"$FIJI_BIN" "${FIJI_FLAGS[@]}" -macro "$MACRO" "$ARG_STR" 2>&1 | tee "$LOG_FILE"
EXIT_CODE=${PIPESTATUS[0]}
set -e
if [[ $EXIT_CODE -eq 0 ]] && grep -qE "Macro Error|Exception in thread" "$LOG_FILE"; then
    EXIT_CODE=1
fi

echo ""
[[ $EXIT_CODE -eq 0 ]] && ok "Step '${STEP}' finished." || error "Fiji exited with code ${EXIT_CODE}."
[[ "$STEP" == "measure" ]] && ok "Results: ${OUTPUT_CSV}"
exit 0
