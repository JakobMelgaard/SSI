choices_plate0 <- c(paste0("Plate0",1:9),paste0("Plate",10:16),paste0("Plate",LETTERS[1:4]))
choices_plate <- vector()
for (i in 1:16){
    choices_plate <- if(i<10) c(choices_plate,paste0("P0",i," ",toString((i-1)*96+1),"-",toString(i*96))) else c(choices_plate,paste0("P",i," ",toString((i-1)*96+1),"-",toString(i*96)))
}
choices_plate <- c(choices_plate,paste0("Plate",LETTERS[1:4]))
choices_plate_switch <- choices_plate0
names(choices_plate_switch) <- choices_plate
choices_well <- sort(c(as.vector(outer(LETTERS[1:8],1:9,paste,sep="0")),as.vector(outer(LETTERS[1:8],10:12,paste0))))
dis <- c("None","ALD","ASL","Biot","CACT","CAH","CPT1","CPT2","CTD","GA1","GALT","HCU","IVA","LCHAD","MADD","MCAD","MCD","MLD","MMA_PA","MPS1","MSUD","OTC","PKU","Pompe","SCID","SMA","TT1","VLCAD","FAER")

# Work from the folder this script is in, so indexDB.xlsx is found there.
# (shiny::runApp() already does this when the app is started from RStudio; this also covers
# source("app.R") and Rscript.)
local({
    args <- commandArgs(trailingOnly = FALSE)
    f    <- sub("^--file=", "", args[grep("^--file=", args)])
    of   <- tryCatch(sys.frame(1)$ofile, error = function(e) NULL)
    path <- if (length(f) == 1 && nzchar(f)) f else of
    # (only if app.R is really in that folder: the launcher script lives in the temp folder, and shiny::runApp()
    # has already moved to the app's folder)
    if (is.character(path) && length(path) == 1 && file.exists(file.path(dirname(path), "app.R"))) {
        setwd(dirname(normalizePath(path)))
    }
})

# One entry per column, left to right. To add another column, add an entry to each of the
# nine vectors below.
# col_type: "text"   = free text ID like S001 (the number at the end counts up by one per row),
#           "select" = dropdown that copies the value from the nearest one you picked above,
#           "choice" = dropdown that never copies: starts at its default and only changes when you change it,
#           "copy"   = free text; rows you haven't typed in copy the nearest value you typed above,
#           "plain"  = free text, stays empty until you type something (no auto-fill),
#           "calc"   = read-only, calculated from the "plain" concentration column.
# How each dropdown column auto-fills the cells you haven't filled in yourself
# (only when a number of rows has been entered):
#   step 0 = copy the value from the nearest cell you filled in above
#   step 1 = take the next option in the list for every row further down (wraps around)
# In the names, _{x} is shown as subscript and ^{x} as superscript.
col_names       <- c("Sygdom", "Sample ID", "Plade ID", "DNA Konc. [ng/\u03bcl]", "Index Plade",
                     "Index Br\u00f8nd", "Lib. Konc. [ng/\u03bcl]", "Lib. Vol. [\u03bcl]", "ng")
col_type        <- c("select", "text", "copy", "plain", "select", "select", "plain", "calc", "plain")
col_choices     <- list(dis, character(0), character(0), character(0), choices_plate, choices_well,
                        character(0), character(0), character(0))
col_step        <- c(0, 0, 0, 0, 0, 1, 0, 0, 0)
col_placeholder <- c("", "", "", "", "", "", "", "", "")
col_default     <- c("", "", "", "", "", "", "", "", "500")   # starting value of a cell nobody has filled in yet
col_hidden      <- c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, TRUE)   # hidden until you press the button
K           <- length(col_choices)

max_rows <- 10000

options(shiny.maxRequestSize = 50 * 1024^2)   # allow sample ID files up to 50 MB

# Fixed values of the sample sheet (taken from the example 20260923_SampleSheet.csv)
file_format_version <- "2"
instrument_platform <- "NextSeq1k2k"
index1_cycles       <- "10"
index2_cycles       <- "10"
software_version    <- "4.2.11"
fastq_compression   <- "gzip"

# Format of the date in the second (IEM) sample sheet: "%d/%m/%Y" gives 06/10/2026
iem_date_format <- "%d/%m/%Y"

# The labels of the pool lines, in the order of the 11 values of a pool line (see "cells" in the JavaScript).
# Part 1 of the pool section is the first 7, part 2 is Pool ID again followed by the last 4.
# (_{x} is subscript and ^{x} superscript, see label_html.) Also used for the data file and the pool sheet.
pool_heads <- c("Pool ID", "Pool Konc. [ng/\u03bcl]", "Pool Konc. [nM]", "Vol. Pool [\u03bcl]", "Vol. Rsb [\u03bcl]",
                "Konc. seqklar [ng/\u03bcl]", "Konc. seqklar [nM]", "% af 24 \u03bcl", "Vol. af 24 \u03bcl",
                "Konc. pool [nM]", "Vol. fortynding [\u03bcl]")
# Standard values of a pool: V, c_target (for Vol. Pool and Vol. Rsb) and the maximum of Konc. pool [nM]
pool_default_v    <- 15
pool_default_ctar <- 1.8
pool_default_cmax <- 0.85

`%||%` <- function(a, b) if (is.null(a)) b else a

dated        <- function(suffix) paste0(format(Sys.Date(), "%Y%m%d"), suffix)          # 20261030_Pools.xlsx
blank_na     <- function(x) ifelse(is.na(x), "", as.character(x))                      # NA -> empty cell
dot_decimal  <- function(x) sub(",", ".", trimws(x), fixed = TRUE)                       # decimal comma -> decimal point
bad_number   <- function(v) is.null(v) || is.na(v) || !is.finite(v) || v <= 0            # not a number above 0
is_yes       <- function(x) tolower(trimws(x)) %in% c("yes", "ja", "1", "true")

# Names with _{sub} and ^{super}: as HTML (for the screen) and as plain text (for messages and Excel)
label_html <- function(x) {
    x <- gsub("\\^\\{([^}]*)\\}_\\{([^}]*)\\}", "<span class='dd-stack'><sup>\\1</sup><sub>\\2</sub></span>", x)
    x <- gsub("\\^\\{([^}]*)\\}", "<sup>\\1</sup>", x)
    gsub("_\\{([^}]*)\\}", "<sub>\\1</sub>", x)
}
label_plain <- function(x) {
    x <- gsub("\\^\\{([^}]*)\\}", "^\\1", x)
    gsub("_\\{([^}]*)\\}", "_\\1", x)
}
col_names_plain <- label_plain(col_names)

# Values for every row of one column: dropdowns the user picked (user == TRUE) keep their
# value, all others are derived from the nearest user-picked dropdown above them.
derive <- function(vals, user, ch, step, default = "") {
    n <- length(vals)
    if (n == 0) return(vals)
    last <- cummax(ifelse(user, seq_len(n), 0L))          # row of nearest picked cell at or above
    base <- match(vals[pmax(last, 1L)], ch)                # its position in the option list
    idx  <- ((base - 1 + step * (seq_len(n) - last)) %% length(ch)) + 1
    out  <- ch[idx]
    out[last == 0 | is.na(out)] <- default
    out[user] <- vals[user]
    out
}

# Same idea for the text column. The ID is "some text followed by a number", e.g. S001 or AB023.
# Rows below a typed ID keep the text and count up by one per row, keeping the leading zeros
# (S001, S002, ... S099, S100). An ID that doesn't end in a number is simply copied.
derive_id <- function(vals, user, from_file = NULL) {
    n <- length(vals)
    if (n == 0) return(vals)
    last   <- cummax(ifelse(user, seq_len(n), 0L))
    base   <- vals[pmax(last, 1L)]
    prefix <- sub("[0-9]+$", "", base)
    digits <- substring(base, nchar(prefix) + 1L)
    ok     <- digits != "" & nchar(digits) <= 15
    num    <- suppressWarnings(as.numeric(digits)) + (seq_len(n) - last)
    s      <- sprintf("%.0f", num)
    padded <- paste0(strrep("0", pmax(0L, nchar(digits) - nchar(s))), s)
    out    <- ifelse(ok, paste0(prefix, padded), base)
    out[last == 0] <- ""
    out[user] <- vals[user]
    # IDs loaded from a file keep their value, but the rows below don't count on from them
    if (!is.null(from_file)) out[from_file] <- vals[from_file]
    out
}

# Written field that copies: rows you haven't typed in take the nearest value you typed above them
# (empty if there is none).
derive_copy <- function(vals, user) {
    n <- length(vals)
    if (n == 0) return(vals)
    last <- cummax(ifelse(user, seq_len(n), 0L))
    out  <- vals[pmax(last, 1L)]
    out[last == 0] <- ""
    out[user] <- vals[user]
    out
}

# The read-only "\u03bcl" column: d / c, rounded up to 1 decimal, where c is the number in the
# concentration column and d is the 500 or 350 chosen in the last column (c = 132, d = 500 gives 3.8).
# (A comma is accepted as decimal mark. Empty, non-numeric or zero/negative -> empty.)
calc_from <- function(conc, divisor) {
    x   <- suppressWarnings(as.numeric(sub(",", ".", trimws(conc), fixed = TRUE)))
    d   <- suppressWarnings(as.numeric(divisor))
    ok  <- !is.na(x) & is.finite(x) & x > 0 & !is.na(d)
    out <- rep("", length(conc))
    out[ok] <- sprintf("%.1f", ceiling(round(d[ok] / x[ok] * 10, 9)) / 10)
    out
}

# The values of column c for every row, with the auto-fill that goes with its type
# (the "calc" column is not handled here: see calc_from)
derive_col <- function(c, vals, user, from_file = NULL) {
    switch(col_type[c],
           text   = derive_id(vals, user, from_file),
           select = derive(vals, user, col_choices[[c]], col_step[c], col_default[c]),
           copy   = derive_copy(vals, user),
           plain  = vals,                 # no auto-fill: stays as typed
           choice = vals)                 # dropdown without auto-fill: stays as chosen
}

# ---- Index database ------------------------------------------------------------------------
# indexDB.xlsx must be in the same folder as app.R (needs the readxl package).
getIndexDB <- function(){
    readxl::read_xlsx("indexDB.xlsx")
}

findIndex <- function(plate,well,ind=NULL,cols=c(7,5)){
    ind <- if(is.null(ind)) getIndexDB() else ind
    combined <- paste(choices_plate_switch[[plate]],well,sep="_")
    x <- ind[ind$Combined == combined,cols]
    return(c(x[[1]],x[[2]]))
}

# Read the file once when the app starts, not once per row
index_db <- tryCatch(getIndexDB(), error = function(e) {
    message("Could not read indexDB.xlsx: ", conditionMessage(e))
    NULL
})

# All well names, in the order the rows are labelled: "A01", "A02", ..., "H12".
# Replace this line with your own vector of well names.
well_names <- sprintf("%s%02d", rep(LETTERS[1:8], each = 12), 1:12)

# Row label like in the table: well_names[1], well_names[2], ... (starts over after the last one)
well_label <- function(i) well_names[((i - 1) %% length(well_names)) + 1]

# ---- Sample IDs from a file ----------------------------------------------------------------
# (needs the xml2 package)
getPunched <- function(path="NGS11092026001.Recovered.p9.xml"){
    punch <- xml2::read_xml(path)
    barcode <- xml2::xml_find_all(punch,"//Well[WellType/Type!='Empty']/SampleBarcode")
    code <- xml2::xml_find_first(punch,"//Barcode/Code")
    list("sampleID" = xml2::xml_text(barcode), "pladeID" = xml2::xml_text(code))
}

# A sample ID file can hold at most this many sample IDs (one 96-well plate)
max_file_ids <- 96

# Which of the n sample IDs of a file to use. `text` is "all", or numbers like "2", "2-5", "2,4,6"
# or "2-5,7,9-11". Returns list(ok = TRUE, idx = positions) or list(ok = FALSE, msg = what is wrong).
parse_selection <- function(text, n) {
    t <- tolower(gsub("\\s+", "", text %||% ""))
    if (t == "") return(list(ok = FALSE, msg = "Write all, or which sample IDs you want, for example \"2-5,7\"."))
    if (t %in% c("all", "alle")) return(list(ok = TRUE, idx = seq_len(n)))
    idx <- integer()
    for (p in strsplit(t, ",", fixed = TRUE)[[1]]) {
        if (grepl("^[0-9]+$", p)) {
            a <- b <- suppressWarnings(as.integer(p))
        } else if (grepl("^[0-9]+-[0-9]+$", p)) {
            ab <- suppressWarnings(as.integer(strsplit(p, "-", fixed = TRUE)[[1]]))
            a <- ab[1]; b <- ab[2]
        } else {
            return(list(ok = FALSE, msg = paste0("Can't read '", p, "'. Use for example \"2\", \"2-5\", \"2,4,6\" or \"2-5,7,9-11\".")))
        }
        if (is.na(a) || is.na(b) || a < 1 || b > n || a > b) {
            return(list(ok = FALSE, msg = paste0("'", p, "' doesn't fit: the file has ", n, " sample IDs (1-", n, ").")))
        }
        idx <- c(idx, a:b)
    }
    list(ok = TRUE, idx = unique(idx))
}

# Which columns of the grid hold what
screen_col <- 1   # Screeningspositiv for
sample_col <- 2   # Sample ID (text)
pladeid_col <- 3  # Plade ID (typed by hand, copies the value above, not required)
dnakonc_col <- 4  # DNA konc. (typed by hand, goes into the concentration sheet)
plate_col  <- 5   # IDT - Index Plade
well_col   <- 6   # Index Br\u00f8nd
conc_col   <- 7   # Concentration ng/\u03bcl (typed by hand)
calc_col   <- 8   # \u03bcl (calculated)
div_col    <- 9   # 500 by default, the number the concentration is divided into (typed by hand)
required_cols <- c(sample_col, plate_col, well_col)   # a row counts as filled in when these are

# Look up the two index values for every row with findIndex(). Each plate/well combination
# is only looked up once. Rows with an empty plate or well, or without exactly one match in
# the file, get NA (an empty cell in the CSV).
lookup_indexes <- function(plate, well, cols) {
    n    <- length(plate)
    out1 <- rep(NA, n)
    out2 <- rep(NA, n)
    have <- plate != "" & well != ""
    if (is.null(index_db) || !any(have)) return(list(out1, out2, have))
    
    pairs <- unique(data.frame(plate = plate[have], well = well[have], stringsAsFactors = FALSE))
    res   <- lapply(seq_len(nrow(pairs)), function(j) {
        r <- tryCatch(findIndex(pairs$plate[j], pairs$well[j], ind = index_db, cols = cols),
                      error = function(e) NULL)
        if (length(r) == 2) r else c(NA, NA)
    })
    first  <- unlist(lapply(res, `[`, 1))
    second <- unlist(lapply(res, `[`, 2))
    
    at <- match(paste(plate[have], well[have], sep = "\t"),
                paste(pairs$plate, pairs$well, sep = "\t"))
    out1[have] <- first[at]
    out2[have] <- second[at]
    list(out1, out2, have)
}

# ---- Sample sheet ---------------------------------------------------------------------------
# Everything above the sample rows, exactly like the example (three columns per line).
# run_name and read_cycles come from the app; the rest is fixed (see the top of the file).
sheet_header <- function(run_name, read_cycles) {
    c("[Header],,",
      paste0("FileFormatVersion,", file_format_version, ","),
      paste0("RunName,", run_name, ","),
      paste0("InstrumentPlatform,", instrument_platform, ","),
      ",,",
      "[Reads],,",
      paste0("Read1Cycles,", read_cycles, ","),
      paste0("Read2Cycles,", read_cycles, ","),
      paste0("Index1Cycles,", index1_cycles, ","),
      paste0("Index2Cycles,", index2_cycles, ","),
      ",,",
      "[BCLConvert_Settings],,",
      paste0("SoftwareVersion,", software_version, ","),
      paste0("FastqCompressionFormat,", fastq_compression, ","),
      ",,",
      "[BCLConvert_Data],,",
      "Sample_ID,Index,Index2")
}

# The second sample sheet (IEM format, comma separated). Everything above the sample rows is
# fixed like in the example Book_2_Samplesheet_v1_.csv, except the date.
iem_header <- function(date_text) {
    c("[Header],,,,,,,,,,",
      "IEMFileVersion,5,,,,,,,,,",
      "Investigator Name,NeoGen,,,,,,,,,",
      "Experiment Name,NBS-NGS,,,,,,,,,",
      paste0("Date,", date_text, ",,,,,,,,,"),
      "Workflow,GenerateFASTQ,,,,,,,,,",
      "Application,NextSeq FASTQ Only,,,,,,,,,",
      "Instrument Type,NextSeq/MiniSeq,,,,,,,,,",
      "Assay,IDT xGEN v2,,,,,,,,,",
      "Index Adapters,IDT_Ilmn DNA_RNA UD Indexes SetA Tagmentation,,,,,,,,,",
      "Description,Sequencing of Screen positive kids,,,,,,,,,",
      "Chemistry,Amplicon,,,,,,,,,",
      ",,,,,,,,,,",
      "[Reads],,,,,,,,,,",
      "151,,,,,,,,,,",
      "151,,,,,,,,,,",
      ",,,,,,,,,,",
      "[Settings],,,,,,,,,,",
      "Adapter,CTGTCTCTTATACACATCT,,,,,,,,,",
      ",,,,,,,,,,",
      "[Data],,,,,,,,,,",
      "Sample_ID,Sample_Name,Sample_Plate,Sample_Well,Index_Plate_ID,Index_Plate_Well,index,index2,Sample_Project,Description,Lane")
}

# ---- Client side -------------------------------------------------------------------------
# The dropdowns are plain <select> elements built in the browser (fast, no per-dropdown
# Shiny inputs). The browser only tells R which cell the user changed, and R only sends back
# the cells whose value actually needs to change.
js_code <- r"---(
(function () {
  var CH = window.DD_CHOICES;
  var TYPES = [].concat(window.DD_TYPES);
  var WELLS = [].concat(window.DD_WELLS);
  var PH = [].concat(window.DD_PLACEHOLDERS);
  var HIDDEN = [].concat(window.DD_HIDDEN);
  var GONE = [].concat(window.DD_GONE);          // columns that are never shown (their value is changed in a prompt)
  var K = CH.length;
  var tpl = [];
  var sels = [];
  var chks = [];
  var gcells = [];
  var POOLS = [];
  var CALC_COL = window.DD_CALC_COL;
  var modified = [];                       // modified[r - 1]: the calculation of row r was changed in the prompt

  function arr(x) { return (x === undefined || x === null) ? [] : [].concat(x); }

  // Row label: the r-th well name (starts over after the last one)
  function wellLabel(r) {
    return WELLS[(r - 1) % WELLS.length];
  }

  function resetSels() {
    sels = [null];
    modified = [];
    chks = [];
    gcells = [];
    for (var c = 1; c <= K; c++) sels.push([]);
  }

  function makeTemplate(col) {
    var type = TYPES[col - 1];
    if (type === 'text' || type === 'plain' || type === 'copy') {
      var inp = document.createElement('input');
      inp.type = 'text';
      inp.className = 'form-control dd-sel';
      inp.autocomplete = 'off';
      inp.placeholder = PH[col - 1] || '';
      return inp;
    }
    if (type === 'calc') {              // read-only: you can't type in it
      var calc = document.createElement('input');
      calc.type = 'text';
      calc.className = 'form-control dd-calc';
      calc.readOnly = true;
      calc.tabIndex = -1;
      return calc;
    }
    var s = document.createElement('select');
    s.className = 'form-control dd-sel';
    var o = document.createElement('option');
    o.value = ''; o.textContent = 'Choose...';
    s.appendChild(o);
    arr(CH[col - 1]).forEach(function (c) {
      var p = document.createElement('option');
      p.value = c; p.textContent = c;
      s.appendChild(p);
    });
    return s;
  }

  function setup() {
    tpl = [null];
    for (var c = 1; c <= K; c++) tpl.push(makeTemplate(c));
    resetSels();

    // One listener for every dropdown
    document.addEventListener('change', function (e) {
      var t = e.target;
      if (!t.classList || !t.classList.contains('dd-sel')) return;
      var val = t.value;
      if (t.tagName === 'INPUT') { val = val.trim(); t.value = val; }
      Shiny.setInputValue('cell_change',
        { col: +t.dataset.col, row: +t.dataset.row, value: val, nonce: Math.random() },
        { priority: 'event' });
    });

    // Show / hide the concentration columns, "Samlet pool" and the concentration download button
    // (the button sits above the column labels)
    document.addEventListener('click', function (e) {
      var b = e.target.closest ? e.target.closest('#dd-toggle') : null;
      if (!b) return;
      var hidden = document.getElementById('dd-root').classList.toggle('dd-hide-extra');
      b.textContent = hidden ? 'Vis lib. konc. kolonner' : 'Skjul lib. konc. kolonner';
    });

    // Clicking a calculated (\u03bcl) cell opens a prompt where its calculation can be changed
    document.addEventListener('click', function (e) {
      var t = e.target;
      if (!t.classList || !t.classList.contains('dd-calc') || !t.dataset.row || !t.closest('#dd-rows')) return;
      Shiny.setInputValue('calc_click', { row: +t.dataset.row, nonce: Math.random() }, { priority: 'event' });
    });

    // Show / hide the pooling (checkboxes, Pool column, pool panel and the "Pool selected rows" button)
    document.addEventListener('click', function (e) {
      var b = e.target.closest ? e.target.closest('#dd-pool-toggle') : null;
      if (!b) return;
      var hidden = document.getElementById('dd-root').classList.toggle('dd-hide-pool');
      b.textContent = hidden ? 'Vis pooling' : 'Skjul pooling';
    });

    // The message below a download button (the buttons can always be pressed)
    Shiny.addCustomMessageHandler('sheetState', function (m) {
      var t = document.getElementById(m.id + '_msg');
      if (t) t.textContent = m.msg || '';
    });

    Shiny.addCustomMessageHandler('addRows', function (m) {
      var box = document.getElementById('dd-rows');
      var vs = arr(m.v).map(arr);          // vs[col - 1][k] = value for new row k
      var count = vs[0].length;
      var frag = document.createDocumentFragment();
      for (var k = 0; k < count; k++) {
        var r = m.start + k;
        var row = document.createElement('div');
        row.className = 'dd-row';
        var chk = document.createElement('input');
        chk.type = 'checkbox';
        chk.className = 'dd-chk dd-pool';
        row.appendChild(chk);
        chks[r - 1] = chk;
        var gc = document.createElement('span');
        gc.className = 'dd-grpcell dd-pool';
        row.appendChild(gc);
        gcells[r - 1] = gc;
        var num = document.createElement('span');
        num.className = 'dd-num';
        num.textContent = wellLabel(r);
        row.appendChild(num);
        for (var c = 1; c <= K; c++) {
          var s = tpl[c].cloneNode(true);
          s.dataset.col = c;
          s.dataset.row = r;
          if (HIDDEN[c - 1]) s.classList.add('dd-extra');
          if (GONE[c - 1]) s.classList.add('dd-gone');
          s.value = vs[c - 1][k];
          row.appendChild(s);
          sels[c][r - 1] = s;
        }
        frag.appendChild(row);
      }
      box.appendChild(frag);
    });

    Shiny.addCustomMessageHandler('setCells', function (m) {
      var idx = arr(m.idx), val = arr(m.val);
      for (var k = 0; k < idx.length; k++) {
        var v = val[k];
        if (m.col === CALC_COL && modified[idx[k] - 1] && v !== '') v += '*';   // changed calculation: marked with *
        sels[m.col][idx[k] - 1].value = v;
      }
    });

    // The note under the column of the calculated cells: shown while any calculation has been changed
    function updateCalcNote() {
      var note = document.getElementById('dd-calcnote-text');
      if (!note) return;
      note.textContent = modified.some(function (x) { return x; }) ? '* m [ng] \u00e6ndret' : '';
      var wrap = document.getElementById('dd-wrap'), foot = document.getElementById('dd-calcnote');
      var gap = wrap ? (wrap.offsetWidth - wrap.clientWidth + 6) : 0;   // scrollbar + padding of the table
      if (foot) foot.style.paddingRight = gap + 'px';                  // same width as the table above
      var hd = document.querySelector('.dd-head');
      if (hd) hd.style.paddingRight = gap + 'px';                      // so the labels stay above their columns
    }
    function buildCalcNote() {                 // a line under the table with one slot per column, like the rows
      var wrap = document.getElementById('dd-wrap');
      if (!wrap || document.getElementById('dd-calcnote')) return;
      var foot = document.createElement('div');
      foot.id = 'dd-calcnote';
      foot.className = 'dd-row';
      function slot(cls) { var x = document.createElement('span'); x.className = cls; foot.appendChild(x); return x; }
      slot('dd-chk dd-pool'); slot('dd-grpcell dd-pool'); slot('dd-num');
      for (var c = 1; c <= K; c++) {
        var cell = slot('dd-calcnote-cell' + (HIDDEN[c - 1] ? ' dd-extra' : '') + (GONE[c - 1] ? ' dd-gone' : ''));
        if (c === CALC_COL) cell.id = 'dd-calcnote-text';
      }
      wrap.parentNode.insertBefore(foot, wrap.nextSibling);
      updateCalcNote();
    }
    if (document.readyState === 'loading') document.addEventListener('DOMContentLoaded', buildCalcNote);
    else buildCalcNote();
    // The table (the part without the pool section) reaches down to the bottom of the sidebar
    function fitWrap() {
      var wrap = document.getElementById('dd-wrap'), side = document.querySelector('#dd-root form.well');
      if (!wrap || !side) return;
      var foot = document.getElementById('dd-calcnote');
      var room = side.getBoundingClientRect().bottom - wrap.getBoundingClientRect().top - (foot ? foot.offsetHeight + 4 : 0);
      var h = Math.max(120, Math.floor(room)) + 'px';
      if (wrap.style.height !== h) { wrap.style.height = h; wrap.style.maxHeight = h; }
      // The table can reach below the bottom of the window (the sidebar is longer than the window). So that its
      // last rows can still be scrolled into view with its own scrollbar, there is that much room below the rows.
      var below = wrap.getBoundingClientRect().bottom + window.pageYOffset + (foot ? foot.offsetHeight + 4 : 0) - window.innerHeight;
      var rowsBox = document.getElementById('dd-rows');
      var pad = Math.max(0, Math.ceil(below)) + 'px';
      if (rowsBox && rowsBox.style.paddingBottom !== pad) rowsBox.style.paddingBottom = pad;
    }
    window.addEventListener('resize', function () { updateCalcNote(); fitWrap(); });
    if (window.ResizeObserver) {                // the scrollbar appears and disappears as rows are added
      var ro = new ResizeObserver(function () { updateCalcNote(); });
      var ro2 = new ResizeObserver(fitWrap);    // the sidebar or the pool section above the table changes size
      var watch = function () {
        ['dd-wrap', 'dd-rows'].forEach(function (id) { var e = document.getElementById(id); if (e) ro.observe(e); });
        ['dd-pools', 'dd-grid'].forEach(function (id) { var e = document.getElementById(id); if (e) ro2.observe(e); });
        var side = document.querySelector('#dd-root form.well');
        if (side) ro2.observe(side);
        fitWrap();
      };
      if (document.readyState === 'loading') document.addEventListener('DOMContentLoaded', watch); else watch();
    }

    // From R: make cells red (m.cols: column numbers, m.rows: row numbers; nothing in rows = no red cells)
    Shiny.addCustomMessageHandler('markBad', function (m) {
      var cols = arr(m.cols), rows = arr(m.rows);
      cols.forEach(function (c) {
        (sels[c] || []).forEach(function (el, i) { if (el) el.classList.toggle('dd-bad', rows.indexOf(i + 1) >= 0); });
      });
    });
    // From R: make one input of the page red (the cartridge cycles)
    Shiny.addCustomMessageHandler('setBad', function (m) {
      var el = document.getElementById(m.id);
      if (el) el.classList.toggle('dd-bad', !!m.bad);
    });

    // Runs change(pool) on pool m.idx, and on all pools below it too when m.below ("for alle under")
    function applyToPools(m, change) {
      for (var q = m.idx; q < (m.below ? POOLS.length : m.idx + 1); q++) change(POOLS[q]);
    }

    // From the prompt: X and Y of one pool's Pool / Rsb calculation
    Shiny.addCustomMessageHandler('setPoolXY', function (m) {
      var g = POOLS[m.idx];
      if (!g) return;
      applyToPools(m, function (g) { g.x = m.x; g.y = m.y; });
      renderPools();                           // redraws the pool lines (and shows/hides the note)
      sendPools();
    });

    // From the prompt: the maximum of Konc. pool [nM] (this pool, or this pool and all below)
    Shiny.addCustomMessageHandler('setPoolCmax', function (m) {
      if (!POOLS[m.idx]) return;
      applyToPools(m, function (g) { g.cmax = m.cmax; });
      renderPools();
      sendPools();
    });

    // From a loaded data file: replace all pools
    Shiny.addCustomMessageHandler('setPools', function (m) {
      var fresh = arr(m.pools).map(function (p) {
        return { rows: arr(p.rows), name: p.name || '', nameUser: !!p.nameUser, text: p.text || '', text2: p.text2 || '',
                 pct: p.pct || '', x: (p.x > 0 ? p.x : DEF.v), y: (p.y > 0 ? p.y : DEF.ctar), cmax: (p.cmax > 0 ? p.cmax : DEF.cmax) };
      });
      POOLS = m.append ? POOLS.concat(fresh) : fresh;       // appended: after the pools already there
      renderPools();
      sendPools();
    });

    // A row whose calculation was changed in the prompt gets a * after the number and another colour
    Shiny.addCustomMessageHandler('markCalc', function (m) {
      var el = sels[CALC_COL][m.row - 1];
      if (!el) return;
      modified[m.row - 1] = !!m.on;
      var v = el.value.replace(/\*$/, '');
      el.value = (m.on && v !== '') ? v + '*' : v;
      updateCalcNote();
    });

    Shiny.addCustomMessageHandler('truncRows', function (m) {
      var box = document.getElementById('dd-rows');
      while (box.children.length > m.n) box.removeChild(box.lastChild);
      for (var c = 1; c <= K; c++) sels[c].length = m.n;
      chks.length = m.n;
      gcells.length = m.n;
      modified.length = m.n;
      updateCalcNote();
      POOLS.forEach(function (g) { g.rows = g.rows.filter(function (r) { return r <= m.n; }); });
      POOLS = POOLS.filter(function (g) { return g.rows.length > 0; });
      renderPools();
    });

    Shiny.addCustomMessageHandler('clearRows', function (m) {
      document.getElementById('dd-rows').innerHTML = '';
      resetSels();
      updateCalcNote();
      POOLS = [];
      renderPools();
    });

    // ---- Pools: tick rows, press "Pool selected rows", write a text for the pool ----
    // Each pool is { rows: [row numbers], text: '...' }. The text and the value calculated from it
    // (x / (660 * 608) * 1000000) are shown in the panel above the table. Pools are also sent to R
    // as input$pools.
    function num(text) {                       // a typed number (comma allowed), NaN if empty or not a number
      var t = String(text).trim().replace(',', '.');
      return (t === '') ? NaN : Number(t);
    }
    function show(x) {                         // number -> text with at most 2 decimals, '' if not a number
      return isFinite(x) ? String(Math.round(x * 100) / 100) : '';
    }
    function nM(text)  { return num(text) / (660 * 608) * 1000000; }   // x / (660 * 608) * 1000000
    function volOf(text) { return 24 * (num(text) / 100); }            // 24 * (x / 100)
    // Starting text of "Pool ID": YYYY_WW_ (the year and week from the fields in the left panel)
    function defaultPoolId() {
      var y = document.getElementById('year'), w = document.getElementById('week');
      var yr = y ? String(y.value).trim() : '', wk = w ? parseInt(w.value, 10) : NaN;
      if (!yr || isNaN(wk)) return '';
      return yr + '_' + (wk < 10 ? '0' : '') + wk + '_';
    }
    // Changing the week or the year changes the Pool IDs that you haven't typed yourself
    document.addEventListener('input', function (e) {
      if (!e.target || (e.target.id !== 'week' && e.target.id !== 'year') || !POOLS.length) return;
      renderPools();
      sendPools();
    });
    function sendPools() {
      var pools = POOLS.map(function (g, gi) {
        return {
          rows: g.rows.slice().sort(function (a, b) { return a - b; }),
          name: g.name, nameUser: !!g.nameUser, text: g.text, text2: g.text2, pct: g.pct, x: g.x, y: g.y, cmax: g.cmax,
          // everything shown in the pool line, in column order (used for the pool sheet)
          cells: cellsOf(g)
        };
      });
      var totals = { sum: sumEl ? sumEl.value : '', rest: restEl ? restEl.value : '' };
      Shiny.setInputValue('pools', { pools: pools, totals: totals, nonce: Math.random() }, { priority: 'event' });
    }
    function wrapField(el, extra) {
      var w = document.createElement('div');
      w.className = 'dd-gfield' + (extra ? ' ' + extra : '');
      w.appendChild(el);
      return w;
    }
    // _{x} is shown as subscript and ^{x} as superscript
    var POOL_HEADS = [].concat(window.DD_POOL_HEADS);      // set in R (pool_heads)
    var DEF = window.DD_POOL_DEFAULTS;                     // { v: 15, ctar: 1.8, cmax: 0.85 }, set in R
    function xyDiffers(g) { return g.x !== DEF.v || g.y !== DEF.ctar; }     // V or c_target changed from the default
    function headHtml(t) {
      return t.replace(/\^\{([^}]*)\}_\{([^}]*)\}/g, '<span class="dd-stack"><sup>$1</sup><sub>$2</sub></span>')
              .replace(/\^\{([^}]*)\}/g, '<sup>$1</sup>').replace(/_\{([^}]*)\}/g, '<sub>$1</sub>');
    }
    function headPlain(t) { return t.replace(/\^\{([^}]*)\}/g, '^$1').replace(/_\{([^}]*)\}/g, '_$1'); }
    function cellsOf(g) {                      // everything shown in the pool line, in column order
      // g.calc = [Konc. nM, Konc. seqklar nM, Vol, Pool nM, v_fortynding, Pool, Rsb]
      var c = g.calc || ['', '', '', '', '', '', ''];
      return [g.name, g.text, c[0], c[5], c[6], g.text2, c[1], g.pct, c[2], c[3], c[4]];
    }
    var pctNoteEl = null;                      // the note under % af 24 \u03bcl (the sum is not 100)
    var sumEl = null, restEl = null;           // the two fields under the v_fortynding column
    function updateTotals() {                  // sum of v_fortynding and 24 minus that sum
      var em = document.getElementById('download5_msg');    // the message below the pool sheet button
      if (!sumEl) {
        if (em) em.textContent = '';
        return;
      }
      var sum = 0;
      POOLS.forEach(function (g) { if (isFinite(g.vf)) sum += g.vf; });
      sumEl.value = show(sum);
      restEl.value = show(24 - sum);
      // every column of every pool is required for the download. The message only names the columns
      // you type in (Pool ID and the three number fields); the calculated ones follow from them.
      var TYPED = [0, 1, 5, 7];
      var missing = [];
      TYPED.forEach(function (j) {
        if (POOLS.some(function (g) { return String(cellsOf(g)[j]).trim() === ''; })) missing.push("'" + headPlain(POOL_HEADS[j]) + "'");
      });
      var badValue = !missing.length && POOLS.some(function (g) {
        return cellsOf(g).some(function (v) { return String(v).trim() === ''; });
      });
      var msg = [];
      if (missing.length) {
        msg.push('Udfyld ' + (missing.length === 1 ? missing[0]
          : missing.slice(0, -1).join(', ') + ' og ' + missing[missing.length - 1]) + '.');
      }
      if (badValue) msg.push('Kontroll\u00e9r v\u00e6rdierne i pool-felterne.');
      // the percentages of the pools must add up to 100
      var pc = 0;
      POOLS.forEach(function (g) { var x = num(g.pct); if (isFinite(x)) pc += x; });
      var pcBad = Math.abs(pc - 100) > 0.005;
      var pcRound = Math.round(pc * 100) / 100;
      if (pcBad) msg.push('Summen af \'% af 24 \u03bcl\' er ' + pcRound + ' og skal v\u00e6re 100.');
      var allPct = POOLS.every(function (g) { return isFinite(num(g.pct)); });     // every pool has a percentage
      if (pctNoteEl) pctNoteEl.textContent = (pcBad && allPct) ? 'Summen er ' + pcRound + ', skal v\u00e6re 100' : '';
      if (em) em.textContent = msg.join(' ');
    }
    // Pool ID works like Sample ID: a pool whose ID you haven't typed takes the ID of the nearest pool
    // above that you typed, and a number at the end counts up by one per pool (2026_42_001, 2026_42_002, ...).
    // Without such a pool above it, it has the starting text YYYY_WW_.
    var nameEls = [];
    var seqHidden = true;                     // part 2 of the pool section hidden? (hidden to start with)
    var nameCopies = [];                      // the copies of Pool ID in part 2
    var fieldEls = {};                        // the typed fields of each pool line, by column (for Enter)
    function deriveNames() {
      var base = null, baseIdx = -1;
      POOLS.forEach(function (g, i) {
        if (g.nameUser) { base = g.name; baseIdx = i; return; }
        if (base === null) { g.name = defaultPoolId(); return; }
        var m = /^(.*?)(\d+)$/.exec(base);
        if (!m || m[2].length > 15) { g.name = base; return; }
        var n = String(Number(m[2]) + (i - baseIdx));
        while (n.length < m[2].length) n = '0' + n;
        g.name = m[1] + n;
      });
    }
    function renderPools() {
      sumEl = restEl = null;
      nameEls = [];
      nameCopies = [];
      fieldEls = {};
      deriveNames();
      for (var k = 0; k < gcells.length; k++) gcells[k].textContent = '';
      POOLS.forEach(function (g, gi) {
        g.rows.forEach(function (r) { if (gcells[r - 1]) gcells[r - 1].textContent = 'Pool ' + (gi + 1); });
      });
      var box = document.getElementById('dd-pools');
      if (!box) return;
      box.innerHTML = '';
      // The pool section is in two parts, one above the other: part 1 is Pool ID up to and including
      // Konc. seqklar [nM], part 2 starts with a copy of Pool ID and goes on with % af 24 \u03bcl and the rest.
      // Column labels of each part come first (also when there are no pools yet).
      function makeHead(heads) {
        var hd = document.createElement('div');
        hd.className = 'dd-pool-row dd-pool-head';
        [''].concat(heads).concat(['']).forEach(function (t, i) {
          var h = document.createElement('span');
          h.className = (i === 0) ? 'dd-glabel' : (i === heads.length + 1) ? 'dd-gbtn' : (i === 1) ? 'dd-gfield dd-gid' : 'dd-gfield';
          h.innerHTML = headHtml(t);
          hd.appendChild(h);
        });
        return hd;
      }
      var rows1 = [], rows2 = [];
      POOLS.forEach(function (g, gi) {
        var row = document.createElement('div');          // part 1
        row.className = 'dd-pool-row';
        var lab = document.createElement('span');
        lab.className = 'dd-glabel';
        lab.textContent = 'Pool ' + (gi + 1);
        var row2 = document.createElement('div');         // part 2
        row2.className = 'dd-pool-row';
        var lab2 = document.createElement('span');
        lab2.className = 'dd-glabel';
        lab2.textContent = 'Pool ' + (gi + 1);
        var nmCopy = document.createElement('input');     // a copy of Pool ID (changes when Pool ID does)
        nmCopy.type = 'text';
        nmCopy.className = 'form-control dd-calc';
        nmCopy.readOnly = true;
        nmCopy.tabIndex = -1;
        nmCopy.value = g.name;
        nameCopies[gi] = nmCopy;
        var nm = document.createElement('input');
        nm.type = 'text';
        nm.className = 'form-control';
        nm.value = g.name;
        nameEls[gi] = nm;
        nm.addEventListener('input', function () {
          g.name = nm.value;
          g.nameUser = nm.value.trim() !== '';        // typing locks the ID, clearing hands it back to the counting
          deriveNames();
          POOLS.forEach(function (p, i) {
            if (p !== g && nameEls[i]) nameEls[i].value = p.name;
            if (nameCopies[i]) nameCopies[i].value = p.name;
          });
          updateTotals();
        });
        nm.addEventListener('change', function () { nm.value = g.name; nmCopy.value = g.name; sendPools(); });
        var inp = document.createElement('input');
        inp.type = 'text';
        inp.className = 'form-control';
        inp.value = g.text;
        function calcField() {                   // a read-only field
          var el = document.createElement('input');
          el.type = 'text';
          el.className = 'form-control dd-calc';
          el.readOnly = true;
          el.tabIndex = -1;
          return el;
        }
        function textField(value, key) {          // a field you type in
          var el = document.createElement('input');
          el.type = 'text';
          el.className = 'form-control';
          el.value = value;
          (fieldEls[key] = fieldEls[key] || [])[gi] = el;
          el.addEventListener('keydown', function (e) {      // Enter: the same field one pool further down
            if (e.key !== 'Enter') return;
            var nx = fieldEls[key][gi + 1];
            if (nx) { e.preventDefault(); nx.focus(); nx.select(); }
          });
          el.addEventListener('input', function () { g[key] = el.value; refresh(); });
          el.addEventListener('change', sendPools);
          return el;
        }
        var inp = textField(g.text, 'text');       // Konc. ng/\u03bcl
        var out = calcField();                     // Konc. nM
        var inp2 = textField(g.text2, 'text2');    // Konc. seqklar ng/\u03bcl
        var out2 = calcField();                    // Konc. seqklar nM
        var pct = textField(g.pct, 'pct');         // % af 24 \u03bcl
        var vol = calcField();                     // Vol. af 24 \u03bcl
        var pnm = calcField();                     // Konc. pool [nM]: the smaller of Konc. nM and g.cmax (DEF.cmax unless changed)
        pnm.classList.add('dd-poolcalc');          // click it to change the maximum
        pnm.title = 'Click to change the maximum';
        pnm.addEventListener('click', function () {
          Shiny.setInputValue('pool_cmax_click', { idx: gi, name: 'Pool ' + (gi + 1), cmax: g.cmax, nonce: Math.random() },
                              { priority: 'event' });
        });
        var vf = calcField();                      // v_fortynding = Vol * Pool nM / Konc. seqklar nM
        var pv = calcField();                      // Pool = X * Y / Konc. nM (X = DEF.v and Y = DEF.ctar unless changed in the prompt)
        var rv = calcField();                      // Rsb = X - Pool
        pv.classList.add('dd-poolcalc');           // click it to change X and Y
        pv.title = 'Click to change V and c_target';
        pv.addEventListener('click', function () {
          Shiny.setInputValue('pool_xy_click', { idx: gi, name: 'Pool ' + (gi + 1), x: g.x, y: g.y, nonce: Math.random() },
                              { priority: 'event' });
        });
        function refresh() {
          var konc = nM(g.text), seq = nM(g.text2), v = volOf(g.pct);
          var pool = isFinite(konc) ? Math.min(konc, g.cmax) : NaN;
          var f = (isFinite(v) && isFinite(pool) && isFinite(seq) && seq !== 0) ? v * pool / seq : NaN;
          out.value = show(konc);
          out2.value = show(seq);
          vol.value = show(v);
          var pnT = show(pool);
          pnm.value = (g.cmax !== DEF.cmax && pnT !== '') ? pnT + '*' : pnT;     // * is only shown, the data stays clean
          vf.value = show(f);
          var pc = (isFinite(konc) && konc > 0) ? g.x * g.y / konc : NaN;     // Pool
          var rs = isFinite(pc) ? g.x - pc : NaN;                             // Rsb
          var pT = show(pc), rT = show(rs);
          var chg = xyDiffers(g);                              // V or c_target changed
          pv.value = (chg && pT !== '') ? pT + '*' : pT;                      // * is only shown, the data stays clean
          rv.value = (chg && rT !== '') ? rT + '*' : rT;
          g.vf = f;
          g.calc = [out.value, out2.value, vol.value, pnT, vf.value, pT, rT];
          updateTotals();
        }
        g.refresh = refresh;
        refresh();
        var del = document.createElement('button');
        del.type = 'button';
        del.className = 'btn btn-default btn-sm dd-gbtn';
        del.textContent = 'Remove';
        del.addEventListener('click', function () { POOLS.splice(gi, 1); renderPools(); sendPools(); });
        row.appendChild(lab);                              // part 1
        row.appendChild(wrapField(nm, 'dd-gid'));
        row.appendChild(wrapField(inp));
        row.appendChild(wrapField(out));
        row.appendChild(wrapField(pv));
        row.appendChild(wrapField(rv));
        row.appendChild(wrapField(inp2));
        row.appendChild(wrapField(out2));
        row.appendChild(del);
        rows1.push(row);
        row2.appendChild(lab2);                            // part 2
        row2.appendChild(wrapField(nmCopy, 'dd-gid'));
        row2.appendChild(wrapField(pct));
        row2.appendChild(wrapField(vol));
        row2.appendChild(wrapField(pnm));
        row2.appendChild(wrapField(vf));
        for (var e1 = 0; e1 < 2; e1++) {                   // empty places, so the columns are as wide as in part 1
          var em1 = document.createElement('span');
          em1.className = 'dd-gfield';
          row2.appendChild(em1);
        }
        var gap2 = document.createElement('span');
        gap2.className = 'dd-gbtn';
        row2.appendChild(gap2);
        rows2.push(row2);
      });
      // notes about changed values, a line under the column they belong to
      function noteRow(slots, texts) {                     // texts: {slot number: html}
        var nr = document.createElement('div');
        nr.className = 'dd-pool-row';
        var ns = document.createElement('span');
        ns.className = 'dd-glabel';
        nr.appendChild(ns);
        for (var j = 1; j <= slots; j++) {
          var nc = document.createElement('span');
          nc.className = (j === 1) ? 'dd-gfield dd-gid' : 'dd-gfield';
          if (texts[j]) { nc.className += ' dd-poolnote'; nc.innerHTML = texts[j]; }
          nr.appendChild(nc);
        }
        var nb = document.createElement('span');
        nb.className = 'dd-gbtn';
        nr.appendChild(nb);
        return nr;
      }
      var xyChanged = POOLS.some(xyDiffers);
      var cmChanged = POOLS.some(function (g) { return g.cmax !== DEF.cmax; });
      box.appendChild(makeHead(POOL_HEADS.slice(0, 7)));
      rows1.forEach(function (r) { box.appendChild(r); });
      if (xyChanged) box.appendChild(noteRow(7, { 4: '* V [\u03bcl] og c<sub>target</sub> [nM] \u00e6ndret' }));
      var sep = document.createElement('div');             // space between the two parts
      sep.style.height = '22px';
      box.appendChild(sep);
      var seq = document.createElement('div');              // part 2, which can be hidden
      var seqBtn = document.createElement('button');
      seqBtn.type = 'button';
      seqBtn.className = 'btn btn-default btn-sm';
      seqBtn.style.cssText = 'min-width: 15em; margin: 0 0 8px 0;';   // same size for Vis and Skjul
      function applySeq() {
        seq.style.display = seqHidden ? 'none' : '';
        seqBtn.textContent = seqHidden ? 'Vis sequencing pool' : 'Skjul sequencing pool';
      }
      seqBtn.addEventListener('click', function () { seqHidden = !seqHidden; applySeq(); });
      applySeq();
      box.appendChild(seqBtn);
      box.appendChild(seq);
      seq.appendChild(makeHead([POOL_HEADS[0]].concat(POOL_HEADS.slice(7), ['', ''])));
      rows2.forEach(function (r) { seq.appendChild(r); });
      if (cmChanged) seq.appendChild(noteRow(7, { 4: '* Konc. pool [nM] \u00e6ndret' }));
      if (POOLS.length) {                      // the note under % af 24 \u03bcl (filled in by updateTotals)
        var pr = noteRow(7, { 2: ' ' });
        pctNoteEl = pr.children[2];
        pctNoteEl.style.color = '#a94442';
        seq.appendChild(pr);
      }
      if (POOLS.length) {                      // under the v_fortynding column: the sum and 24 minus the sum
        [['Sum', 'sumEl'], ['RSB Vol.', 'restEl']].forEach(function (d) {
          var fr = document.createElement('div');
          fr.className = 'dd-pool-row';
          var sp = document.createElement('span');
          sp.className = 'dd-glabel';
          fr.appendChild(sp);
          for (var i = 1; i <= 4; i++) {
            var c = document.createElement('span');
            c.className = (i === 1) ? 'dd-gfield dd-gid' : 'dd-gfield';
            if (i === 4) { c.textContent = d[0]; c.style.textAlign = 'right'; c.style.fontWeight = 'bold'; }
            fr.appendChild(c);
          }
          var f = document.createElement('input');
          f.type = 'text';
          f.className = 'form-control dd-calc';
          f.readOnly = true;
          f.tabIndex = -1;
          fr.appendChild(wrapField(f));
          for (var e2 = 0; e2 < 2; e2++) {
            var em2 = document.createElement('span');
            em2.className = 'dd-gfield';
            fr.appendChild(em2);
          }
          var bs = document.createElement('span');
          bs.className = 'dd-gbtn';
          fr.appendChild(bs);
          seq.appendChild(fr);
          if (d[1] === 'sumEl') sumEl = f; else restEl = f;
        });
        updateTotals();
      }
    }
    // Put the ticked rows into a new pool
    function poolSelected() {
      var rows = [];
      for (var k = 0; k < chks.length; k++) if (chks[k].checked) { rows.push(k + 1); chks[k].checked = false; }
      poolRows(rows);
    }
    // Put all rows that are not in a pool yet into one new pool (the existing pools stay as they are)
    function poolAll() {
      var pooled = {};
      POOLS.forEach(function (g) { g.rows.forEach(function (r) { pooled[r] = true; }); });
      var rows = [];
      for (var r = 1; r <= chks.length; r++) if (!pooled[r]) { rows.push(r); chks[r - 1].checked = false; }
      poolRows(rows);
    }
    function poolRows(rows) {
      if (!rows.length) return;
      // a row belongs to one pool only: take the rows out of their old pools
      POOLS.forEach(function (g) { g.rows = g.rows.filter(function (r) { return rows.indexOf(r) < 0; }); });
      POOLS = POOLS.filter(function (g) { return g.rows.length > 0; });
      POOLS.push({ rows: rows, name: defaultPoolId(), nameUser: false, text: '', text2: '', pct: '', x: DEF.v, y: DEF.ctar, cmax: DEF.cmax });
      renderPools();
      sendPools();
    }
    document.addEventListener('click', function (e) {
      if (!e.target.closest) return;
      if (e.target.closest('#dd-pool-btn')) poolSelected();
      if (e.target.closest('#dd-pool-all-btn')) poolAll();
    });
    // The space bar also pools the ticked rows (only while pooling is shown, and not while you type in a field)
    // Enter in a "plain" column (typed numbers) moves down to the same column in the row below
    document.addEventListener('keydown', function (e) {
      if (e.key !== 'Enter') return;
      var t = e.target;
      if (!t || t.tagName !== 'INPUT') return;
      for (var c = 1; c <= K; c++) {
        if (TYPES[c - 1] !== 'plain') continue;
        var i = sels[c].indexOf(t);
        if (i < 0) continue;
        var nx = sels[c][i + 1];
        if (nx) { e.preventDefault(); nx.focus(); if (nx.select) nx.select(); }
        return;
      }
    });
    document.addEventListener('keydown', function (e) {
      if (e.key !== ' ') return;
      if (document.getElementById('dd-root').classList.contains('dd-hide-pool')) return;
      var t = e.target;
      var typing = t.tagName === 'SELECT' || t.tagName === 'TEXTAREA' || t.tagName === 'BUTTON' ||
                   (t.tagName === 'INPUT' && t.type !== 'checkbox');
      if (typing) return;
      var any = false;
      for (var k = 0; k < chks.length; k++) if (chks[k].checked) { any = true; break; }
      if (!any) return;
      e.preventDefault();                       // (a ticked checkbox would otherwise toggle)
      poolSelected();
    });

    // Drag the edge below the pool section to make it taller (the table below gets shorter) or shorter
    document.addEventListener('pointerdown', function (e) {
      var grip = e.target.closest ? e.target.closest('#dd-pools-grip') : null;
      if (!grip) return;
      e.preventDefault();
      var pools = document.getElementById('dd-pools'), root = document.getElementById('dd-root');
      var startY = e.clientY, startH = pools.getBoundingClientRect().height;
      grip.setPointerCapture(e.pointerId);
      function move(ev) {
        var h = Math.max(80, Math.min(window.innerHeight * 0.8, startH + ev.clientY - startY));
        root.style.setProperty('--pool-h', h + 'px');
        root.style.setProperty('--wrap-shrink', (h - window.innerHeight * 0.25) + 'px');   // how much the table below shrinks
      }
      function up() {
        grip.removeEventListener('pointermove', move);
        grip.removeEventListener('pointerup', up);
      }
      grip.addEventListener('pointermove', move);
      grip.addEventListener('pointerup', up);
    });

    // Show the pool column labels from the start
    if (document.readyState === 'loading') document.addEventListener('DOMContentLoaded', renderPools);
    else renderPools();
  }

  if (window.Shiny) { setup(); } else { document.addEventListener('DOMContentLoaded', setup); }
})();
)---"

css_code <- "
#ids_file_ui .shiny-input-container, #data_file_ui .shiny-input-container { margin-bottom: 2px; }
#ids_file_ui .progress, #data_file_ui .progress { margin: 2px 0 0 0; height: 6px; }
#ids_file_ui .progress-bar, #data_file_ui .progress-bar { font-size: 0; }   /* no upload-complete text in the blue bar */
#ids_file_ui .btn-file, #data_file_ui .btn-file { width: 110px; text-align: center; }
/* the same space above and below the lines between the parts of the sidebar */
#dd-root form.well hr { margin: 14px 0; }
#dd-root form.well .dd-tight, #dd-root form.well .dd-tight .form-group { margin-bottom: 0; }
#dd-wrap { max-height: max(120px, calc(72vh - var(--wrap-shrink, 0px))); overflow-y: auto; padding-right: 6px; }   /* fitWrap in the JavaScript sets the real height */
.dd-hide-pool #dd-wrap { max-height: 72vh; }
.dd-head, .dd-row { display: flex; gap: 8px; align-items: center; }
.dd-head { font-weight: bold; margin-bottom: 6px; align-items: flex-end; }
.dd-head span { overflow-wrap: anywhere; }
.dd-gone { display: none !important; }
#dd-calcnote { margin-top: 4px; }
.dd-calcnote-cell { flex: 1 1 100px; min-width: 0; font-size: 85%; color: #8a6d3b; white-space: nowrap; }
#dd-rows .dd-calc, .dd-pool-row input.dd-poolcalc { cursor: pointer; padding-right: 26px; border-color: #8aa4c8;
  background-repeat: no-repeat; background-position: right 7px center; background-size: 13px 13px;
  background-image: url('data:image/svg+xml;utf8,<svg xmlns=%22http://www.w3.org/2000/svg%22 viewBox=%220 0 16 16%22><path fill=%22%23337ab7%22 d=%22M12.1 1.3l2.6 2.6c.4.4.4 1 0 1.4L5.6 14.4 1.4 15l.6-4.2 9.1-9.5c.4-.4 1-.4 1.4 0z%22/></svg>'); }
#dd-rows .dd-calc:hover, .dd-pool-row input.dd-poolcalc:hover { background-color: #e6f0fb; border-color: #337ab7; }
.dd-hide-pool .dd-pool { display: none !important; }
.dd-hide-pool .dd-poolghost { visibility: hidden; }
.dd-hide-extra .dd-extra { display: none !important; }
.dd-hide-extra .dd-ghost { visibility: hidden; }
.dd-msgbox { position: relative; margin-top: 2px; }
.dd-msg-ghost, .dd-msg { font-size: 85%; line-height: 1.25; }
.dd-msg-ghost { visibility: hidden; }
.dd-msg { position: absolute; top: 0; right: 0; bottom: 0; left: 0; overflow-y: auto; color: #a94442; }
.dd-row { margin-bottom: 4px; }
.dd-chk { flex: none; width: 1.5em; margin: 0; }
.dd-grpcell { flex: none; width: 3.4em; color: #31708f; font-weight: bold; white-space: nowrap; }
#dd-pools { height: var(--pool-h, 25vh); overflow-y: auto; scrollbar-gutter: stable; padding-bottom: 6px; }
#dd-pools-grip { position: relative; height: 10px; margin-bottom: 8px; border-top: 1px solid #ccc; cursor: row-resize; touch-action: none; }
#dd-pools-grip::after { content: ''; position: absolute; left: 50%; top: 3px; width: 40px; height: 3px; margin-left: -20px; border-radius: 2px; background: #bbb; }
#dd-pools-grip:hover { background: #f0f0f0; }
.dd-pool-row { display: flex; gap: 6px; align-items: center; margin-bottom: 6px; }
.dd-pool-head { font-weight: bold; margin-bottom: 4px; }
.dd-glabel { flex: 0 0 3.4em; font-weight: bold; }
.dd-gfield { flex: 1; min-width: 0; }
.dd-stack { display: inline-flex; flex-direction: column; vertical-align: middle; font-size: 75%; line-height: 1.1; }
.dd-stack sup, .dd-stack sub { position: static; font-size: 100%; line-height: 1.1; vertical-align: baseline; }
#dd-toggle { min-width: 16em; }          /* same size whether it says Vis or Skjul */
#dd-pool-toggle { min-width: 9em; }
.dd-bad, #dd-root select.dd-bad, #dd-root input.dd-bad { border-color: #a94442; background-color: #f2dede; color: #a94442; }
.dd-poolnote { font-size: 85%; color: #8a6d3b; white-space: nowrap; overflow: visible; }
.dd-gid { flex: 2.5 1 0; }
.dd-gbtn { flex: 0 0 62px; box-sizing: border-box; }
.dd-pool-row button.dd-gbtn { padding-left: 4px; padding-right: 4px; }
.dd-num { width: 2.6em; margin-left: -4px; text-align: right; color: #888; flex: none; }
/* column widths (children 1-3 are tick box, Pool and Well; column c is child c+3):
   Sample ID and Plade ID never get narrower than 7em, Index Br\u00f8nd and Lib. Vol. are narrower than the rest, DNA Konc. and Lib. Konc. a little narrower */
#dd-root .dd-head > :nth-child(5), #dd-root .dd-row > :nth-child(5),
#dd-root .dd-head > :nth-child(6), #dd-root .dd-row > :nth-child(6) { min-width: 7em; }
#dd-root .dd-head > :nth-child(9), #dd-root .dd-row > :nth-child(9),
#dd-root .dd-head > :nth-child(11), #dd-root .dd-row > :nth-child(11) { flex: 0.75 1 75px; }
#dd-root .dd-head > :nth-child(7), #dd-root .dd-row > :nth-child(7),
#dd-root .dd-head > :nth-child(10), #dd-root .dd-row > :nth-child(10) { flex: 0.85 1 85px; }
.dd-head span:not(.dd-num):not(.dd-chk):not(.dd-grpcell), .dd-row select, .dd-row input:not([type=checkbox]) { flex: 1 1 100px; min-width: 0; }   /* same start size for everyone, so padding can't make the columns differ from the labels */
"

# The message for columns that still have to be filled in: Udfyld 'a', Udfyld 'a' og 'b', Udfyld 'a', 'b' og 'c'
udfyld_text <- function(cols) {
    q <- paste0("'", cols, "'")
    paste0("Udfyld ", if (length(q) == 1) q else
        paste(paste(head(q, -1), collapse = ", "), "og", q[length(q)]))
}

# The buttons of a prompt that changes a value: Cancel, "for alle under" (this one and everything below it) and OK.
# The buttons get the ids <id>_cancel, <id>_ok_below and <id>_ok.
prompt_footer <- function(id) {
    tagList(actionButton(paste0(id, "_cancel"),   "Cancel",         class = "btn-sm"),
            actionButton(paste0(id, "_ok_below"), "for alle under", class = "btn-sm"),
            actionButton(paste0(id, "_ok"),       "OK",             class = "btn-primary btn-sm"))
}
# A red error line in a prompt (nothing when there is no error)
error_text <- function(msg) if (nzchar(msg)) div(style = "color: #a94442;", msg)

# A download button with a spot below it for its message. The server fills in the message
# (see "sheetState" in the JavaScript); the button can always be pressed.
# `longest` is the longest message this button can show: it is written invisibly in the spot, so the
# spot is exactly as tall as that message (at the current width) and the buttons below never move.
sheet_button <- function(id, label, longest, hide = FALSE, pool = FALSE) {
    cls <- c(if (hide) "dd-extra", if (pool) "dd-poolghost")   # dd-poolghost: invisible (but keeps its space) while pooling is hidden
    div(class = if (length(cls)) paste(cls, collapse = " "), style = "margin-top: 4px;",
        downloadButton(id, label),
        div(class = "dd-msgbox",
            div(class = "dd-msg-ghost", `aria-hidden` = "true", longest),
            div(id = paste0(id, "_msg"), class = "dd-msg")))
}

ui <- function(request) fluidPage(
    tags$head(
        tags$style(HTML(css_code)),
        tags$script(HTML(paste0("window.DD_CHOICES = ", jsonlite::toJSON(col_choices), ";",
                                "window.DD_TYPES = ", jsonlite::toJSON(col_type), ";",
                                "window.DD_WELLS = ", jsonlite::toJSON(well_names), ";",
                                "window.DD_PLACEHOLDERS = ", jsonlite::toJSON(col_placeholder), ";",
                                "window.DD_POOL_HEADS = ", jsonlite::toJSON(pool_heads), ";",
                                "window.DD_POOL_DEFAULTS = ", jsonlite::toJSON(list(v = pool_default_v, ctar = pool_default_ctar,
                                                                                    cmax = pool_default_cmax), auto_unbox = TRUE), ";",
                                "window.DD_HIDDEN = ", jsonlite::toJSON(col_hidden), ";",
                                "window.DD_CALC_COL = ", calc_col, ";",
                                "window.DD_GONE = ", jsonlite::toJSON(seq_len(K) == div_col), ";"))),
        tags$script(HTML(js_code))
    ),
    div(id = "dd-root", class = "dd-hide-extra dd-hide-pool",   # while it has this class, everything marked dd-extra is hidden
        titlePanel("Jakobs sample sheet app"),
        sidebarLayout(
            sidebarPanel(
                width = 3,
                fluidRow(
                    column(6, textInput("project_name", "Project name", value = "NBS_NGS")),
                    column(6, textInput("cartridge_cycles", "Cartridge cycles", value = "200"),
                           uiOutput("cycles_msg"))
                ),
                fluidRow(class = "dd-tight",
                         column(6, numericInput("week", "Week", value = as.integer(format(Sys.Date(), "%V")),
                                                min = 1, max = 53, step = 1)),
                         column(6, numericInput("year", "Year", value = as.integer(format(Sys.Date(), "%G")),
                                                min = 2000, max = 2100, step = 1))
                ),
                hr(),
                numericInput("n_rows", "Antal samples", value = NA,
                             min = 1, max = max_rows, step = 1),
                div(class = "form-group dd-ghost dd-tight",
                    tags$label("Samlet pool"),
                    div(class = "form-control", style = "background-color: #eee;",
                        textOutput("samlet_pool", inline = TRUE))),
                hr(),
                uiOutput("ids_file_ui"),
                hr(),
                uiOutput("data_file_ui"),
                downloadButton("save_data", "Save data (csv)", style = "margin-top: 4px;"),
                hr(),
                div(style = "display: flex; gap: 6px;",                  # the two pool buttons, equally wide
                    tags$button(id = "dd-pool-btn", type = "button", class = "btn btn-default dd-poolghost",
                                style = "flex: 1 1 0;", "Pool valgte r\u00e6kker"),
                    tags$button(id = "dd-pool-all-btn", type = "button", class = "btn btn-default dd-poolghost",
                                style = "flex: 1 1 0;", "Pool alle r\u00e6kker")),
                actionButton("reset", "Nulstil", style = "margin-top: 6px;"),
                hr(),
                sheet_button("download",  "Download sample sheet",
                             udfyld_text(c(col_names_plain[required_cols], "Project name", "Week", "Year", "Cartridge cycles"))),
                sheet_button("download2", "Download IEM sample sheet",
                             udfyld_text(c(col_names_plain[required_cols], col_names_plain[screen_col], "Project name"))),
                sheet_button("download3", "Download concentration sheet",
                             udfyld_text(c(col_names_plain[sample_col], col_names_plain[dnakonc_col], col_names_plain[conc_col], "Pool Konc. [ng/\u03bcl]")), hide = TRUE),
                sheet_button("download4", "Download plate layout",
                             udfyld_text(col_names_plain[conc_col]), hide = TRUE),
                sheet_button("download5", "Download pool sheet",
                             paste("Udfyld 'Pool ID', 'Pool Konc. [ng/\u03bcl]', 'Konc. seqklar [ng/\u03bcl]' og '% af 24 \u03bcl'.",
                                   "Summen af '% af 24 \u03bcl' er 100.01 og skal v\u00e6re 100."), pool = TRUE),
                uiOutput("status_ui")
            ),
            mainPanel(
                width = 9,
                div(id = "dd-grid", style = "max-width: 1400px;",
                    div(style = "text-align: right; margin-bottom: 6px;",
                        tags$button(id = "dd-toggle", type = "button", class = "btn btn-default btn-sm",
                                    "Vis lib. konc. kolonner"),
                        tags$button(id = "dd-pool-toggle", type = "button", class = "btn btn-default btn-sm",
                                    "Vis pooling")),
                    div(id = "dd-pools", class = "dd-pool"),
                    div(id = "dd-pools-grip", class = "dd-pool", title = "Drag to change the height of the pool section"),
                    do.call(div, c(list(class = "dd-head", span(class = "dd-chk dd-pool"), span("Pool", class = "dd-grpcell dd-pool"),
                                        span(class = "dd-num", "Well")),
                                   lapply(seq_along(col_names), function(i) {
                                       span(HTML(label_html(col_names[i])), class = paste(c(if (col_hidden[i]) "dd-extra", if (i == div_col) "dd-gone"), collapse = " "))
                                   }))),
                    div(id = "dd-wrap", div(id = "dd-rows"))
                )
            )
        )
    )
)

# Used by the launcher (StartSampleSheet.vbs): counts the open browser windows of the app
session_counter <- new.env()
session_counter$n <- 0

server <- function(input, output, session) {
    # When started by the launcher (option samplesheet.autostop): stop R when the window is closed.
    # It waits a few seconds first, so reloading the page (which ends one session and starts another)
    # doesn't stop the app.
    if (isTRUE(getOption("samplesheet.autostop"))) {
        session_counter$n <- session_counter$n + 1
        session$onSessionEnded(function() {
            session_counter$n <- session_counter$n - 1
            later::later(function() if (session_counter$n <= 0) stopApp(), 5)
        })
    }
    
    # The app's own record of the table (the browser only displays it)
    st <- do.call(reactiveValues, c(
        list(n = 0L, file_rows = logical()),   # file_rows: Sample IDs in these rows came from a file
        setNames(rep(list(character()), K), paste0("v", seq_len(K))),  # current value of every dropdown
        setNames(rep(list(logical()),   K), paste0("u", seq_len(K)))   # TRUE = the user picked it themselves
    ))
    vk <- function(c) paste0("v", c)
    uk <- function(c) paste0("u", c)
    
    # Where rows that come from a file go: below everything that is there (IDs from a file, and rows you
    # have filled in). The exception is a single empty row (or no rows at all): then the file starts in row 1.
    # Returns the number of rows to keep above the new ones.
    rows_above_new <- function() {
        used <- st$file_rows
        for (c in setdiff(seq_len(K), calc_col)) used <- used | st[[uk(c)]]
        if (st$n == 0 || (st$n == 1 && !any(used))) 0L else as.integer(st$n)
    }
    
    # Konc. [nM] of the pool every row 1..n is in ("" for rows in no pool). The browser sends the pools
    # as input$pools; the third value of a pool line is Konc. [nM].
    pool_nM_by_row <- function(n) {
        out <- rep("", n)
        for (pl in input$pools$pools) {
            v   <- as.character(unlist(pl$cells))[3]
            rws <- as.integer(unlist(pl$rows))
            rws <- rws[!is.na(rws) & rws >= 1 & rws <= n]
            out[rws] <- if (is.na(v)) "" else v
        }
        out
    }
    
    # NA = no number of samples chosen, otherwise the number of rows requested
    mode_n <- reactive({
        n <- input$n_rows
        if (is.null(n) || is.na(n) || n < 1) NA else min(floor(n), max_rows)
    })
    
    # Append k empty rows
    add_rows <- function(k) {
        old <- st$n
        for (c in seq_len(K)) {
            st[[vk(c)]] <- c(st[[vk(c)]], rep(col_default[c], k))
            st[[uk(c)]] <- c(st[[uk(c)]], rep(FALSE, k))
        }
        st$file_rows <- c(st$file_rows, rep(FALSE, k))
        st$n <- old + k
        session$sendCustomMessage("addRows",
                                  list(start = old + 1L, v = lapply(seq_len(K), function(c) rep(col_default[c], k))))
    }
    
    # Drop all rows below row k
    truncate_to <- function(k) {
        keep <- seq_len(k)
        for (c in seq_len(K)) {
            st[[vk(c)]] <- st[[vk(c)]][keep]
            st[[uk(c)]] <- st[[uk(c)]][keep]
        }
        st$file_rows <- st$file_rows[keep]
        st$n <- as.integer(k)
        session$sendCustomMessage("truncRows", list(n = k))
    }
    
    # Fixed mode: show exactly `target` rows and auto-fill the dropdowns nobody picked
    resize <- function(target) {
        if (target < st$n) truncate_to(target)
        old <- st$n
        k   <- target - old
        old_rows <- seq_len(old)
        new_rows <- old + seq_len(k)
        
        ff <- c(st$file_rows, rep(FALSE, k))
        st$file_rows <- ff
        
        new_vals <- vector("list", K)
        
        # Store a column's new values and tell the browser about existing rows that changed
        commit <- function(c, v, u, nv) {
            ch <- which(nv[old_rows] != v[old_rows])
            if (length(ch)) session$sendCustomMessage("setCells", list(col = c, idx = ch, val = nv[ch]))
            st[[vk(c)]] <- nv
            st[[uk(c)]] <- u
            new_vals[[c]] <<- nv[new_rows]
        }
        
        for (c in setdiff(seq_len(K), calc_col)) {
            v <- c(st[[vk(c)]], rep(col_default[c], k))
            u <- c(st[[uk(c)]], rep(FALSE, k))
            commit(c, v, u, derive_col(c, v, u, if (c == sample_col) ff else NULL))
        }
        # The calculated column goes last: it needs the concentration and the 500/350 column
        v <- c(st[[vk(calc_col)]], rep("", k))
        u <- c(st[[uk(calc_col)]], rep(FALSE, k))
        commit(calc_col, v, u, calc_from(st[[vk(conc_col)]], st[[vk(div_col)]]))
        
        st$n <- as.integer(target)
        
        if (k > 0) {
            session$sendCustomMessage("addRows", list(start = old + 1L, v = new_vals))
        }
    }
    
    # The "Number of rows" box changed (also runs once at startup)
    observeEvent(mode_n(), {
        target <- mode_n()
        if (is.na(target)) {
            # No number of samples chosen: start with one row, otherwise leave the rows as they are
            # (rows are never added by filling in the last one)
            if (st$n == 0) add_rows(1)
        } else {
            resize(target)
        }
    }, ignoreNULL = FALSE)
    
    # Recalculate the Pools column and tell the browser about the cells that changed
    refresh_calc <- function() {
        old <- st[[vk(calc_col)]]
        new <- calc_from(st[[vk(conc_col)]], st[[vk(div_col)]])
        ch  <- which(new != old)
        if (length(ch)) {
            session$sendCustomMessage("setCells", list(col = calc_col, idx = ch, val = new[ch]))
            st[[vk(calc_col)]] <- new
        }
    }
    
    # The sum of the Pools column
    output$samlet_pool <- renderText({
        x <- suppressWarnings(as.numeric(st[[vk(calc_col)]]))
        sprintf("%.1f", sum(x, na.rm = TRUE))
    })
    
    # The user changed a dropdown
    observeEvent(input$cell_change, {
        ev  <- input$cell_change
        col <- ev$col
        i   <- ev$row
        val <- ev$value %||% ""
        n   <- st$n
        if (is.null(col) || is.null(i) || col < 1 || col > K || i < 1 || i > n) return()
        if (col_type[col] == "calc") return()      # read-only column
        if ((col_type[col] == "choice" || col == div_col) && val == "") {   # emptied -> back to the default
            val <- col_default[col]
            session$sendCustomMessage("setCells", list(col = col, idx = i, val = val))
        }
        
        v <- st[[vk(col)]]
        u <- st[[uk(col)]]
        v[i] <- val
        u[i] <- (val != "")           # picking locks the cell; clearing hands it back to the auto-fill
        
        # Editing a Sample ID by hand: it is no longer "from the file"
        ff <- st$file_rows
        if (col == sample_col) ff[i] <- FALSE
        
        fixed <- !is.na(mode_n())
        if (fixed) {
            new <- derive_col(col, v, u, if (col == sample_col) ff else NULL)
            ch  <- which(new != v)      # only these cells have to be updated in the browser
            if (length(ch)) {
                session$sendCustomMessage("setCells", list(col = col, idx = ch, val = new[ch]))
            }
            v <- new
        }
        st[[vk(col)]] <- v
        st[[uk(col)]] <- u
        st$file_rows <- ff
        
        # Concentration or the 500/350 choice changed: recalculate the Pools column
        if (col == conc_col || col == div_col) refresh_calc()
    })
    
    # Clicking a cell of the calculated column (\u03bcl) opens a prompt: the number the concentration is
    # divided into (500 by default; 350 for example). It is kept per row, in the hidden column div_col.
    div_row <- reactiveVal(NULL)
    observeEvent(input$calc_click, {
        i <- input$calc_click$row
        if (is.null(i) || i < 1 || i > st$n) return()
        div_row(i)
        cur <- suppressWarnings(as.numeric(st[[vk(div_col)]][i]))
        if (is.na(cur)) cur <- as.numeric(col_default[div_col])
        showModal(modalDialog(
            title = paste0("Change calculation (", well_label(i), ")"),
            size = "s",
            p("V = m/c. m: \u00f8nskede v\u00e6gt library"),
            numericInput("div_value", "m (for eksempel 500 ng eller 350 ng)", value = cur, min = 1, step = 50),
            uiOutput("div_msg"),
            footer = prompt_footer("div"),
            easyClose = FALSE
        ))
    })
    div_error <- reactiveVal("")
    output$div_msg <- renderUI(error_text(div_error()))
    observeEvent(input$div_cancel, { removeModal(); div_error("") })
    observeEvent(input$div_ok, div_apply(FALSE))
    observeEvent(input$div_ok_below, div_apply(TRUE))     # "for alle under": this row and all rows below
    div_apply <- function(below) {
        i <- div_row()
        x <- input$div_value
        if (is.null(i) || i > st$n) { removeModal(); return() }
        if (bad_number(x)) {
            div_error("Write a number above 0.")
            return()
        }
        div_error("")
        txt <- format(x, scientific = FALSE, trim = TRUE)
        v <- st[[vk(div_col)]]; u <- st[[uk(div_col)]]
        rows <- if (below) i:st$n else i
        v[rows] <- txt
        u[rows] <- (txt != col_default[div_col])
        st[[vk(div_col)]] <- v
        st[[uk(div_col)]] <- u
        session$sendCustomMessage("setCells", list(col = div_col, idx = rows, val = rep(txt, length(rows))))
        refresh_calc()
        for (r in rows) session$sendCustomMessage("markCalc", list(row = r, on = u[r]))   # * while it differs from the default
        removeModal()
    }
    
    # Clicking a cell of the "Pool" column in the pool section opens a prompt for X and Y of
    # Pool = V * c_target / Konc. nM (and Rsb = V - Pool). They are kept per pool in the browser.
    pool_xy_idx <- reactiveVal(NULL)
    pool_xy_err <- reactiveVal("")
    observeEvent(input$pool_xy_click, {
        ck <- input$pool_xy_click
        pool_xy_idx(ck$idx)
        pool_xy_err("")
        showModal(modalDialog(
            title = paste0("Change V and c_target (", ck$name, ")"),
            size = "s",
            numericInput("pool_x", paste0("V (", pool_default_v, " \u03bcl standard)"), value = ck$x, min = 0, step = 1),
            numericInput("pool_y", tags$span("c", tags$sub("target"), paste0(" (", pool_default_ctar, " nM standard)")),
                         value = ck$y, min = 0, step = 0.1),
            uiOutput("pool_xy_msg"),
            footer = prompt_footer("pool_xy"),
            easyClose = FALSE
        ))
    })
    output$pool_xy_msg <- renderUI(error_text(pool_xy_err()))
    observeEvent(input$pool_xy_cancel, removeModal())
    pool_xy_apply <- function(below) {
        x <- input$pool_x; y <- input$pool_y
        if (bad_number(x) || bad_number(y)) {
            pool_xy_err("V and c_target must be numbers above 0.")
            return()
        }
        session$sendCustomMessage("setPoolXY", list(idx = pool_xy_idx(), x = x, y = y, below = below))
        removeModal()
    }
    observeEvent(input$pool_xy_ok, pool_xy_apply(FALSE))
    observeEvent(input$pool_xy_ok_below, pool_xy_apply(TRUE))
    
    # Clicking a cell of the "Konc. pool [nM]" column opens a prompt for its maximum (0.85 as standard)
    pool_cmax_idx <- reactiveVal(NULL)
    pool_cmax_err <- reactiveVal("")
    observeEvent(input$pool_cmax_click, {
        ck <- input$pool_cmax_click
        pool_cmax_idx(ck$idx)
        pool_cmax_err("")
        showModal(modalDialog(
            title = paste0("Change Konc. pool [nM] (", ck$name, ")"),
            size = "s",
            numericInput("pool_cmax", paste0("Konc. pool [nM] (", pool_default_cmax, " standard)"), value = ck$cmax, min = 0, step = 0.05),
            uiOutput("pool_cmax_msg"),
            footer = prompt_footer("pool_cmax"),
            easyClose = FALSE
        ))
    })
    output$pool_cmax_msg <- renderUI(error_text(pool_cmax_err()))
    observeEvent(input$pool_cmax_cancel, removeModal())
    pool_cmax_apply <- function(below) {
        v <- input$pool_cmax
        if (bad_number(v)) {
            pool_cmax_err("Konc. pool [nM] must be a number above 0.")
            return()
        }
        session$sendCustomMessage("setPoolCmax", list(idx = pool_cmax_idx(), cmax = v, below = below))
        removeModal()
    }
    observeEvent(input$pool_cmax_ok, pool_cmax_apply(FALSE))
    observeEvent(input$pool_cmax_ok_below, pool_cmax_apply(TRUE))
    
    # Reset: start over with empty dropdowns
    # (one empty row, and "Antal samples" is emptied)
    observeEvent(input$reset, {
        for (c in seq_len(K)) {
            st[[vk(c)]] <- character()
            st[[uk(c)]] <- logical()
        }
        st$n <- 0L
        st$file_rows <- logical()
        # the fields in the sidebar go back to what they were when the app was opened
        updateTextInput(session, "project_name", value = "NBS_NGS")
        updateTextInput(session, "cartridge_cycles", value = "200")
        updateNumericInput(session, "week", value = as.integer(format(Sys.Date(), "%V")))
        updateNumericInput(session, "year", value = as.integer(format(Sys.Date(), "%G")))
        session$sendCustomMessage("clearRows", list())
        add_rows(1)                            # back to one empty row, and no number of samples chosen
        updateNumericInput(session, "n_rows", value = NA)
    })
    
    # The file pickers are redrawn (which clears the chosen files) when "Nulstil" is clicked
    # or when the choice of sample IDs is cancelled
    ids_picker  <- reactiveVal(0)
    ids_pending <- reactiveVal(NULL)     # the IDs of the file while you choose which of them to use
    ids_error   <- reactiveVal("")
    output$ids_file_ui <- renderUI({
        input$reset
        ids_picker()
        tagList(
            # Two buttons: one takes every sample ID of the file, the other asks which ones to take
            fileInput("ids_file", "Puncherfil", buttonLabel = "All IDs...",
                      placeholder = "No file selected", accept = c(".xml", "text/xml", ".txt", "text/plain")),
            fileInput("ids_file_sel", NULL, buttonLabel = "Choose IDs...",
                      placeholder = "No file selected", accept = c(".xml", "text/xml", ".txt", "text/plain"))
        )
    })
    
    # The sample IDs of a .txt file: nothing but IDs, separated by a newline, tab, space, comma or semicolon.
    # (line breaks always separate too, so a comma-separated file can run over several lines)
    id_separators <- c(Newline = "newline", Tab = "tab", Space = "space", Comma = "comma", Semicolon = "semicolon")
    split_ids <- function(path, sep = "newline") {
        x <- paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
        x <- sub("^\ufeff", "", x)
        pattern <- switch(sep, tab = "[\t\r\n]+", space = "[ \r\n]+", comma = "[,\r\n]+",
                          semicolon = "[;\r\n]+", "[\r\n]+")
        x <- trimws(strsplit(x, pattern)[[1]])
        x[x != ""]
    }
    
    # Read the sample IDs of a file (NULL, with a message, if it can't be used)
    read_ids <- function(path) {
        # .txt: nothing but IDs, one per line. Anything else is read as a puncher (xml) file.
        plade <- ""                              # the plate ID of the file (only a puncher file has one)
        ids <- tryCatch({
            if (tolower(tools::file_ext(path)) == "txt") {
                split_ids(path)
            } else {
                pu <- getPunched(path)
                pc <- pu$pladeID
                plade <- if (length(pc) == 0 || is.na(pc[1])) "" else trimws(pc[1])
                trimws(pu$sampleID)
            }
        }, error = function(e) {
            showNotification(paste("Could not read the file:", conditionMessage(e)), type = "error")
            NULL
        })
        if (is.null(ids)) return(NULL)
        if (length(ids) == 0) {
            showNotification("No sample IDs found in the file.", type = "error")
            return(NULL)
        }
        if (length(ids) > max_file_ids) {
            showNotification(paste0("The file has ", length(ids), " sample IDs, at most ", max_file_ids,
                                    " are allowed."), type = "error")
            ids_picker(ids_picker() + 1)       # clear the file pickers
            return(NULL)
        }
        attr(ids, "plade") <- plade
        ids
    }
    
    # "All IDs...": every sample ID of the file is loaded
    observeEvent(input$ids_file, {
        ids <- read_ids(input$ids_file$datapath)
        if (!is.null(ids)) load_ids(ids, input$ids_file$name)
    })
    
    # "Choose IDs...": ask which of the sample IDs to use
    observeEvent(input$ids_file_sel, {
        ids <- read_ids(input$ids_file_sel$datapath)
        if (is.null(ids)) return()
        is_txt <- tolower(tools::file_ext(input$ids_file_sel$name)) == "txt"
        ids_pending(list(ids = ids, name = input$ids_file_sel$name, path = input$ids_file_sel$datapath, txt = is_txt))
        ids_error("")
        showModal(modalDialog(
            title = "Choose sample IDs",
            size = "s",
            if (is_txt) radioButtons("ids_sep", "The IDs in the file are separated by", choices = id_separators,
                                     selected = "newline", inline = TRUE),   # only for .txt files
            uiOutput("ids_count"),
            textInput("ids_sel", "Which ones?", value = "all"),
            tags$small(class = "text-muted", "\"all\", \"2\", \"2-5\", \"2,4,6\" or \"2-5,7,9-11\""),
            uiOutput("ids_sel_msg"),
            footer = tagList(actionButton("ids_cancel", "Cancel", class = "btn-sm"),
                             actionButton("ids_ok", "OK", class = "btn-primary btn-sm")),
            easyClose = FALSE
        ))
    })
    
    # The IDs of the file while the prompt is open: a .txt file is split again when the separator changes
    ids_current <- reactive({
        pend <- ids_pending()
        if (is.null(pend)) return(NULL)
        if (!pend$txt) return(pend$ids)
        ids <- tryCatch(split_ids(pend$path, input$ids_sep %||% "newline"), error = function(e) character())
        attr(ids, "plade") <- ""
        ids
    })
    output$ids_count <- renderUI(p(paste0("The file has ", length(ids_current()), " sample IDs.")))
    output$ids_sel_msg <- renderUI(error_text(ids_error()))
    
    observeEvent(input$ids_cancel, {
        removeModal()
        ids_pending(NULL)
        ids_picker(ids_picker() + 1)         # clear the file picker
    })
    
    observeEvent(input$ids_ok, {
        pend <- ids_pending()
        ids  <- ids_current()
        if (is.null(pend)) return()
        if (length(ids) == 0) {
            ids_error("No sample IDs found (is the separator right?)")
            return()
        }
        if (length(ids) > max_file_ids) {
            ids_error(paste0("At most ", max_file_ids, " sample IDs are allowed."))
            return()
        }
        sel <- parse_selection(input$ids_sel, length(ids))
        if (!sel$ok) {
            ids_error(sel$msg)                 # stay in the prompt
            return()
        }
        removeModal()
        ids_pending(NULL)
        chosen_ids <- ids[sel$idx]
        attr(chosen_ids, "plade") <- attr(ids, "plade")           # (picking IDs drops the plate ID)
        load_ids(chosen_ids, pend$name)
    })
    
    # Load the chosen Sample IDs: one row per ID
    load_ids <- function(ids, file_name) {
        n <- length(ids)                       # number of sample IDs to load
        
        # The IDs always come in new rows below the rows that are already there (see rows_above_new)
        start <- rows_above_new()
        new_rows <- start + seq_len(n)
        
        # Keep a number of rows you have chosen (unless there are more IDs than that)
        chosen <- mode_n()
        target <- if (is.na(chosen)) max(new_rows) else max(chosen, max(new_rows))
        
        resize(target)                         # right number of rows (plate/well picks are kept)
        
        # These rows get the IDs from the file. They don't start a count: the rows below
        # only count on from an ID you type in by hand.
        v  <- st[[vk(sample_col)]]
        u  <- st[[uk(sample_col)]]
        ff <- st$file_rows
        v[new_rows]  <- ids
        u[new_rows]  <- FALSE
        ff[new_rows] <- TRUE
        nv  <- derive_col(sample_col, v, u, ff)
        chg <- which(nv != st[[vk(sample_col)]])      # cells that differ from what the browser shows
        if (length(chg)) {
            session$sendCustomMessage("setCells", list(col = sample_col, idx = chg, val = nv[chg]))
        }
        st[[vk(sample_col)]] <- nv
        st[[uk(sample_col)]] <- u
        st$file_rows <- ff
        
        # The plate ID of the file goes in the Plade ID column of these rows (the first one counts as typed,
        # so the rows below it copy it, like any Plade ID you type yourself)
        plade <- attr(ids, "plade") %||% ""
        if (nzchar(plade)) {
            pv <- st[[vk(pladeid_col)]]
            pu <- st[[uk(pladeid_col)]]
            pv[new_rows] <- plade
            pu[new_rows] <- FALSE
            pu[new_rows[1]] <- TRUE
            npv  <- derive_col(pladeid_col, pv, pu)
            pchg <- which(npv != st[[vk(pladeid_col)]])
            if (length(pchg)) {
                session$sendCustomMessage("setCells", list(col = pladeid_col, idx = pchg, val = npv[pchg]))
            }
            st[[vk(pladeid_col)]] <- npv
            st[[uk(pladeid_col)]] <- pu
        }
        
        updateNumericInput(session, "n_rows", value = target)
        msg <- paste0(n, " sample IDs loaded from ", file_name, if (start > 0) paste0(" (added in new rows below the ", start, " existing rows).") else ".")
        if (!is.na(chosen) && target > chosen) {
            msg <- paste0(msg, " The number of samples was increased to ", target, " to fit them all.")
        }
        showNotification(msg, type = "message")
    }
    
    # ---- Save everything that is filled in to a csv file, and read such a file back in ----
    # The file is meant to be read by people too. It uses ";" between the cells (so it opens in Excel
    # with a Danish setup) and has three parts: the settings, the SAMPLES table (one line per row) and
    # the POOLS table (one line per pool). Reading it in also works with "," or tab between the cells.
    # Two last columns of the SAMPLES table are for the app: "From ID file" and "By hand (column numbers)"
    # (which cells you typed or picked yourself, so the auto-filled ones keep being auto-filled).
    data_picker <- reactiveVal(0)
    output$data_file_ui <- renderUI({
        input$reset
        data_picker()
        fileInput("data_file", "Datafil", buttonLabel = "Load data...",
                  placeholder = "No file selected", accept = c(".csv", "text/csv"))
    })
    
    # One line of a csv file (cells with the separator, a quote or a line break in them get quotes)
    csv_line <- function(x, sep = ";") {
        x <- as.character(x)
        x[is.na(x)] <- ""
        need <- grepl(paste0("[", sep, "\"\n\r]"), x)
        x[need] <- paste0("\"", gsub("\"", "\"\"", x[need], fixed = TRUE), "\"")
        paste(x, collapse = sep)
    }
    # c(1,2,3,5) -> "1-3,5"  (read back with parse_selection)
    rows_text <- function(r) {
        r <- sort(unique(as.integer(r)))
        if (!length(r)) return("")
        grp <- cumsum(c(1, diff(r) != 1))
        paste(vapply(split(r, grp), function(g) if (length(g) == 1) as.character(g) else paste0(g[1], "-", g[length(g)]),
                     character(1)), collapse = ",")
    }
    
    output$save_data <- downloadHandler(
        filename = function() dated(paste0("_", gsub("[^A-Za-z0-9_-]", "_", input$project_name %||% ""), "_data.csv")),
        content  = function(file) {
            n  <- st$n
            nr <- input$n_rows
            L  <- c(csv_line(c("Sample sheet builder data file", "1")),
                    csv_line(c("Project name", input$project_name %||% "")),
                    csv_line(c("Week", input$week %||% "")),
                    csv_line(c("Year", input$year %||% "")),
                    csv_line(c("Cartridge cycles", input$cartridge_cycles %||% "")),
                    csv_line(c("Antal samples", if (is.null(nr) || is.na(nr)) "" else nr)),
                    "", "SAMPLES",
                    csv_line(c("Row", "Well", col_names_plain, "From ID file", "By hand (column numbers)")))
            if (n > 0) {
                hand <- vapply(seq_len(n), function(i) {
                    paste(which(vapply(seq_len(K), function(cc) isTRUE(st[[uk(cc)]][i]) && cc != calc_col, logical(1))), collapse = " ")
                }, character(1))
                for (i in seq_len(n)) {
                    vals <- vapply(seq_len(K), function(cc) st[[vk(cc)]][i], character(1))
                    L <- c(L, csv_line(c(i, well_label(i), vals, if (isTRUE(st$file_rows[i])) "yes" else "", hand[i])))
                }
            }
            L <- c(L, "", "POOLS",
                   csv_line(c("Pool", "Rows", label_plain(pool_heads), "V [\u03bcl]", "c_target [nM]", "Pool ID set by hand", "Konc. pool max [nM]")))
            pl <- input$pools$pools
            for (i in seq_along(pl)) {
                q     <- pl[[i]]
                cells <- as.character(unlist(q$cells))
                length(cells) <- 11
                cells[is.na(cells)] <- ""
                L <- c(L, csv_line(c(paste("Pool", i), rows_text(unlist(q$rows)), cells, q$x %||% pool_default_v, q$y %||% pool_default_ctar,
                                     if (isTRUE(q$nameUser)) "yes" else "", q$cmax %||% pool_default_cmax)))
            }
            txt <- paste0(paste(L, collapse = "\r\n"), "\r\n")
            con <- file(file, "wb")
            on.exit(close(con))
            writeBin(as.raw(c(0xEF, 0xBB, 0xBF)), con)            # so Excel reads the special characters right
            writeBin(charToRaw(enc2utf8(txt)), con)
        }
    )
    
    observeEvent(input$data_file, {
        fail <- function(msg) {
            showNotification(msg, type = "error", duration = 10)
            data_picker(data_picker() + 1)         # clear the file picker
        }
        tryCatch(load_data_file(input$data_file$datapath, input$data_file$name, fail),
                 error = function(e) fail(paste("Could not load the file:", conditionMessage(e))))
    })
    
    load_data_file <- function(path, file_name, fail) {
        L <- readLines(path, warn = FALSE, encoding = "UTF-8")
        bad <- !validUTF8(L)                      # a file saved by Excel may use another encoding
        if (any(bad)) L[bad] <- iconv(L[bad], "latin1", "UTF-8", sub = "?")
        if (!length(L)) return(fail("The file is empty."))
        L[1] <- sub("^\ufeff", "", L[1])
        cnt <- c(";" = 0, "," = 0, "\t" = 0)
        for (s in names(cnt)) cnt[s] <- nchar(gsub(paste0("[^", s, "]"), "", L[1]))
        sep <- names(cnt)[which.max(cnt)]
        if (max(cnt) == 0) return(fail("Could not read the file. Use a file saved with \"Save data (csv)\"."))
        first <- sub(paste0("[", sep, "].*$"), "", L)
        first <- trimws(gsub("\"", "", first))
        blank <- grepl("^[;,\t \"]*$", L)
        s <- which(first == "SAMPLES")[1]
        p <- which(first == "POOLS")[1]
        if (is.na(s)) return(fail("Could not read the file. Use a file saved with \"Save data (csv)\"."))
        
        rd <- function(lines, header) {
            utils::read.csv(text = paste(lines, collapse = "\n"), header = header, sep = sep, quote = "\"",
                            colClasses = "character", na.strings = character(0), check.names = FALSE,
                            fill = TRUE, strip.white = FALSE, blank.lines.skip = TRUE)
        }
        block <- function(from) {                 # the lines under a marker, up to the next blank line
            if (is.na(from) || from >= length(L)) return(character())
            idx <- (from + 1):length(L)
            stop_at <- which(blank[idx])[1]
            if (!is.na(stop_at)) idx <- idx[seq_len(stop_at - 1)]
            L[idx]
        }
        
        # settings (the lines above SAMPLES)
        set_lines <- L[seq_len(s - 1)]
        set_lines <- set_lines[!blank[seq_len(s - 1)]]
        setting <- function(key) {
            for (ln in set_lines) {
                f <- rd(ln, FALSE)
                if (ncol(f) >= 1 && trimws(f[1, 1]) == key) return(if (ncol(f) >= 2) f[1, 2] else "")
            }
            ""
        }
        
        # the SAMPLES table: Row, Well, one column per column of the grid, From ID file, By hand
        sl <- block(s)
        if (length(sl) < 2) return(fail("The file has no sample rows."))
        d <- rd(sl, TRUE)
        if (ncol(d) < K + 2) return(fail("The SAMPLES table does not have the right number of columns."))
        n <- nrow(d)
        if (n < 1 || n > max_rows) return(fail("The file has no valid number of rows."))
        nv <- lapply(seq_len(K), function(cc) {
            x <- d[[2 + cc]]
            x[trimws(x) == ""] <- col_default[cc]
            x
        })
        nu <- lapply(seq_len(K), function(cc) rep(FALSE, n))
        has_hand <- ncol(d) >= K + 4
        for (cc in seq_len(K)) {
            if (cc == calc_col) next
            if (has_hand) {
                nu[[cc]] <- vapply(strsplit(trimws(d[[K + 4]]), "[^0-9]+"), function(tk) cc %in% suppressWarnings(as.integer(tk)), logical(1))
            } else {
                nu[[cc]] <- d[[2 + cc]] != "" & d[[2 + cc]] != col_default[cc]
            }
        }
        ff <- if (ncol(d) >= K + 3) is_yes(d[[K + 3]]) else rep(FALSE, n)
        nv[[calc_col]] <- calc_from(nv[[conc_col]], nv[[div_col]])
        
        # Like the sample ID files: the rows come in new rows below the ones already in the app, except
        # that the file replaces a single empty row (or nothing at all)
        start <- rows_above_new()
        
        # the POOLS table (pools without existing rows are dropped)
        pools <- list()
        pl <- block(p)
        if (length(pl) >= 2) {
            pd <- rd(pl, TRUE)
            cell <- function(r, j) if (j <= ncol(pd)) pd[r, j] else ""
            num  <- function(x, default) { v <- suppressWarnings(as.numeric(sub(",", ".", trimws(x), fixed = TRUE))); if (is.na(v) || v <= 0) default else v }
            for (r in seq_len(nrow(pd))) {
                sel <- parse_selection(trimws(cell(r, 2)), n)
                if (!isTRUE(sel$ok) || !length(sel$idx)) next
                pools[[length(pools) + 1]] <- list(
                    rows = as.list(sel$idx + start), name = cell(r, 3), nameUser = is_yes(cell(r, 16)),
                    text = cell(r, 4), text2 = cell(r, 8), pct = cell(r, 10),
                    x = num(cell(r, 14), pool_default_v), y = num(cell(r, 15), pool_default_ctar),
                    cmax = num(cell(r, 17), pool_default_cmax))
            }
        }
        
        # put it all in the app
        if (start == 0L) {                       # replaces what is there (settings too)
            for (cc in seq_len(K)) { st[[vk(cc)]] <- nv[[cc]]; st[[uk(cc)]] <- nu[[cc]] }
            st$file_rows <- ff
            st$n <- as.integer(n)
            session$sendCustomMessage("clearRows", list())
            session$sendCustomMessage("addRows", list(start = 1L, v = nv))
            session$sendCustomMessage("setPools", list(pools = pools, append = FALSE))
            updateTextInput(session, "project_name", value = setting("Project name"))
            updateTextInput(session, "cartridge_cycles", value = setting("Cartridge cycles"))
            wk <- suppressWarnings(as.integer(setting("Week"))); yr <- suppressWarnings(as.integer(setting("Year")))
            if (!is.na(wk)) updateNumericInput(session, "week", value = wk)
            if (!is.na(yr)) updateNumericInput(session, "year", value = yr)
            nr <- suppressWarnings(as.integer(setting("Antal samples")))
            updateNumericInput(session, "n_rows", value = if (is.na(nr)) NA else nr)
        } else {                                 # added below the rows that are there (the settings stay as they are)
            for (cc in seq_len(K)) {
                st[[vk(cc)]] <- c(st[[vk(cc)]], nv[[cc]])
                st[[uk(cc)]] <- c(st[[uk(cc)]], nu[[cc]])
            }
            st$file_rows <- c(st$file_rows, ff)
            st$n <- as.integer(start + n)
            session$sendCustomMessage("addRows", list(start = start + 1L, v = nv))
            session$sendCustomMessage("setPools", list(pools = pools, append = TRUE))
            if (!is.na(mode_n())) updateNumericInput(session, "n_rows", value = start + n)
        }
        for (r in which(nu[[div_col]])) session$sendCustomMessage("markCalc", list(row = start + r, on = TRUE))
        data_picker(data_picker() + 1)           # clear the file picker
        showNotification(paste0("Loaded ", n, " rows", if (length(pools)) paste0(" and ", length(pools), " pool(s)") else "",
                                " from ", file_name, if (start > 0) paste0(" (added below the ", start, " existing rows)") else "", "."), type = "message")
    }
    
    # Run name like NBS_NGS_W39_2026 (NA if project, week or year is invalid)
    run_name <- reactive({
        proj <- trimws(input$project_name %||% "")
        wk   <- input$week %||% NA
        yr   <- input$year %||% NA
        if (proj == "" || grepl(",", proj, fixed = TRUE)) return(NA_character_)
        if (is.na(wk) || wk != round(wk) || wk < 1 || wk > 53) return(NA_character_)
        if (is.na(yr) || yr != round(yr) || yr < 2000 || yr > 2100) return(NA_character_)
        sprintf("%s_W%02d_%d", proj, as.integer(wk), as.integer(yr))
    })
    
    # Read1Cycles = Read2Cycles = x / 2 + 1, where x is the cartridge cycles (NA if invalid)
    read_cycles <- reactive({
        x <- suppressWarnings(as.numeric(trimws(input$cartridge_cycles %||% "")))
        if (length(x) != 1 || is.na(x) || !is.finite(x)) return(NA_real_)
        r <- x / 2 + 1
        if (r != round(r) || r < 1) NA_real_ else r
    })
    read_cycles_text <- reactive({
        r <- read_cycles()
        if (is.na(r)) NA_character_ else sprintf("%.0f", r)
    })
    
    # Cartridge cycles: is the number odd (or not a whole number)? Then it is red, with a message below
    # (an empty or non-numeric field is not "odd": that one is reported as missing by the download buttons)
    cycles_not_even <- reactive({
        x <- suppressWarnings(as.numeric(trimws(input$cartridge_cycles %||% "")))
        length(x) == 1 && is.finite(x) && x != 2 * round(x / 2)
    })
    output$cycles_msg <- renderUI(if (cycles_not_even()) error_text("Must be even"))
    observe(session$sendCustomMessage("setBad", list(id = "cartridge_cycles", bad = cycles_not_even())))
    
    # Is each sheet ready to download? These are cheap checks (the slower index lookup only runs when
    # you download). The result is sent to the browser, which switches the button on/off and shows the
    # message below it. Only changes are sent.
    status_msg <- reactiveVal("")
    last_state <- new.env()
    last_msg   <- new.env()
    
    set_sheet_state <- function(id, fill, other = character()) {
        fill <- unique(fill)
        msg  <- ""
        if (length(fill)) msg <- udfyld_text(fill)
        if (length(other)) msg <- paste(c(if (nzchar(msg)) paste0(msg, "."), other), collapse = " ")
        ok  <- length(fill) == 0 && length(other) == 0
        key <- paste(ok, msg)
        if (!identical(last_state[[id]], key)) {
            last_state[[id]] <- key
            last_msg[[id]]   <- list(id = id, ok = ok, msg = msg)
            session$sendCustomMessage("sheetState", last_msg[[id]])
        }
    }
    
    # Send the states once more after the page has been set up for the first time
    session$onFlushed(function() {
        for (id in ls(last_msg)) session$sendCustomMessage("sheetState", last_msg[[id]])
    }, once = TRUE)
    
    observe({
        sample <- st[[vk(sample_col)]]
        plate  <- st[[vk(plate_col)]]
        well   <- st[[vk(well_col)]]
        screen <- st[[vk(screen_col)]]
        conc   <- st[[vk(conc_col)]]
        
        complete <- sample != "" & plate != "" & well != ""
        partial  <- sum(!complete & (sample != "" | plate != "" | well != ""))
        
        # --- what has to be filled in (names as shown in the app) ---
        # Sample ID, plate and well: at least one row where all three are filled in
        cols_missing <- character()
        if (!any(complete)) {
            empty <- required_cols[vapply(required_cols, function(c) all(st[[vk(c)]] == ""), logical(1))]
            cols_missing <- col_names_plain[if (length(empty)) empty else required_cols]
        }
        # "Sygdom" and the concentration: filled in for every row that goes into the sheet
        screen_missing <- if (any(complete & trimws(screen) == "")) col_names_plain[screen_col] else character()
        
        # Concentration sheet: Sample ID, DNA konc. and ng/\u03bcl, filled in for every row that has a Sample ID
        # (plate and well are not needed there)
        dna        <- st[[vk(dnakonc_col)]]
        has_sample <- sample != ""
        # ... and "Pool Konc. [ng/\u03bcl]" of the pool section: every one of those rows must be in a pool that has it
        # (checked on Konc. [nM], which is calculated from it)
        pool_conc <- pool_nM_by_row(length(sample))
        pool_label <- "Pool Konc. [ng/\u03bcl]"
        conc3_missing <- if (!any(has_sample)) c(col_names_plain[c(sample_col, dnakonc_col, conc_col)], pool_label) else c(
            if (any(has_sample & trimws(dna)  == "")) col_names_plain[dnakonc_col],
            if (any(has_sample & trimws(conc) == "")) col_names_plain[conc_col],
            if (any(has_sample & trimws(pool_conc) == "")) pool_label)
        other3 <- if (any(has_sample & grepl(",", sample, fixed = TRUE))) "Sample IDs can't contain commas." else character()
        
        # The same Sample ID in more than one row: those cells are red, and the sheets that need Sample ID say so
        same_id   <- has_sample & (duplicated(sample) | duplicated(sample, fromLast = TRUE))
        session$sendCustomMessage("markBad", list(cols = sample_col, rows = as.list(which(same_id))))
        id_dup_msg <- if (any(same_id)) "The same Sample ID is used by more than one row." else character()
        other3 <- c(other3, id_dup_msg)
        
        # Project name, week, year and cartridge cycles (the fields in the sidebar)
        proj <- trimws(input$project_name %||% "")
        wk   <- input$week %||% NA
        yr   <- input$year %||% NA
        proj_missing <- if (proj == "") "Project name" else character()
        run_missing  <- c(if (is.na(wk) || wk != round(wk) || wk < 1 || wk > 53) "Week",
                          if (is.na(yr) || yr != round(yr) || yr < 2000 || yr > 2100) "Year",
                          if (is.na(read_cycles()) && !cycles_not_even()) "Cartridge cycles")
        
        # --- other problems (not about something left empty) ---
        other <- character()
        if (grepl(",", proj, fixed = TRUE)) other <- c(other, "The project name can't contain commas.")
        if (cycles_not_even()) other <- c(other, "Cartridge cycles must be even.")
        other <- c(other, id_dup_msg)
        
        # The same plate + well must not be used by more than one row
        used <- which(plate != "" & well != "")
        key  <- paste(plate[used], well[used], sep = "\t")
        dup  <- unique(key[duplicated(key)])
        # every row that shares its plate and well with another row gets red Plade ID and Index Br\u00f8nd cells
        session$sendCustomMessage("markBad", list(cols = c(plate_col, well_col),
                                                  rows = as.list(used[key %in% dup])))
        if (length(dup) > 0) {
            other <- c(other, "The same plate and well is used by more than one row.")
        }
        if (any(complete) && any(grepl(",", sample[complete], fixed = TRUE))) {
            other <- c(other, "Sample IDs can't contain commas.")
        }
        other_common <- other[!other %in% c("The project name can't contain commas.", "Cartridge cycles must be even.")]
        
        # --- the four buttons ---
        set_sheet_state("download",                                               # sample sheet
                        c(cols_missing, proj_missing, run_missing), other)
        set_sheet_state("download2",                                              # IEM sample sheet
                        c(cols_missing, screen_missing, proj_missing),
                        other[other != "Cartridge cycles must be even."])      # the IEM sheet has no cycles
        set_sheet_state("download3",                                              # concentration sheet
                        conc3_missing, other3)
        
        # Plate layout: the only requirement is the concentration (ng/\u03bcl); every row with a
        # concentration needs its own position, so no two of them may have the same well label
        rows_c <- which(trimws(conc) != "")
        other4 <- character()
        if (length(rows_c) > 0 && anyDuplicated(well_label(rows_c))) {
            other4 <- paste0("Max. ", length(well_names), " rows.")   # short, it has to fit the one-line message area
        }
        set_sheet_state("download4",
                        if (length(rows_c) == 0) col_names_plain[conc_col] else character(), other4)
        
        warn <- character()
        if (is.null(index_db)) {
            warn <- "indexDB.xlsx could not be read, so the index values will be empty."
        }
        if (partial > 0) warn <- c(warn, paste0(partial, " partly filled in row(s) will be left out."))
        status_msg(paste(warn, collapse = " "))
    })
    
    output$status_ui <- renderUI({
        m <- status_msg()
        if (m != "") div(class = "text-warning", style = "margin-top: 8px;", m)
    })
    
    # The rows that go into the sample sheets. Only completely filled-in rows are included
    # (empty rows are skipped silently).
    export_data <- reactive({
        sample <- st[[vk(sample_col)]]
        plate  <- st[[vk(plate_col)]]
        well   <- st[[vk(well_col)]]
        screen <- st[[vk(screen_col)]]
        conc   <- st[[vk(conc_col)]]
        pool   <- st[[vk(calc_col)]]
        
        complete <- sample != "" & plate != "" & well != ""
        pos    <- which(complete)                 # row number, used for the Well label in the IEM sheet
        sample <- sample[complete]; plate <- plate[complete]; well <- well[complete]
        screen <- screen[complete]; conc <- conc[complete]; pool <- pool[complete]
        
        data.frame(Sample_ID = sample, Pos = pos, Plate = plate, Well = well, Screen = screen,
                   Conc = conc, Pool = pool, stringsAsFactors = FALSE)
    })
    
    # Rows of the concentration sheet: every row that has a Sample ID
    export_data3 <- reactive({
        sample <- st[[vk(sample_col)]]
        keep   <- which(sample != "")
        data.frame(Sample_ID = sample[keep], Pos = keep,
                   Dna  = st[[vk(dnakonc_col)]][keep],
                   Conc = st[[vk(conc_col)]][keep], stringsAsFactors = FALSE)
    })
    
    # Write the sample sheet to `file`
    write_sheet <- function(file) {
        d     <- export_data()
        idx   <- lookup_indexes(d$Plate, d$Well, c(7, 5))   # columns 7 and 5 of indexDB.xlsx
        rows  <- paste(d$Sample_ID, blank_na(idx[[1]]), blank_na(idx[[2]]), sep = ",")
        lines <- c(sheet_header(run_name(), read_cycles_text()), rows)
        # Binary connection so the file always uses plain "\n" line endings, like the example
        con <- file(file, "wb")
        on.exit(close(con))
        writeLines(lines, con, sep = "\n", useBytes = TRUE)
    }
    
    output$download <- downloadHandler(
        filename = function() dated("_SampleSheet.csv"),
        content  = function(file) write_sheet(file)
    )
    
    # Second sample sheet: IEM format, comma separated, Windows line endings (like the example)
    write_sheet2 <- function(file) {
        d       <- export_data()
        idx     <- lookup_indexes(d$Plate, d$Well, c(7, 6))   # columns 7 and 6 of indexDB.xlsx
        project <- trimws(input$project_name)
        rows <- paste(d$Sample_ID,                 # Sample_ID
                      d$Sample_ID,                 # Sample_Name
                      project,                     # Sample_Plate
                      well_label(d$Pos),           # Sample_Well (the "Well" numbering of the row)
                      d$Plate,                     # Index_Plate_ID
                      d$Well,                      # Index_Plate_Well
                      blank_na(idx[[1]]),          # index
                      blank_na(idx[[2]]),          # index2
                      project,                     # Sample_Project
                      d$Screen,                    # Description
                      "",                          # Lane (left empty)
                      sep = ",")
        lines <- c(iem_header(format(Sys.Date(), iem_date_format)), rows)
        con <- file(file, "wb")
        on.exit(close(con))
        writeLines(lines, con, sep = "\r\n", useBytes = TRUE)
    }
    
    output$download2 <- downloadHandler(
        filename = function() dated("_SampleSheet_IEM.csv"),
        content  = function(file) write_sheet2(file)
    )
    
    # Third sample sheet: sample ID, DNA concentration (DNA konc.), library concentration (ng/\u03bcl) and capture nM.
    # capt_nM is the "Konc. nM" of the pool that contains the row (empty if the row is in no pool).
    # Comma separated, UTF-8 (the column names contain the Greek letter mu).
    write_sheet3 <- function(file) {
        d      <- export_data3()
        header <- enc2utf8(paste(c("sampleID", "dna_konc_ng_\u03bcl", "lib_ng_\u03bcl", "capt_nM"),
                                 collapse = ","))
        capt   <- pool_nM_by_row(max(c(0L, d$Pos)))     # Konc. [nM] of the pool each row is in
        rows   <- paste(d$Sample_ID, dot_decimal(d$Dna), dot_decimal(d$Conc), capt[d$Pos], sep = ",")
        con <- file(file, "wb")
        on.exit(close(con))
        writeBin(as.raw(c(0xEF, 0xBB, 0xBF)), con)   # byte order mark: tells Excel the file is UTF-8
        writeLines(c(header, rows), con, sep = "\n", useBytes = TRUE)
    }
    
    # Fourth file: the 96-well plate layout as an Excel file (a csv can't hold formatting or print settings).
    # Two tables one under the other (ng/\u03bcl, then \u03bcl), each with the columns 1-12 across and the rows
    # A-H down. A row of the app goes in the cell that matches its well label (B02 -> row B, column 2).
    # "Samlet pool" is written to the right of the second table. Needs the openxlsx package.
    write_sheet4 <- function(file) {
        conc   <- st[[vk(conc_col)]]                # every row of the app, not only complete ones
        pool   <- st[[vk(calc_col)]]
        lab    <- well_label(seq_along(conc))
        r      <- match(substr(lab, 1, 1), LETTERS[1:8])
        cc     <- suppressWarnings(as.integer(substring(lab, 2)))
        ok     <- !is.na(r) & !is.na(cc) & cc >= 1 & cc <= 12 & trimws(conc) != ""
        
        plate_matrix <- function(values) {
            m <- matrix("", nrow = 8, ncol = 12)
            m[cbind(r[ok], cc[ok])] <- values[ok]
            m
        }
        # Numbers are written as numbers, empty positions as empty cells
        as_cells <- function(m) {
            df <- as.data.frame(m, stringsAsFactors = FALSE)
            for (j in seq_along(df)) {
                x <- df[[j]]
                blank <- trimws(x) == ""
                n <- suppressWarnings(as.numeric(x))
                df[[j]] <- if (all(blank | !is.na(n))) ifelse(blank, NA_real_, n) else ifelse(blank, NA_character_, x)
            }
            df
        }
        
        wb <- openxlsx::createWorkbook()
        sh <- "Plate layout"
        openxlsx::addWorksheet(wb, sh, gridLines = FALSE)
        
        cell     <- openxlsx::createStyle(halign = "center", border = "TopBottomLeftRight", borderStyle = "thin")
        cell_dec <- openxlsx::createStyle(numFmt = "0.0")
        bold     <- openxlsx::createStyle(textDecoration = "bold", halign = "center")
        
        # One table: title bar, header row (1-12), then rows A-H
        add_table <- function(top, title, m, colour, one_decimal = FALSE) {
            openxlsx::writeData(wb, sh, title, startRow = top, startCol = 1, colNames = FALSE)
            openxlsx::mergeCells(wb, sh, cols = 1:13, rows = top)
            openxlsx::addStyle(wb, sh, openxlsx::createStyle(textDecoration = "bold", fontSize = 12, halign = "center",
                                                             fgFill = colour, border = "TopBottomLeftRight",
                                                             borderStyle = "thin"),
                               rows = top, cols = 1:13, gridExpand = TRUE)
            openxlsx::writeData(wb, sh, as.data.frame(matrix(1:12, nrow = 1)), startRow = top + 1, startCol = 2, colNames = FALSE)
            openxlsx::writeData(wb, sh, LETTERS[1:8], startRow = top + 2, startCol = 1, colNames = FALSE)
            openxlsx::writeData(wb, sh, as_cells(m), startRow = top + 2, startCol = 2, colNames = FALSE)
            openxlsx::addStyle(wb, sh, cell, rows = top + 1:9, cols = 1:13, gridExpand = TRUE, stack = TRUE)
            openxlsx::addStyle(wb, sh, bold, rows = top + 1, cols = 1:13, gridExpand = TRUE, stack = TRUE)
            openxlsx::addStyle(wb, sh, bold, rows = top + 2:9, cols = 1, gridExpand = TRUE, stack = TRUE)
            openxlsx::addStyle(wb, sh, openxlsx::createStyle(fgFill = colour), rows = top + 1, cols = 1:13,
                               gridExpand = TRUE, stack = TRUE)
            openxlsx::addStyle(wb, sh, openxlsx::createStyle(fgFill = colour), rows = top + 2:9, cols = 1,
                               gridExpand = TRUE, stack = TRUE)
            if (one_decimal) {
                openxlsx::addStyle(wb, sh, cell_dec, rows = top + 2:9, cols = 2:13, gridExpand = TRUE, stack = TRUE)
            }
        }
        
        add_table(1,  "ng/\u03bcl", plate_matrix(dot_decimal(conc)), "#DDEBF7")
        add_table(12, "\u03bcl",    plate_matrix(pool),      "#E2EFDA", one_decimal = TRUE)
        
        # Samlet pool: to the right of the second table (same sum as the field in the app)
        pool_sum <- sum(suppressWarnings(as.numeric(st[[vk(calc_col)]])), na.rm = TRUE)
        openxlsx::writeData(wb, sh, "Samlet pool", startRow = 13, startCol = 15, colNames = FALSE)
        openxlsx::writeData(wb, sh, pool_sum,      startRow = 14, startCol = 15, colNames = FALSE)
        openxlsx::addStyle(wb, sh, openxlsx::createStyle(textDecoration = "bold", halign = "center", fgFill = "#E2EFDA",
                                                         border = "TopBottomLeftRight", borderStyle = "thin"),
                           rows = 13, cols = 15)
        openxlsx::addStyle(wb, sh, openxlsx::createStyle(halign = "center", numFmt = "0.0",
                                                         border = "TopBottomLeftRight", borderStyle = "thin"),
                           rows = 14, cols = 15)
        
        # Narrow columns, and print everything on one landscape page
        openxlsx::setColWidths(wb, sh, cols = 1,    widths = 4)
        openxlsx::setColWidths(wb, sh, cols = 2:13, widths = 7)
        openxlsx::setColWidths(wb, sh, cols = 14,   widths = 2)
        openxlsx::setColWidths(wb, sh, cols = 15,   widths = 12)
        openxlsx::pageSetup(wb, sh, orientation = "landscape", paperSize = 9,
                            fitToWidth = 1, fitToHeight = 1)
        
        openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
    }
    
    # The pool sheet (Excel, for printing): every pool line like in the app, with wider columns where
    # the text needs room. input$pools is sent by the browser (see sendPools() in the JavaScript).
    write_sheet5 <- function(file) {
        p      <- input$pools$pools
        totals <- input$pools$totals
        # The same two parts as in the app. Part 1: Pool ID up to and including Konc. seqklar [nM].
        # Part 2: a copy of Pool ID, then % af 24 ul and the rest. (A pool line has 11 values, in the order below.)
        heads <- label_plain(pool_heads)
        part1 <- 1:7
        part2 <- c(1, 8:11)
        n <- length(p)
        m <- matrix("", nrow = n, ncol = 11)
        for (i in seq_len(n)) {
            x <- as.character(unlist(p[[i]]$cells))
            length(x) <- 11
            x[is.na(x)] <- ""
            m[i, ] <- x
        }
        
        # Which pools have a changed value: V or c_target (Vol. Pool and Vol. Rsb), the maximum of Konc. pool [nM]
        differs <- function(v, default) {
            v <- suppressWarnings(as.numeric(v))
            is.na(v) || abs(v - default) > 1e-9
        }
        chg_xy <- vapply(p, function(q) differs(q$x %||% pool_default_v, pool_default_v) ||
                             differs(q$y %||% pool_default_ctar, pool_default_ctar), logical(1))
        chg_cm <- vapply(p, function(q) differs(q$cmax %||% pool_default_cmax, pool_default_cmax), logical(1))
        changed_fill <- openxlsx::createStyle(fgFill = "#FFD966")
        
        wb <- openxlsx::createWorkbook()
        sh <- "Pools"
        openxlsx::addWorksheet(wb, sh, gridLines = FALSE)
        thin <- "TopBottomLeftRight"
        
        openxlsx::writeData(wb, sh, "Koncentrationsbestemmelse post-capture PCT (HS-kit)", startRow = 1, startCol = 1, colNames = FALSE)
        openxlsx::mergeCells(wb, sh, cols = 1:8, rows = 1)
        openxlsx::addStyle(wb, sh, openxlsx::createStyle(textDecoration = "bold", fontSize = 14, halign = "left"),
                           rows = 1, cols = 1)
        
        # one part: labels at row `top`, the pools below. Numbers are written as numbers, the Pool ID as text.
        put_part <- function(top, idx) {
            nc <- length(idx) + 1
            openxlsx::writeData(wb, sh, as.data.frame(t(c("Pool", heads[idx]))), startRow = top, startCol = 1, colNames = FALSE)
            if (n > 0) {
                openxlsx::writeData(wb, sh, paste("Pool", seq_len(n)), startRow = top + 1, startCol = 1, colNames = FALSE)
                for (k in seq_along(idx)) {
                    col <- m[, idx[k]]
                    num <- suppressWarnings(as.numeric(sub(",", ".", col, fixed = TRUE)))
                    val <- if (idx[k] > 1 && all(is.na(num) == (col == ""))) num else col
                    openxlsx::writeData(wb, sh, val, startRow = top + 1, startCol = k + 1, colNames = FALSE)
                }
            }
            body <- top:(top + n)
            openxlsx::addStyle(wb, sh, openxlsx::createStyle(halign = "center", valign = "center", border = thin, borderStyle = "thin"),
                               rows = body, cols = 1:nc, gridExpand = TRUE, stack = TRUE)
            openxlsx::addStyle(wb, sh, openxlsx::createStyle(textDecoration = "bold", wrapText = TRUE, fgFill = "#D9D9D9"),
                               rows = top, cols = 1:nc, gridExpand = TRUE, stack = TRUE)
            if (n > 0) {
                openxlsx::addStyle(wb, sh, openxlsx::createStyle(textDecoration = "bold"),
                                   rows = top + seq_len(n), cols = 1, gridExpand = TRUE, stack = TRUE)
                openxlsx::addStyle(wb, sh, openxlsx::createStyle(halign = "left"),
                                   rows = top + seq_len(n), cols = 2, gridExpand = TRUE, stack = TRUE)
            }
            # colour the cells whose value has been changed
            for (k in seq_along(idx)) {
                hit <- if (idx[k] %in% c(4, 5)) chg_xy else if (idx[k] == 10) chg_cm else logical(n)
                if (any(hit)) {
                    openxlsx::addStyle(wb, sh, changed_fill, rows = top + which(hit), cols = k + 1, gridExpand = TRUE, stack = TRUE)
                }
            }
            openxlsx::setRowHeights(wb, sh, rows = top, heights = 32)
            if (n > 0) openxlsx::setRowHeights(wb, sh, rows = top + seq_len(n), heights = 20)
        }
        
        top1 <- 2
        top2 <- top1 + n + 2                            # one empty row between the two parts
        put_part(top1, part1)
        put_part(top2, part2)
        
        # under Vol. fortynding of part 2: the sum and RSB Vol.
        sum_num  <- suppressWarnings(as.numeric(totals$sum))
        rest_num <- suppressWarnings(as.numeric(totals$rest))
        r0 <- top2 + n + 1
        openxlsx::writeData(wb, sh, c("Sum", "RSB Vol."), startRow = r0, startCol = 5, colNames = FALSE)
        openxlsx::writeData(wb, sh, c(sum_num, rest_num), startRow = r0, startCol = 6, colNames = FALSE)
        openxlsx::addStyle(wb, sh, openxlsx::createStyle(textDecoration = "bold", halign = "right"),
                           rows = r0 + 0:1, cols = 5, gridExpand = TRUE, stack = TRUE)
        openxlsx::addStyle(wb, sh, openxlsx::createStyle(halign = "center", border = thin, borderStyle = "thin",
                                                         fgFill = "#E2EFDA", numFmt = "0.00"),
                           rows = r0 + 0:1, cols = 6, gridExpand = TRUE, stack = TRUE)
        openxlsx::setRowHeights(wb, sh, rows = 1, heights = 26)
        openxlsx::setRowHeights(wb, sh, rows = r0 + 0:1, heights = 20)
        if (any(chg_xy) || any(chg_cm)) {                 # explains the colour
            openxlsx::writeData(wb, sh, "= \u00e6ndret v\u00e6rdi (V, c_target eller Konc. pool [nM])", startRow = r0 + 3, startCol = 2, colNames = FALSE)
            openxlsx::addStyle(wb, sh, openxlsx::createStyle(fgFill = "#FFD966", border = thin, borderStyle = "thin"),
                               rows = r0 + 3, cols = 1, stack = TRUE)
        }
        
        # Wide Pool ID, normal width for the rest (the same columns in both parts), one landscape page
        openxlsx::setColWidths(wb, sh, cols = 1,     widths = 8)
        openxlsx::setColWidths(wb, sh, cols = 2,     widths = 30)
        openxlsx::setColWidths(wb, sh, cols = 3:8,   widths = 13)
        openxlsx::pageSetup(wb, sh, orientation = "landscape", paperSize = 9,
                            fitToWidth = 1, fitToHeight = 0)
        openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
    }
    
    output$download5 <- downloadHandler(
        filename = function() dated("_Pools.xlsx"),
        content  = function(file) write_sheet5(file)
    )
    
    output$download4 <- downloadHandler(
        filename = function() dated("_PlateLayout.xlsx"),
        content  = function(file) write_sheet4(file)
    )
    
    output$download3 <- downloadHandler(
        filename = function() dated("_Concentrations.csv"),
        content  = function(file) write_sheet3(file)
    )
    
    if (is.null(index_db)) {
        showNotification("Could not read indexDB.xlsx (it must be in the app folder). Index values will be empty.",
                         type = "error", duration = NULL)
    }
}

shinyApp(ui, server)