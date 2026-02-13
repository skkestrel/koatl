import itertools
import builtins

import koatl.std.iter.Iterable
import .control.(Ok, Err, Result)


# --- Exception type ---

export DebugIteratorError = class(Exception):
    """Raised when an unchecked DebugIterator pipeline encounters an error.
    Wraps the original exception and includes the debug grid."""
    __init__ = (self, message, iterator, cause) =>
        super().__init__(message)
        self.cause = cause
        self.iterator = iterator

# --- Cell types for grid rendering ---

_SKIP = "skip"
_EMPTY = "empty"
_UNKNOWN = "unknown"
_REPEAT = "repeat"
_VALUE = "value"
_ERROR = "error"

_Cell = class:
    __init__ = (self, kind, text="") =>
        self.kind = kind
        self.text = text

    is_protected = property! self =>
        """Cells that should not be overwritten by skip/repeat logic."""
        self.kind == _ERROR or self.kind == _UNKNOWN

    display = property! self =>
        """Render the cell as a display string."""
        if self.kind == _SKIP:
            return "[skip]"
        if self.kind == _UNKNOWN:
            return "[?]"
        if self.kind == _EMPTY:
            return "---"
        if self.kind == _REPEAT:
            return "..."
        self.text


# --- Helpers ---

_truncate_repr = (value, max_width) =>
    let s = repr(value)
    if len(s) <= max_width:
        return s
    s[..max_width - 3] + "..."




# --- Grid renderer ---

_DebugGrid = class:
    """
    Renders an ASCII grid from a list of DebugIteratorValue history entries.

    Each column corresponds to a pipeline stage. Cells are typed _Cell objects
    (value, error, skip, unknown, empty, repeat) so rendering never depends on
    string-content checks.
    """

    __init__ = (self, history, stages, tap_stages, flat_map_stages, consumed_count, history_limit, max_width=20) =>
        self._history = history
        self._stages = stages
        self._n = len(stages)
        self._tap_stages = tap_stages
        self._flat_map_stages = flat_map_stages
        self._consumed_count = consumed_count
        self._history_limit = history_limit
        self._max_width = max_width

    # --- Phase 1: build rows of _Cell objects from lineage data ---

    _build_rows = self =>
        """Convert history entries into (idx_label, row_dict, dbv) tuples."""
        let rows = []
        let n = self._n
        let max_width = self._max_width
        for dbv in self._history:
            let row = {}
            let first_known = None
            let last_known = -1
            let was_skipped_at = None

            for entry in dbv.lineage:
                let val = entry[0]
                let si = entry[1]
                if si === None:
                    continue
                if len(entry) > 2 and entry[2]:
                    row[si] = _Cell(_ERROR, _truncate_repr(val, max_width))
                    if was_skipped_at === None:
                        was_skipped_at = si
                else:
                    row[si] = _Cell(_VALUE, _truncate_repr(val, max_width))
                if first_known === None:
                    first_known = si
                last_known = si

            # Determine skip/unreached markers
            if dbv.is_skipped and was_skipped_at === None:
                was_skipped_at = last_known

            # Fill missing cells
            let skip_point_marked = False
            for i in ..n:
                if i not in row:
                    if first_known !== None and i < first_known:
                        row[i] = _Cell(_UNKNOWN)
                    elif dbv.is_skipped and i == was_skipped_at and not skip_point_marked:
                        row[i] = _Cell(_SKIP)
                        skip_point_marked = True
                    elif i > last_known and last_known >= 0:
                        if dbv.is_skipped:
                            row[i] = _Cell(_EMPTY)
                        else:
                            row[i] = _Cell(_VALUE, _truncate_repr(dbv.value, max_width))
                    else:
                        row[i] = _Cell(_EMPTY)

            # Override skip markers
            if dbv.is_skipped and was_skipped_at !== None and was_skipped_at >= 0:
                if not row[was_skipped_at].is_protected:
                    row[was_skipped_at] = _Cell(_SKIP)
                for i in ..n:
                    if i > was_skipped_at:
                        if not row[i].is_protected:
                            row[i] = _Cell(_EMPTY)

            # Build index label
            let idx_label
            if dbv.index_path !== None:
                idx_label = ".".join(list(dbv.index_path.iter.map(str)))
            else:
                idx_label = str(self._consumed_count - len(self._history) + len(rows))

            rows.append((idx_label, row, dbv))
        rows

    # --- Phase 2: suppress repeated parent cells for flat_map children ---

    _suppress_repeats = (self, rows) =>
        let sorted_fm = builtins.sorted(self._flat_map_stages)
        if len(sorted_fm) == 0:
            return None
        for ri in ..len(rows):
            let entry = rows[ri]
            let dbv = entry[2]
            if dbv.index_path === None or len(dbv.index_path) < 2:
                continue
            let path = dbv.index_path
            for level in ..len(sorted_fm):
                let all_zero = True
                for pl in range(level + 1, len(path)):
                    if path[pl] != 0:
                        all_zero = False
                        break
                if not all_zero:
                    let col_start = 0
                    if level > 0:
                        col_start = sorted_fm[level - 1]
                    let col_end = sorted_fm[level]
                    for ci in range(col_start, col_end):
                        let row = entry[1]
                        if ci in row and not row[ci].is_protected:
                            row[ci] = _Cell(_REPEAT)

    # --- Phase 3: compute column widths ---

    _compute_widths = (self, rows) =>
        """Return (col_widths, idx_w)."""
        let idx_w = 5
        let col_widths = []
        for i in ..self._n:
            col_widths.append(len(self._stages[i]))
        for entry in rows:
            let idx_label = entry[0]
            let row = entry[1]
            if len(idx_label) > idx_w:
                idx_w = len(idx_label)
            for i in ..self._n:
                if i in row:
                    let cell_len = len(row[i].text)
                    if cell_len > col_widths[i]:
                        if cell_len > self._max_width:
                            col_widths[i] = self._max_width
                        else:
                            col_widths[i] = cell_len
        (col_widths, idx_w)

    # --- Phase 4: render ASCII table ---

    _sep = (self, left_col, right_col) =>
        """Separator character between two columns (solid or dashed for taps)."""
        let left_is_tap = left_col !== None and left_col in self._tap_stages
        let right_is_tap = right_col !== None and right_col in self._tap_stages
        if left_is_tap or right_is_tap:
            return "┊"
        "│"

    _hfill = (self, col_idx, width) =>
        """Horizontal fill character (dashed for taps)."""
        if col_idx in self._tap_stages:
            return "╌" * width
        "─" * width

    _build_row = (self, cells, col_widths, idx_text) =>
        let result = idx_text + self._sep(None, 0)
        for i in ..len(cells):
            result = result + cells[i]
            if i < len(cells) - 1:
                result = result + self._sep(i, i + 1)
            else:
                result = result + self._sep(i, None)
        result

    _build_hborder = (self, col_widths, idx_w, left_char, mid_char, right_char) =>
        let parts = []
        for i in ..self._n:
            parts.append(self._hfill(i, col_widths[i] + 2))
        let result = " " * (idx_w + 2) + left_char
        for i in ..len(parts):
            result = result + parts[i]
            if i < len(parts) - 1:
                result = result + mid_char
        result + right_char

    _render_table = (self, rows, col_widths, idx_w) =>
        """Build the complete ASCII table from processed rows."""
        let n = self._n
        let total = self._consumed_count
        let shown = len(self._history)
        let is_truncated = total > shown and self._history_limit !== None

        # Header summary
        let header_text
        if is_truncated:
            header_text = f"DebugIterator (showing last {shown} of {total} consumed, {n} stages)"
        else:
            header_text = f"DebugIterator ({total} values, {n} stages)"

        let lines = [header_text]

        # Top border
        lines.append(self._build_hborder(col_widths, idx_w, "┌", "┬", "┐"))

        # Header row
        let idx_header = " " + "".center(idx_w) + " "
        let stage_headers = []
        for i in ..n:
            let w = col_widths[i]
            stage_headers.append(" " + self._stages[i].center(w) + " ")
        lines.append(self._build_row(stage_headers, col_widths, idx_header))

        # Header separator
        lines.append(self._build_hborder(col_widths, idx_w, "├", "┼", "┤"))

        # Truncation indicator row
        if is_truncated:
            let dots_cells = []
            for i in ..n:
                let w = col_widths[i]
                dots_cells.append(" " + "...".center(w) + " ")
            lines.append(self._build_row(dots_cells, col_widths, " " + "...".center(idx_w) + " "))

        # Data rows
        for entry in rows:
            let idx_label = entry[0]
            let row = entry[1]
            let idx_cell = " " + idx_label.rjust(idx_w) + " "
            let cells = []
            for i in ..n:
                let w = col_widths[i]
                let cell_val
                if i in row:
                    cell_val = row[i].display
                else:
                    cell_val = "---"
                if len(cell_val) > w:
                    cell_val = cell_val[..w - 3] + "..."
                cells.append(" " + cell_val.center(w) + " ")
            lines.append(self._build_row(cells, col_widths, idx_cell))

        # Bottom border
        lines.append(self._build_hborder(col_widths, idx_w, "└", "┴", "┘"))

        "\n".join(lines)

    # --- Main entry point ---

    render = self =>
        if len(self._history) == 0:
            return "DebugIterator (empty — no values consumed yet)"
        let rows = self._build_rows()
        self._suppress_repeats(rows)
        let widths = self._compute_widths(rows)
        self._render_table(rows, widths[0], widths[1])


# Dataclass representing a debug iterator value with its transformation lineage.
# Each lineage entry is a (displayed_value, stage_index) tuple, where stage_index
# is an int position in DebugIterator._stages, or None for unknown provenance.
export DebugIteratorValue = class:
    __init__ = (self, value=None, lineage=None, is_skipped=False, index_path=None) =>
        self.value = value
        self.lineage = lineage ?? []
        self.is_skipped = is_skipped
        self.index_path = index_path

    with_value = (self, new_value) =>
        DebugIteratorValue(new_value, self.lineage, self.is_skipped, self.index_path)

    with_lineage = (self, new_lineage) =>
        DebugIteratorValue(self.value, new_lineage, self.is_skipped, self.index_path)

    skip = (self) =>
        DebugIteratorValue(self.value, self.lineage, True, self.index_path)

    add_lineage_step = (self, val, stage_id, is_error=False) =>
        let entry = is_error then (val, stage_id, True) else (val, stage_id)
        DebugIteratorValue(self.value, [*self.lineage, entry], self.is_skipped, self.index_path)

    __repr__ = self =>
        f"DebugIteratorValue(value={repr(self.value)}, stages={len(self.lineage)}, skipped={self.is_skipped})"


# --- DebugIterator ---

export DebugIterator = class:
    """
    Lazily wraps an iterator pipeline, tracking the lineage of each value
    through transformation stages. Renders an ASCII grid for introspection.

    How it works:
        Each value flowing through the pipeline is wrapped in a
        DebugIteratorValue that carries a lineage — a list of
        (value, stage_index) pairs recording the value at each stage.

        Transform methods (.map, .filter, etc.) don't consume values
        eagerly. They create a new DebugIterator whose _source is a
        generator pulling from the parent's _source, transforming each
        DebugIteratorValue and appending a lineage step. This means
        the pipeline is a chain of nested generators.

        Only the outermost iterator's __next__ is called (by the
        consumer, e.g. list(di)). Each call pulls one value through
        the entire generator chain, then records the resulting
        DebugIteratorValue into _history and increments
        _consumed_count. Skipped values (filtered out, errors in
        checked mode, etc.) are recorded in history but not returned
        to the consumer — __next__ loops until it finds a non-skipped
        value.

        _history is a bounded list (controlled by the history
        parameter). When history=N, only the last N entries are kept;
        older entries are evicted. When history=None, all entries are
        kept. The grid is rendered from whatever is in _history at the
        time of display.

        All internal state (_stages, _checked, _history_limit,
        _tap_stages, _flat_map_stages, _error_cell) is shared across
        the entire chain via _wrap_transform, so the outermost
        iterator has access to the full stage list and can render the
        complete grid.

    Constructor parameters:
        source      The iterable to wrap.
        checked     If True, errors in transform functions (map, filter, etc.)
                    are caught and displayed as [ERR: ...] in the grid, allowing
                    the pipeline to continue. If False (default), errors propagate
                    normally — but the grid is printed before re-raising so you
                    can see where the failure occurred.
        history     Maximum number of rows to retain for the grid display.
                    Older rows are evicted as new ones arrive. Set to None to
                    keep all rows. Default: 5.

    Internal state (propagated through the transform chain):
        _stages          List[str] of stage names, one per column in the grid.
        _tap_stages      Set[int] of stage indices that are tap columns
                         (rendered with dashed ┊╌ borders instead of solid │─).
        _flat_map_stages  Set[int] of stage indices created by flat_map/product.
                         Used to suppress repeated parent values with "..." and
                         to render hierarchical index paths (e.g. 2.1.0).
        _error_cell      Shared single-element list [dbv | None] across the
                         entire transform chain. When checked=False and a
                         transform raises, the failing value is stored here so
                         the outermost iterator can record it into history
                         before printing the grid.
        _raw             Internal flag: when True, source is already a stream of
                         DebugIteratorValue objects (used by _wrap_transform).
                         When False (public constructor), raw values are wrapped
                         into DebugIteratorValues with initial lineage.

    Key internals:
        _wrap_transform  Creates a child DebugIterator from a generator,
                         appending a new stage name and propagating all internal
                         state (checked, history, tap/flat_map sets, error_cell).
        _each            Shared helper for transforms that process each value
                         independently (map, filter, enumerate, unique, tap).
    """

    __init__ = (self, source, stages=None, checked=False, history=5, _raw=False, _tap_stages=None, _error_cell=None, _flat_map_stages=None) =>
        if _raw:
            self._source = source
            self._stages = stages ?? []
        else:
            let wrap_source = () =>
                for value in source:
                    yield DebugIteratorValue(value, [(value, 0)])
            self._source = wrap_source()
            self._stages = ["source"]
        self._checked = checked
        self._history_limit = history
        self._history = []
        self._consumed_count = 0
        self._tap_stages = _tap_stages ?? set()
        self._error_cell = _error_cell ?? [None]
        self._flat_map_stages = _flat_map_stages ?? set()

    # --- Iteration protocol ---

    __iter__ = self => self

    __next__ = self =>
        while True:
            let dbv = self._next_dbv()
            self._record(dbv)
            if not dbv.is_skipped:
                return dbv.value

    _next_dbv = self =>
        try:
            return next(self._source)
        except StopIteration() as e =>
            raise e
        except BaseException() as e =>
            if not self._checked:
                if self._error_cell[0] !== None:
                    self._record(self._error_cell[0])
                let grid = self.format_grid()
                let err = DebugIteratorError(f"{type(e).__name__}: {e}\n\n{grid}", self, e)
                err.__suppress_context__ = True
                raise err
            raise e

    _record = (self, dbv) =>
        self._consumed_count = self._consumed_count + 1
        self._history.append(dbv)
        if self._history_limit !== None and len(self._history) > self._history_limit:
            self._history.pop(0)

    # --- Internal helpers ---

    _wrap_transform = (self, gen, stage_name, is_tap=False, is_flat_map=False) =>
        let new_stages = [*self._stages, stage_name]
        let new_tap_stages = set(self._tap_stages)
        if is_tap:
            new_tap_stages.add(len(new_stages) - 1)
        let new_fm_stages = set(self._flat_map_stages)
        if is_flat_map:
            new_fm_stages.add(len(new_stages) - 1)
        DebugIterator(gen, new_stages, self._checked, self._history_limit, _raw=True, _tap_stages=new_tap_stages, _error_cell=self._error_cell, _flat_map_stages=new_fm_stages)

    _apply_fn = (self, f, value, stage_name, dbv=None, stage_idx=None) =>
        """Apply f(value), handling errors based on checked mode."""
        try:
            return Ok(f(value))
        except BaseException() as e =>
            if self._checked:
                return Err(e)
            if dbv !== None and stage_idx !== None:
                self._error_cell[0] = dbv.add_lineage_step(e, stage_idx, is_error=True).skip()
            raise e

    # --- Shared transform helpers ---

    _each = (self, name, fn, is_tap=False) =>
        """Transform each non-skipped value. fn(dbv, stage_idx) should yield DebugIteratorValue(s)."""
        let si = len(self._stages)
        let gen = () =>
            for dbv in self._source:
                if dbv.is_skipped:
                    yield dbv
                    continue
                for out in fn(dbv, si):
                    yield out
        self._wrap_transform(gen(), name, is_tap)



    # --- Transformation methods ---

    map = (self, f) =>
        let body = (dbv, si) =>
            let result = self._apply_fn(f, dbv.value, "map", dbv, si)
            if result matches Ok(new_val):
                yield dbv.add_lineage_step(new_val, si).with_value(new_val)
            else:
                yield dbv.add_lineage_step(result.value, si, is_error=True).skip()
        self._each("map", body)

    filter = (self, f) =>
        let body = (dbv, si) =>
            let result = self._apply_fn(f, dbv.value, "filter", dbv, si)
            if result matches Ok(passed):
                if passed:
                    yield dbv.add_lineage_step(dbv.value, si)
                else:
                    yield dbv.add_lineage_step(dbv.value, si).skip()
            else:
                yield dbv.add_lineage_step(result.value, si, is_error=True).skip()
        self._each("filter", body)

    filter_map = (self, f=None) =>
        let body = (dbv, si) =>
            if f === None:
                let r = Result(dbv.value)
                if r matches Ok(val):
                    yield dbv.add_lineage_step(val, si).with_value(val)
                else:
                    yield dbv.add_lineage_step(r.value, si, is_error=True).skip()
            else:
                let result = self._apply_fn(f, dbv.value, "filter_map", dbv, si)
                if result matches Ok(val):
                    let r = Result(val)
                    if r matches Ok(inner):
                        yield dbv.add_lineage_step(inner, si).with_value(inner)
                    else:
                        yield dbv.add_lineage_step(r.value, si, is_error=True).skip()
                else:
                    yield dbv.add_lineage_step(result.value, si, is_error=True).skip()
        self._each("filter_map", body)

    flat_map = (self, f=None) =>
        let stage_idx = len(self._stages)
        let gen = () =>
            let parent_idx = 0
            for dbv in self._source:
                if dbv.is_skipped:
                    yield dbv
                    parent_idx = parent_idx + 1
                    continue
                let child_idx = 0
                let items = None
                if f === None:
                    items = dbv.value
                else:
                    let result = self._apply_fn(f, dbv.value, "flat_map", dbv, stage_idx)
                    if result matches Ok(val):
                        items = val
                    else:
                        let err = result.value
                        yield dbv.add_lineage_step(err, stage_idx, is_error=True).skip()
                        parent_idx = parent_idx + 1
                        continue
                let parent_path = dbv.index_path ?? [parent_idx]
                for item in items:
                    let child_dbv = DebugIteratorValue(
                        item
                        dbv.lineage
                        False
                        [*parent_path, child_idx]
                    )
                    yield child_dbv.add_lineage_step(item, stage_idx)
                    child_idx = child_idx + 1
                parent_idx = parent_idx + 1
        self._wrap_transform(gen(), "flat_map", is_flat_map=True)

    take = (self, n) =>
        let si = len(self._stages)
        let gen = () =>
            let count = 0
            for dbv in self._source:
                if dbv.is_skipped:
                    yield dbv
                    continue
                if count < n:
                    yield dbv.add_lineage_step(dbv.value, si)
                else:
                    yield dbv.add_lineage_step(dbv.value, si).skip()
                    return None
                count = count + 1
        self._wrap_transform(gen(), "take")

    skip = (self, n) =>
        let si = len(self._stages)
        let gen = () =>
            let count = 0
            for dbv in self._source:
                if dbv.is_skipped:
                    yield dbv
                    continue
                if count >= n:
                    yield dbv.add_lineage_step(dbv.value, si)
                else:
                    yield dbv.add_lineage_step(dbv.value, si).skip()
                count = count + 1
        self._wrap_transform(gen(), "skip")

    take_while = (self, f) =>
        let si = len(self._stages)
        let gen = () =>
            let done = False
            for dbv in self._source:
                if dbv.is_skipped:
                    yield dbv
                    continue
                if done:
                    yield dbv.add_lineage_step(dbv.value, si).skip()
                    continue
                let result = self._apply_fn(f, dbv.value, "take_while", dbv, si)
                if result matches Ok(passed):
                    if passed:
                        yield dbv.add_lineage_step(dbv.value, si)
                    else:
                        done = True
                        yield dbv.add_lineage_step(dbv.value, si).skip()
                else:
                    done = True
                    yield dbv.add_lineage_step(result.value, si, is_error=True).skip()
        self._wrap_transform(gen(), "take_while")

    skip_while = (self, f) =>
        let si = len(self._stages)
        let gen = () =>
            let skipping = True
            for dbv in self._source:
                if dbv.is_skipped:
                    yield dbv
                    continue
                if not skipping:
                    yield dbv.add_lineage_step(dbv.value, si)
                    continue
                let result = self._apply_fn(f, dbv.value, "skip_while", dbv, si)
                if result matches Ok(passed):
                    if passed:
                        yield dbv.add_lineage_step(dbv.value, si).skip()
                    else:
                        skipping = False
                        yield dbv.add_lineage_step(dbv.value, si)
                else:
                    skipping = False
                    yield dbv.add_lineage_step(result.value, si, is_error=True).skip()
        self._wrap_transform(gen(), "skip_while")

    chain = (self, *others) =>
        let stage_idx = len(self._stages)
        let gen = () =>
            for dbv in self._source:
                if dbv.is_skipped:
                    yield dbv
                    continue
                yield dbv.add_lineage_step(dbv.value, stage_idx)
            for other in others:
                for value in other:
                    let dbv = DebugIteratorValue(value, [(value, None)])
                    yield dbv.add_lineage_step(value, stage_idx)
        self._wrap_transform(gen(), "chain")

    zip = (self, *others) =>
        let stage_idx = len(self._stages)
        let gen = () =>
            let other_iters = []
            for o in others:
                other_iters.append(o.iter)
            for dbv in self._source:
                if dbv.is_skipped:
                    yield dbv
                    continue
                let other_vals = []
                let exhausted = False
                for oi in other_iters:
                    try:
                        other_vals.append(next(oi))
                    except StopIteration() =>
                        exhausted = True
                        break
                if exhausted:
                    return None
                let zipped = (dbv.value, *other_vals)
                yield dbv.add_lineage_step(zipped, stage_idx).with_value(zipped)
        self._wrap_transform(gen(), "zip")

    enumerate = (self, start=0) =>
        let idx = [start]
        let body = (dbv, si) =>
            let pair = (idx[0], dbv.value)
            idx[0] = idx[0] + 1
            yield dbv.add_lineage_step(pair, si).with_value(pair)
        self._each("enumerate", body)

    unique = (self) =>
        let seen = set()
        let body = (dbv, si) =>
            if not seen.__contains__(dbv.value):
                seen.add(dbv.value)
                yield dbv.add_lineage_step(dbv.value, si)
            else:
                yield dbv.add_lineage_step(dbv.value, si).skip()
        self._each("unique", body)

    reversed = (self) =>
        DebugIterator(Iterable.reversed(self), checked=self._checked, history=self._history_limit).label("reversed")

    sorted = (self, key=None, reverse=False) =>
        DebugIterator(Iterable.sorted(self, key=key, reverse=reverse), checked=self._checked, history=self._history_limit).label("sorted")

    cycle = (self) =>
        let values = []
        for dbv in self._source:
            if not dbv.is_skipped:
                values.append(dbv.value)
        let gen = () =>
            while True:
                for v in values:
                    yield v
        DebugIterator(gen(), checked=self._checked, history=self._history_limit)

    product = (self, *others) =>
        let stage_idx = len(self._stages)
        let gen = () =>
            let self_items = list(self._source)
            let active_items = []
            let parent_idx = 0
            for dbv in self_items:
                if dbv.is_skipped:
                    yield dbv
                else:
                    active_items.append((parent_idx, dbv))
                parent_idx = parent_idx + 1
            let other_lists = []
            for o in others:
                other_lists.append(list(o))
            for item in active_items:
                let pidx = item[0]
                let dbv = item[1]
                let parent_path = dbv.index_path ?? [pidx]
                let child_idx = 0
                for combo in itertools.product([dbv], *other_lists):
                    let other_vals = list(combo[1..])
                    let prod_val = (dbv.value, *other_vals)
                    let child_dbv = DebugIteratorValue(
                        prod_val
                        dbv.lineage
                        False
                        [*parent_path, child_idx]
                    )
                    yield child_dbv.add_lineage_step(prod_val, stage_idx)
                    child_idx = child_idx + 1
        self._wrap_transform(gen(), "product", is_flat_map=True)

    # --- Label & Tap ---

    label = (self, name) =>
        """Rename the most recent stage for readability in the grid."""
        if len(self._stages) == 0:
            return self
        self._stages[len(self._stages) - 1] = name
        self

    tap = (self, f) =>
        let body = (dbv, si) =>
            let result = self._apply_fn(f, dbv.value, "tap", dbv, si)
            if result matches Ok(tap_val):
                yield dbv.add_lineage_step(tap_val, si)
            else:
                # On error in tap, record the error but pass value through (no skip)
                yield dbv.add_lineage_step(result.value, si, is_error=True)
        self._each("tap", body, is_tap=True)

    # --- Terminal methods ---

    show = (self, max_width=20) =>
        """Consume the entire iterator and print the debug grid."""
        # Drain remaining values, catching unchecked errors to display the grid.
        try:
            for dbv in self._source:
                self._record(dbv)
        except StopIteration() =>
            pass
        except BaseException() =>
            if self._error_cell[0] !== None:
                self._record(self._error_cell[0])
        print(self.format_grid(max_width))

    # --- Properties ---

    history = property! self => self._history
    consumed_count = property! self => self._consumed_count
    stages = property! self => list(self._stages)

    # --- Repr ---

    __repr__ = self =>
        if self._consumed_count > len(self._history) and self._history_limit !== None:
            f"DebugIterator(showing last {len(self._history)} of {self._consumed_count} consumed, {len(self._stages)} stages)"
        else:
            f"DebugIterator({self._consumed_count} consumed, {len(self._stages)} stages)"

    # --- Grid formatting ---

    format_grid = (self, max_width=20) =>
        _DebugGrid(
            self._history
            self._stages
            self._tap_stages
            self._flat_map_stages
            self._consumed_count
            self._history_limit
            max_width
        ).render()