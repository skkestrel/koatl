import util.assert_eq
import koatl.std.debug_iter.(DebugIterator, DebugIteratorValue, DebugIteratorError)

# Helper: consume a pipeline and return (values, di).
let consume = di =>
    let vals = list(di)
    (vals, di)

# =============================================================================
# Real Err values flowing through the pipeline are NOT misidentified as errors
# =============================================================================

let vals_err, di_err = consume(DebugIterator([Ok(1), Err("bad"), Ok(3)], history=None).map(x => x))
assert_eq(vals_err, [Ok(1), Err("bad"), Ok(3)])
let h_err = di_err.history
# Err("bad") should pass through as a normal value, not an error cell
assert_eq(h_err[1].is_skipped, False)
assert_eq(h_err[1].value, Err("bad"))
# Lineage entries should be plain 2-tuples (no is_error flag)
assert_eq(len(h_err[1].lineage[0]), 2)
assert_eq(len(h_err[1].lineage[1]), 2)
assert_eq(h_err[1].lineage[0][0], Err("bad"))
assert_eq(h_err[1].lineage[1][0], Err("bad"))

# Filter on Err values — Err should be treated as a normal value
let vals_ef, di_ef = consume(
    DebugIterator([Ok(1), Err("x"), Ok(2)], history=None)
        .filter(x => x matches Ok())
)
assert_eq(vals_ef, [Ok(1), Ok(2)])
assert_eq(di_ef.history[1].is_skipped, True)
assert_eq(di_ef.history[1].value, Err("x"))
# Still a 2-tuple lineage — not marked as error
assert_eq(len(di_ef.history[1].lineage[1]), 2)

# =============================================================================
# Checked errors produce 3-tuple lineage entries with is_error=True
# =============================================================================

let vals_ce, di_ce = consume(
    DebugIterator(["hello", 42, "world"], checked=True, history=None)
        .map(x => x.upper())
)
assert_eq(vals_ce, ["HELLO", "WORLD"])
assert_eq(di_ce.consumed_count, 3)
let h_ce = di_ce.history
# "hello" → "HELLO" (normal)
assert_eq(h_ce[0].value, "HELLO")
assert_eq(h_ce[0].is_skipped, False)
assert_eq(len(h_ce[0].lineage[1]), 2)
# 42 → error (int has no .upper())
assert_eq(h_ce[1].is_skipped, True)
assert_eq(len(h_ce[1].lineage[1]), 3)
assert_eq(h_ce[1].lineage[1][2], True)
assert_eq(h_ce[1].lineage[1][0] matches AttributeError(), True)
# "world" → "WORLD" (normal, pipeline continues after error)
assert_eq(h_ce[2].value, "WORLD")
assert_eq(h_ce[2].is_skipped, False)

# =============================================================================
# Multiple checked errors in a row
# =============================================================================

let vals_me, di_me = consume(
    DebugIterator([0, 0, 0, 1], checked=True, history=None)
        .map(x => 1 / x)
)
assert_eq(vals_me, [1.0])
assert_eq(di_me.consumed_count, 4)
# First 3 are errors, all skipped
for i in ..3:
    assert_eq(di_me.history[i].is_skipped, True)
    assert_eq(len(di_me.history[i].lineage[1]), 3)
# Last one succeeds
assert_eq(di_me.history[3].is_skipped, False)
assert_eq(di_me.history[3].value, 1.0)

# =============================================================================
# Checked errors in filter — predicate error skips the value
# =============================================================================

let vals_cf, di_cf = consume(
    DebugIterator([1, "two", 3], checked=True, history=None)
        .filter(x => x > 1)
)
assert_eq(vals_cf, [3])
assert_eq(di_cf.consumed_count, 3)
# 1 filtered out by predicate (normal skip, 2-tuple)
assert_eq(di_cf.history[0].is_skipped, True)
assert_eq(len(di_cf.history[0].lineage[1]), 2)
# "two" causes error in predicate (error skip, 3-tuple)
assert_eq(di_cf.history[1].is_skipped, True)
assert_eq(len(di_cf.history[1].lineage[1]), 3)
assert_eq(di_cf.history[1].lineage[1][2], True)
# 3 passes the predicate
assert_eq(di_cf.history[2].is_skipped, False)
assert_eq(di_cf.history[2].value, 3)

# =============================================================================
# Checked errors in filter_map with function
# =============================================================================

let vals_cfm, di_cfm = consume(
    DebugIterator([1, 0, 3], checked=True, history=None)
        .filter_map(x => x > 1 then 100 / x else None)
)
assert_eq(len(vals_cfm), 1)
# x=1 → None → skipped (not error)
assert_eq(di_cfm.history[0].is_skipped, True)
# x=0 → ZeroDivisionError → error skip
assert_eq(di_cfm.history[1].is_skipped, True)
assert_eq(len(di_cfm.history[1].lineage[1]), 3)
assert_eq(di_cfm.history[1].lineage[1][2], True)

# =============================================================================
# Checked errors in take_while — error stops iteration
# =============================================================================

let vals_tw, di_tw = consume(
    DebugIterator([1, 2, "x", 4], checked=True, history=None)
        .take_while(x => x < 5)
)
assert_eq(vals_tw, [1, 2])
# "x" < 5 raises TypeError → is_error=True, marks done
assert_eq(di_tw.history[2].is_skipped, True)
assert_eq(len(di_tw.history[2].lineage[1]), 3)
assert_eq(di_tw.history[2].lineage[1][2], True)
# 4 is skipped because done=True after the error
assert_eq(di_tw.history[3].is_skipped, True)
assert_eq(len(di_tw.history[3].lineage[1]), 2)

# =============================================================================
# Checked errors in skip_while — error stops skipping
# =============================================================================

let vals_sw, di_sw = consume(
    DebugIterator([1, "x", 3], checked=True, history=None)
        .skip_while(x => x < 5)
)
assert_eq(vals_sw, [3])
# 1 < 5 → True → skipped (normal)
assert_eq(di_sw.history[0].is_skipped, True)
assert_eq(len(di_sw.history[0].lineage[1]), 2)
# "x" < 5 → error → stops skipping, marked error
assert_eq(di_sw.history[1].is_skipped, True)
assert_eq(len(di_sw.history[1].lineage[1]), 3)
assert_eq(di_sw.history[1].lineage[1][2], True)
# 3 passes through (skipping=False after error)
assert_eq(di_sw.history[2].is_skipped, False)
assert_eq(di_sw.history[2].value, 3)

# =============================================================================
# Checked errors in tap — error recorded but value passes through
# =============================================================================

let vals_ct, di_ct = consume(
    DebugIterator([1, "two", 3], checked=True, history=None)
        .tap(x => x + 1)
)
assert_eq(vals_ct, [1, "two", 3])
assert_eq(di_ct.consumed_count, 3)
# 1 → tap produces 2 (normal)
assert_eq(di_ct.history[0].is_skipped, False)
assert_eq(len(di_ct.history[0].lineage[1]), 2)
# "two" → tap error, but value passes through (NOT skipped)
assert_eq(di_ct.history[1].is_skipped, False)
assert_eq(di_ct.history[1].value, "two")
assert_eq(len(di_ct.history[1].lineage[1]), 3)
assert_eq(di_ct.history[1].lineage[1][2], True)
# 3 → tap produces 4 (normal)
assert_eq(di_ct.history[2].is_skipped, False)

# =============================================================================
# Checked errors in flat_map — error skips parent, children not produced
# =============================================================================

let vals_fm, di_fm = consume(
    DebugIterator([2, "bad", 3], checked=True, history=None)
        .flat_map(x => list(range(x)))
)
assert_eq(vals_fm, [0, 1, 0, 1, 2])
# range("bad") raises TypeError → parent entry is error-skipped
let bad_entries = []
for h in di_fm.history:
    if h.is_skipped:
        bad_entries.append(h)
assert_eq(len(bad_entries), 1)
assert_eq(bad_entries[0].lineage[1][2], True)

# =============================================================================
# Deep pipeline: map → filter → map → take — lineage grows correctly
# =============================================================================

let vals_dp, di_dp = consume(
    DebugIterator((..10), history=None)
        .map($ * 2).label("double")
        .filter($ > 5).label("big")
        .map($ + 100).label("shift")
        .take(3)
)
assert_eq(vals_dp, [106, 108, 110])
assert_eq(di_dp.stages, ["source", "double", "big", "shift", "take"])
# A passing value has 5-stage lineage (one per stage)
let pass_entry = None
for h in di_dp.history:
    if h.value == 106:
        pass_entry = h
        break
assert_eq(pass_entry !== None, True)
assert_eq(len(pass_entry.lineage), 5)
assert_eq(pass_entry.lineage[0], (3, 0))
assert_eq(pass_entry.lineage[1], (6, 1))
assert_eq(pass_entry.lineage[2], (6, 2))
assert_eq(pass_entry.lineage[3], (106, 3))
assert_eq(pass_entry.lineage[4], (106, 4))

# Skipped (filtered) values have shorter lineage — stops at filter stage
let skip_entry = di_dp.history[0]
assert_eq(skip_entry.is_skipped, True)
assert_eq(skip_entry.value, 0)
assert_eq(len(skip_entry.lineage), 3)
assert_eq(skip_entry.lineage[0], (0, 0))
assert_eq(skip_entry.lineage[1], (0, 1))
assert_eq(skip_entry.lineage[2], (0, 2))

# =============================================================================
# flat_map → map: children get new lineage step from downstream map
# =============================================================================

let vals_fmm, di_fmm = consume(
    DebugIterator(["ab", "cd"], history=None)
        .flat_map(list)
        .map(x => x.upper())
)
assert_eq(vals_fmm, ["A", "B", "C", "D"])
assert_eq(di_fmm.stages, ["source", "flat_map", "map"])
let h_fmm = di_fmm.history
# "ab" → flat_map produces "a" at [0,0], "b" at [0,1]
assert_eq(h_fmm[0].index_path, [0, 0])
assert_eq(h_fmm[0].value, "A")
assert_eq(h_fmm[0].lineage[2], ("A", 2))
assert_eq(h_fmm[1].index_path, [0, 1])
assert_eq(h_fmm[1].value, "B")

# =============================================================================
# flat_map → filter: some children filtered out
# =============================================================================

let vals_fmf, di_fmf = consume(
    DebugIterator([[1, 2, 3], [4, 5, 6]], history=None)
        .flat_map()
        .filter(x => x > 3)
)
assert_eq(vals_fmf, [4, 5, 6])
# Values <= 3 are skipped
let skipped_vals = []
let passed_vals = []
for h in di_fmf.history:
    if h.is_skipped:
        skipped_vals.append(h.value)
    else:
        passed_vals.append(h.value)
assert_eq(passed_vals, [4, 5, 6])
assert_eq(skipped_vals, [1, 2, 3])

# =============================================================================
# Nested flat_map: double expansion with 3-level index_path
# =============================================================================

let vals_nfm, di_nfm = consume(
    DebugIterator([[1, 2], [3]], history=None)
        .flat_map()
        .flat_map(x => [x, x * 10])
)
assert_eq(vals_nfm, [1, 10, 2, 20, 3, 30])
assert_eq(di_nfm.stages, ["source", "flat_map", "flat_map"])
# Children of nested flat_map have 3-level index_path
assert_eq(di_nfm.history[0].index_path, [0, 0, 0])
assert_eq(di_nfm.history[1].index_path, [0, 0, 1])
assert_eq(di_nfm.history[2].index_path, [0, 1, 0])
assert_eq(di_nfm.history[3].index_path, [0, 1, 1])
assert_eq(di_nfm.history[4].index_path, [1, 0, 0])
assert_eq(di_nfm.history[5].index_path, [1, 0, 1])

# =============================================================================
# enumerate after filter: indices are dense (no gaps from filtered values)
# =============================================================================

let vals_ef2, di_ef2 = consume(
    DebugIterator([1, 2, 3, 4, 5], history=None)
        .filter($ > 2)
        .enumerate()
)
assert_eq(vals_ef2, [(0, 3), (1, 4), (2, 5)])
# Filtered values are skipped and don't get enumerated
assert_eq(di_ef2.history[0].is_skipped, True)
assert_eq(di_ef2.history[1].is_skipped, True)
assert_eq(di_ef2.history[2].is_skipped, False)
assert_eq(di_ef2.history[2].value, (0, 3))

# =============================================================================
# enumerate with start offset
# =============================================================================

let vals_eo, di_eo = consume(DebugIterator(["a", "b", "c"], history=None).enumerate(start=10))
assert_eq(vals_eo, [(10, "a"), (11, "b"), (12, "c")])
assert_eq(di_eo.history[0].lineage, [("a", 0), ((10, "a"), 1)])
assert_eq(di_eo.history[2].lineage, [("c", 0), ((12, "c"), 1)])

# =============================================================================
# unique after map: dedup on transformed values
# =============================================================================

let vals_um, di_um = consume(
    DebugIterator([1, -1, 2, -2, 3], history=None)
        .map(x => x * x)
        .unique()
)
assert_eq(vals_um, [1, 4, 9])
assert_eq(di_um.consumed_count, 5)
# -1 → 1 → unique already seen → skipped
assert_eq(di_um.history[1].is_skipped, True)
assert_eq(di_um.history[1].value, 1)
# -2 → 4 → unique already seen → skipped
assert_eq(di_um.history[3].is_skipped, True)
assert_eq(di_um.history[3].value, 4)

# =============================================================================
# Multiple taps: value passes through, tap results in lineage
# =============================================================================

let log1 = []
let log2 = []
let vals_mt, di_mt = consume(
    DebugIterator([1, 2, 3], history=None)
        .tap(x => log1.append(x)).label("log1")
        .map($ * 10)
        .tap(x => log2.append(x)).label("log2")
)
assert_eq(vals_mt, [10, 20, 30])
assert_eq(log1, [1, 2, 3])
assert_eq(log2, [10, 20, 30])
assert_eq(di_mt.stages, ["source", "log1", "map", "log2"])

# =============================================================================
# Unchecked error preserves grid context via DebugIteratorError
# =============================================================================

let err_caught = None
try:
    list(
        DebugIterator([1, 2, 3, 0, 5], history=None)
            .map($ * 2).label("double")
            .map(x => 100 / x).label("invert")
    )
except DebugIteratorError() as e =>
    err_caught = e

assert_eq(err_caught !== None, True)
assert_eq(err_caught.cause matches ZeroDivisionError(), True)
# The iterator was exposed so we can inspect post-error state
assert_eq(err_caught.iterator.consumed_count, 4)
assert_eq(err_caught.iterator.stages, ["source", "double", "invert"])
# First 3 values (1→2→50, 2→4→25, 3→6→16.66) succeeded
assert_eq(err_caught.iterator.history[0].value, 50.0)
assert_eq(err_caught.iterator.history[1].value, 25.0)
# 4th value (0→0→error) is recorded in history
assert_eq(err_caught.iterator.history[3].is_skipped, True)
assert_eq(len(err_caught.iterator.history[3].lineage[2]), 3)
assert_eq(err_caught.iterator.history[3].lineage[2][2], True)

# =============================================================================
# History limit with errors — error entry counts toward history
# =============================================================================

let err_hist = None
try:
    list(
        DebugIterator((..20), history=3)
            .map(x => x == 15 then 1 / 0 else x)
    )
except DebugIteratorError() as e =>
    err_hist = e

assert_eq(err_hist !== None, True)
# History should have last 3 entries (indices 13, 14, 15-error)
assert_eq(len(err_hist.iterator.history), 3)
assert_eq(err_hist.iterator.consumed_count, 16)
assert_eq(err_hist.iterator.history[2].is_skipped, True)

# =============================================================================
# show() on empty iterator produces output without crashing
# =============================================================================

# show() prints to stdout; we just verify it doesn't crash
DebugIterator([], history=None).show()

# =============================================================================
# show() after partial consumption
# =============================================================================

let di_show = DebugIterator([1, 2, 3, 4, 5], history=3).map($ * 2)
# Consume 2 values manually
next(di_show)
next(di_show)
assert_eq(di_show.consumed_count, 2)
# show() drains the rest and prints the grid
di_show.show()
assert_eq(di_show.consumed_count, 5)
assert_eq(len(di_show.history), 3)

# =============================================================================
# format_grid returns a string containing stage names
# =============================================================================

let di_grid = DebugIterator([1, 2], history=None).map($ + 1).label("inc").filter($ > 1).label("big")
list(di_grid)
let grid_str = di_grid.format_grid()
assert_eq("source" in grid_str, True)
assert_eq("inc" in grid_str, True)
assert_eq("big" in grid_str, True)
assert_eq("DebugIterator" in grid_str, True)

# =============================================================================
# repr shows summary info
# =============================================================================

let di_repr = DebugIterator([1, 2, 3], history=None).map($ * 2)
list(di_repr)
let r = repr(di_repr)
assert_eq("3 consumed" in r, True)
assert_eq("2 stages" in r, True)

# Truncated repr
let di_repr2 = DebugIterator((..100), history=5).map($ * 2)
list(di_repr2)
let r2 = repr(di_repr2)
assert_eq("showing last 5 of 100 consumed" in r2, True)

# =============================================================================
# Chain with empty iterables
# =============================================================================

let vals_ce2, di_ce2 = consume(DebugIterator([1, 2], history=None).chain([], [3]))
assert_eq(vals_ce2, [1, 2, 3])
assert_eq(di_ce2.stages, ["source", "chain"])
assert_eq(di_ce2.consumed_count, 3)

# =============================================================================
# Zip with multiple iterables
# =============================================================================

let vals_zm, di_zm = consume(DebugIterator([1, 2], history=None).zip([10, 20], [100, 200]))
assert_eq(vals_zm, [(1, 10, 100), (2, 20, 200)])
assert_eq(di_zm.history[0].lineage, [(1, 0), ((1, 10, 100), 1)])

# =============================================================================
# skip(0) and take(0): edge cases
# =============================================================================

let vals_s0, di_s0 = consume(DebugIterator([1, 2, 3], history=None).skip(0))
assert_eq(vals_s0, [1, 2, 3])

let vals_t0, di_t0 = consume(DebugIterator([1, 2, 3], history=None).take(0))
assert_eq(vals_t0, [])
assert_eq(di_t0.consumed_count, 1)
assert_eq(di_t0.history[0].is_skipped, True)

# =============================================================================
# skip + take: pagination pattern
# =============================================================================

let vals_pg, di_pg = consume(DebugIterator((..20), history=None).skip(5).take(3))
assert_eq(vals_pg, [5, 6, 7])
assert_eq(di_pg.stages, ["source", "skip", "take"])
# 5 skipped + 3 taken + 1 rejected by take = 9
assert_eq(di_pg.consumed_count, 9)

# =============================================================================
# sorted with key and reverse
# =============================================================================

let vals_sk, di_sk = consume(DebugIterator(["banana", "apple", "cherry"], history=None).sorted(key=len))
assert_eq(vals_sk, ["apple", "banana", "cherry"])

let vals_sr, di_sr = consume(DebugIterator([3, 1, 2], history=None).sorted(reverse=True))
assert_eq(vals_sr, [3, 2, 1])

# =============================================================================
# DebugIteratorValue.with_value / with_lineage / skip builder methods
# =============================================================================

let dbv = DebugIteratorValue(42, [(42, 0)], False, None)
let dbv2 = dbv.with_value(99)
assert_eq(dbv2.value, 99)
assert_eq(dbv2.lineage, [(42, 0)])
assert_eq(dbv2.is_skipped, False)

let dbv3 = dbv.skip()
assert_eq(dbv3.value, 42)
assert_eq(dbv3.is_skipped, True)
assert_eq(dbv3.lineage, [(42, 0)])

let dbv4 = dbv.add_lineage_step(100, 1)
assert_eq(dbv4.lineage, [(42, 0), (100, 1)])
assert_eq(dbv4.value, 42)

let dbv5 = dbv.add_lineage_step("err", 1, is_error=True)
assert_eq(dbv5.lineage, [(42, 0), ("err", 1, True)])
assert_eq(len(dbv5.lineage[1]), 3)

let dbv6 = dbv.with_lineage([(1, 0), (2, 1)])
assert_eq(dbv6.lineage, [(1, 0), (2, 1)])
assert_eq(dbv6.value, 42)

# =============================================================================
# Identity map preserves values exactly
# =============================================================================

let complex_vals = [None, True, False, 0, "", [], {}, (1, 2)]
let vals_id, di_id = consume(DebugIterator(complex_vals, history=None).map(x => x))
assert_eq(vals_id, complex_vals)
assert_eq(di_id.consumed_count, len(complex_vals))
for h in di_id.history:
    assert_eq(h.is_skipped, False)
