import util.assert_eq
import koatl.std.debug_iter.(DebugIterator, DebugIteratorValue, DebugIteratorError)

# Helper: consume a pipeline and return (values, di).
let consume = di =>
    let vals = list(di)
    (vals, di)

# Helper: extract (value, is_skipped, lineage_values) from a history entry.
# lineage_values is the list of raw values at each lineage step.
let entry_info = dbv =>
    let lv = []
    for step in dbv.lineage:
        lv.append(step[0])
    (dbv.value, dbv.is_skipped, lv)

# =============================================================================
# Empty iterator
# =============================================================================

let d_empty = DebugIterator([], history=None)
list(d_empty)
assert_eq(d_empty.consumed_count, 0)
assert_eq(d_empty.history, [])
assert_eq(d_empty.stages, ["source"])

# =============================================================================
# Basic map + filter: lineage tracks each stage value, skipped items flagged
# =============================================================================

let vals1, di1 = consume(DebugIterator([1, 2, 3, 4, 5], history=None).map($ * 2).filter($ > 4))
assert_eq(vals1, [6, 8, 10])
assert_eq(di1.consumed_count, 5)
assert_eq(di1.stages, ["source", "map", "filter"])

let h1 = di1.history
# source=1 → map→2 → filter rejects (skipped); .value is last-stage output
assert_eq(h1[0].value, 2)
assert_eq(h1[0].is_skipped, True)
assert_eq(h1[0].lineage, [(1, 0), (2, 1), (2, 2)])
# value=3 → map→6 → filter passes
assert_eq(h1[2].value, 6)
assert_eq(h1[2].is_skipped, False)
assert_eq(h1[2].lineage, [(3, 0), (6, 1), (6, 2)])
# value=5 → map→10 → filter passes
assert_eq(h1[4].value, 10)
assert_eq(h1[4].is_skipped, False)
assert_eq(h1[4].lineage, [(5, 0), (10, 1), (10, 2)])

# =============================================================================
# Filter then map: skipped values have no lineage beyond the filter stage
# =============================================================================

let vals2, di2 = consume(DebugIterator([1, 2, 3], history=None).filter($ > 1).map($ * 100))
assert_eq(vals2, [200, 300])
let h2 = di2.history
# value=1 filtered out — skipped, lineage stops at filter
assert_eq(h2[0].value, 1)
assert_eq(h2[0].is_skipped, True)
assert_eq(h2[0].lineage, [(1, 0), (1, 1)])
# value=2 passes filter → map→200
assert_eq(h2[1].value, 200)
assert_eq(h2[1].is_skipped, False)
assert_eq(h2[1].lineage, [(2, 0), (2, 1), (200, 2)])

# =============================================================================
# Label: renames the last stage
# =============================================================================

let vals3, di3 = consume(DebugIterator([1, 2], history=None).map($ + 1).label("inc"))
assert_eq(vals3, [2, 3])
assert_eq(di3.stages, ["source", "inc"])

# =============================================================================
# Chain: chained values have None provenance for prior stages
# =============================================================================

let vals4, di4 = consume(DebugIterator([1, 2], history=None).map($ * 10).chain([7, 8]))
assert_eq(vals4, [10, 20, 7, 8])
assert_eq(di4.stages, ["source", "map", "chain"])
let h4 = di4.history
# Original values have full lineage
assert_eq(h4[0].lineage, [(1, 0), (10, 1), (10, 2)])
assert_eq(h4[1].lineage, [(2, 0), (20, 1), (20, 2)])
# Chained values have None for prior stages
assert_eq(h4[2].lineage, [(7, None), (7, 2)])
assert_eq(h4[3].lineage, [(8, None), (8, 2)])

# =============================================================================
# History truncation: only last N entries retained
# =============================================================================

let vals5, di5 = consume(DebugIterator((..10), history=3).map($ * 2))
assert_eq(len(vals5), 10)
assert_eq(di5.consumed_count, 10)
assert_eq(len(di5.history), 3)
# Last 3 entries are indices 7, 8, 9
assert_eq(di5.history[0].value, 14)
assert_eq(di5.history[1].value, 16)
assert_eq(di5.history[2].value, 18)

# history=None keeps all
let vals5b, di5b = consume(DebugIterator((..20), history=None).map($ + 1))
assert_eq(di5b.consumed_count, 20)
assert_eq(len(di5b.history), 20)

# =============================================================================
# Skip: first n values are skipped
# =============================================================================

let vals6, di6 = consume(DebugIterator([1, 2, 3, 4], history=None).skip(2))
assert_eq(vals6, [3, 4])
assert_eq(di6.stages, ["source", "skip"])
let h6 = di6.history
assert_eq(h6[0].is_skipped, True)
assert_eq(h6[0].value, 1)
assert_eq(h6[1].is_skipped, True)
assert_eq(h6[1].value, 2)
assert_eq(h6[2].is_skipped, False)
assert_eq(h6[2].value, 3)
assert_eq(h6[3].is_skipped, False)
assert_eq(h6[3].value, 4)

# =============================================================================
# Take: passes first n, then rejects (skips) the next
# =============================================================================

let vals7, di7 = consume(DebugIterator([10, 20, 30, 40], history=None).take(2))
assert_eq(vals7, [10, 20])
assert_eq(di7.consumed_count, 3)
let h7 = di7.history
assert_eq(h7[0].is_skipped, False)
assert_eq(h7[0].value, 10)
assert_eq(h7[1].is_skipped, False)
assert_eq(h7[1].value, 20)
assert_eq(h7[2].is_skipped, True)
assert_eq(h7[2].value, 30)

# =============================================================================
# Checked mode: errors become Err() in lineage, entry is skipped
# =============================================================================

let vals8, di8 = consume(DebugIterator([2, 0, 3], checked=True, history=None).map(x => 10 / x))
assert_eq(vals8, [5.0, 3.3333333333333335])
assert_eq(di8.consumed_count, 3)
let h8 = di8.history
# Successful: value=5.0
assert_eq(h8[0].value, 5.0)
assert_eq(h8[0].is_skipped, False)
assert_eq(h8[0].lineage[0], (2, 0))
assert_eq(h8[0].lineage[1], (5.0, 1))
# Error: 10/0 — skipped, lineage has Err
assert_eq(h8[1].is_skipped, True)
assert_eq(h8[1].lineage[0], (0, 0))
assert_eq(h8[1].lineage[1][1], 1)
assert_eq(h8[1].lineage[1][0] matches Err(), True)
# Successful: value=3.333...
assert_eq(h8[2].value, 3.3333333333333335)
assert_eq(h8[2].is_skipped, False)

# =============================================================================
# Tap: does not alter values, tap result appears in lineage
# =============================================================================

let vals9, di9 = consume(DebugIterator([1, 2, 3], history=None).map($ * 10).tap($ > 15).label("big?"))
assert_eq(vals9, [10, 20, 30])
assert_eq(di9.stages, ["source", "map", "big?"])
let h9 = di9.history
assert_eq(h9[0].value, 10)
assert_eq(h9[0].is_skipped, False)
assert_eq(h9[0].lineage, [(1, 0), (10, 1), (False, 2)])
assert_eq(h9[1].lineage, [(2, 0), (20, 1), (True, 2)])
assert_eq(h9[2].lineage, [(3, 0), (30, 1), (True, 2)])

# =============================================================================
# Flat_map: children have index_path, lineage inherits parent
# =============================================================================

let vals10, di10 = consume(DebugIterator([1, 2], history=None).flat_map(x => [x, x * 10]))
assert_eq(vals10, [1, 10, 2, 20])
assert_eq(di10.stages, ["source", "flat_map"])
let h10 = di10.history
# Child 0.0: parent=1, child=1
assert_eq(h10[0].value, 1)
assert_eq(h10[0].index_path, [0, 0])
assert_eq(h10[0].lineage, [(1, 0), (1, 1)])
# Child 0.1: parent=1, child=10
assert_eq(h10[1].value, 10)
assert_eq(h10[1].index_path, [0, 1])
assert_eq(h10[1].lineage, [(1, 0), (10, 1)])
# Child 1.0: parent=2, child=2
assert_eq(h10[2].value, 2)
assert_eq(h10[2].index_path, [1, 0])
assert_eq(h10[3].value, 20)
assert_eq(h10[3].index_path, [1, 1])

# =============================================================================
# Product: like flat_map with tuple children
# =============================================================================

let vals11, di11 = consume(DebugIterator(["a", "b"], history=None).product([1, 2]))
assert_eq(vals11, [("a", 1), ("a", 2), ("b", 1), ("b", 2)])
assert_eq(di11.stages, ["source", "product"])
let h11 = di11.history
assert_eq(h11[0].index_path, [0, 0])
assert_eq(h11[0].value, ("a", 1))
assert_eq(h11[1].index_path, [0, 1])
assert_eq(h11[1].value, ("a", 2))
assert_eq(h11[2].index_path, [1, 0])
assert_eq(h11[3].index_path, [1, 1])

# =============================================================================
# Enumerate: value becomes (index, original)
# =============================================================================

let vals12, di12 = consume(DebugIterator(["a", "b"], history=None).enumerate())
assert_eq(vals12, [(0, "a"), (1, "b")])
assert_eq(di12.stages, ["source", "enumerate"])
assert_eq(di12.history[0].lineage, [("a", 0), ((0, "a"), 1)])
assert_eq(di12.history[1].lineage, [("b", 0), ((1, "b"), 1)])

# =============================================================================
# Unique: duplicates are skipped
# =============================================================================

let vals13, di13 = consume(DebugIterator([1, 2, 2, 3], history=None).unique())
assert_eq(vals13, [1, 2, 3])
let h13 = di13.history
assert_eq(h13[0].is_skipped, False)
assert_eq(h13[0].value, 1)
assert_eq(h13[1].is_skipped, False)
assert_eq(h13[1].value, 2)
assert_eq(h13[2].is_skipped, True)
assert_eq(h13[2].value, 2)
assert_eq(h13[3].is_skipped, False)
assert_eq(h13[3].value, 3)

# =============================================================================
# Sorted / reversed: fresh single-stage iterators
# =============================================================================

let vals14, di14 = consume(DebugIterator([3, 1, 2], history=None).sorted())
assert_eq(vals14, [1, 2, 3])
assert_eq(di14.stages, ["sorted"])
assert_eq(di14.consumed_count, 3)

let vals15, di15 = consume(DebugIterator([3, 1, 2], history=None).reversed())
assert_eq(vals15, [2, 1, 3])
assert_eq(di15.stages, ["reversed"])
assert_eq(di15.consumed_count, 3)

# =============================================================================
# Cycle + take: infinite source truncated by take
# =============================================================================

let vals16, di16 = consume(DebugIterator([1, 2], history=None).cycle().take(5))
assert_eq(vals16, [1, 2, 1, 2, 1])
assert_eq(di16.consumed_count, 6)

# =============================================================================
# Unchecked mode: error re-raised, caught externally
# =============================================================================

let caught = False
let caught_err = None
try:
    list(DebugIterator([1, 0], history=None).map(x => 10 / x))
except DebugIteratorError() as e =>
    caught = True
    caught_err = e
assert_eq(caught, True)
# The error wraps the original exception and includes the grid
assert_eq(caught_err.cause matches ZeroDivisionError(), True)
assert_eq("ZeroDivisionError" in str(caught_err), True)
assert_eq(caught_err.grid matches str(), True)

# =============================================================================
# debug_iter from iterable trait
# =============================================================================

let vals17, di17 = consume([10, 20, 30].debug_iter(history=None).map($ + 1))
assert_eq(vals17, [11, 21, 31])
assert_eq(di17.stages, ["source", "map"])
assert_eq(di17.history[0].lineage, [(10, 0), (11, 1)])

# =============================================================================
# Chained pipeline: map → filter → take
# =============================================================================

let vals18, di18 = consume(DebugIterator([1, 2, 3, 4, 5], history=None).map($ * 2).filter($ > 4).take(2))
assert_eq(vals18, [6, 8])
assert_eq(di18.stages, ["source", "map", "filter", "take"])
assert_eq(di18.consumed_count, 5)
let h18 = di18.history
# First two: filter rejects → skipped
assert_eq(h18[0].is_skipped, True)
assert_eq(h18[1].is_skipped, True)
# Next two: filter passes, take passes
assert_eq(h18[2].is_skipped, False)
assert_eq(h18[2].value, 6)
assert_eq(h18[3].is_skipped, False)
assert_eq(h18[3].value, 8)
# Fifth: filter passes, take rejects; .value is mapped value 10, not source 5
assert_eq(h18[4].is_skipped, True)
assert_eq(h18[4].value, 10)

# =============================================================================
# take_while / skip_while
# =============================================================================

let vals19, di19 = consume(DebugIterator([1, 2, 3, 4, 5], history=None).take_while($ < 4))
assert_eq(vals19, [1, 2, 3])
let h19 = di19.history
assert_eq(h19[0].is_skipped, False)
assert_eq(h19[2].is_skipped, False)
assert_eq(h19[3].is_skipped, True)
assert_eq(h19[3].value, 4)

let vals20, di20 = consume(DebugIterator([1, 2, 3, 4, 5], history=None).skip_while($ < 3))
assert_eq(vals20, [3, 4, 5])
let h20 = di20.history
assert_eq(h20[0].is_skipped, True)
assert_eq(h20[0].value, 1)
assert_eq(h20[1].is_skipped, True)
assert_eq(h20[1].value, 2)
assert_eq(h20[2].is_skipped, False)
assert_eq(h20[2].value, 3)

# =============================================================================
# filter_map
# =============================================================================

let vals21, di21 = consume(DebugIterator([1, None, 3], history=None).filter_map())
assert_eq(vals21, [1, 3])
assert_eq(di21.history[1].is_skipped, True)

let vals22, di22 = consume(DebugIterator([1, 2, 3], history=None).filter_map(x => x > 1 then x * 2 else None))
assert_eq(vals22, [4, 6])
assert_eq(di22.history[0].is_skipped, True)
assert_eq(di22.history[1].value, 4)
assert_eq(di22.history[2].value, 6)

# =============================================================================
# Zip
# =============================================================================

let vals23, di23 = consume(DebugIterator([1, 2, 3], history=None).zip([10, 20, 30]))
assert_eq(vals23, [(1, 10), (2, 20), (3, 30)])
assert_eq(di23.stages, ["source", "zip"])
assert_eq(di23.history[0].lineage, [(1, 0), ((1, 10), 1)])

# Zip stops at shortest
let vals24, di24 = consume(DebugIterator([1, 2], history=None).zip([10, 20, 30]))
assert_eq(vals24, [(1, 10), (2, 20)])
assert_eq(di24.consumed_count, 2)
