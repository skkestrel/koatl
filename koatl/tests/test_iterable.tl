import util.assert_eq

test_sum = () =>
    assert_eq((..10).sum(), 45)
    assert_eq([1, 2, 3, 4, 5].sum(), 15)
    assert_eq([].sum(), 0)
    assert_eq([1.5, 2.5, 3.0].sum(), 7.0)

test_product = () =>
    let prod_iter = [1, 2, 3].product([4, 5])
    assert_eq(list(prod_iter), [(1, 4), (1, 5), (2, 4), (2, 5), (3, 4), (3, 5)])

test_min = () =>
    assert_eq([3, 1, 4, 1, 5].min(), 1)
    assert_eq([10, 5, 8].min(), 5)
    assert_eq([42].min(), 42)

test_max = () =>
    assert_eq([3, 1, 4, 1, 5].max(), 5)
    assert_eq([10, 5, 8].max(), 10)
    assert_eq([42].max(), 42)

test_map = () =>
    # List.map returns a list directly (not an iterator)
    assert_eq([1, 2, 3].map($ * 2), [2, 4, 6])
    assert_eq([1, 2, 3].map($ + 10), [11, 12, 13])
    assert_eq([].map($ * 2), [])
    # Range/iterator map still returns an iterator
    assert_eq((..5).map($ * 2).list(), [0, 2, 4, 6, 8])

test_filter = () =>
    assert_eq([1, 2, 3, 4, 5].filter($ > 3).list(), [4, 5])
    assert_eq([1, 2, 3, 4, 5].filter($ %% 2 == 0).list(), [2, 4])
    assert_eq((..10).filter($ > 5).list(), [6, 7, 8, 9])
    assert_eq([1, 2, 3].filter($ > 10).list(), [])

test_flat_map = () =>
    assert_eq([1, 2, 3].flat_map(x => [x, x * 2]).list(), [1, 2, 2, 4, 3, 6])
    assert_eq([[1, 2], [3, 4]].flat_map(x => x).list(), [1, 2, 3, 4])
    assert_eq([].flat_map(x => [x, x]).list(), [])

test_find = () =>
    # find() returns Ok(value) or Err()
    assert_eq([1, 2, 3, 4, 5].find($ > 3).ok, True)
    let result = [1, 2, 3, 4, 5].find($ > 10)
    assert_eq(result.ok, False)

test_any = () =>
    assert_eq([1, 2, 3].any(x => x > 2), True)
    assert_eq([1, 2, 3].any(x => x > 10), False)
    assert_eq([].any(x => x > 0), False)
    assert_eq([False, False, True].any(x => x), True)

test_all = () =>
    assert_eq([1, 2, 3].all(x => x > 0), True)
    assert_eq([1, 2, 3].all(x => x > 2), False)
    assert_eq([].all(x => x > 0), True)
    assert_eq([True, True, True].all(x => x), True)

test_count = () =>
    # count() works on Iterable trait via .iter
    assert_eq([1, 2, 3, 4, 5].iter.count(x => x > 2), 3)
    assert_eq([1, 2, 3, 4, 5].iter.count(x => x %% 2 == 0), 2)
    assert_eq([1, 2, 3].iter.count(x => x > 10), 0)

test_take = () =>
    assert_eq([1, 2, 3, 4, 5].take(3).list(), [1, 2, 3])
    assert_eq((..100).take(5).list(), [0, 1, 2, 3, 4])
    assert_eq([1, 2].take(10).list(), [1, 2])
    assert_eq([].take(5).list(), [])

test_skip = () =>
    assert_eq([1, 2, 3, 4, 5].skip(2).list(), [3, 4, 5])
    assert_eq((..10).skip(5).list(), [5, 6, 7, 8, 9])
    assert_eq([1, 2].skip(10).list(), [])
    assert_eq([].skip(5).list(), [])

test_take_while = () =>
    assert_eq([1, 2, 3, 4, 5].take_while($ < 4).list(), [1, 2, 3])
    assert_eq([2, 4, 6, 7, 8].take_while($ %% 2 == 0).list(), [2, 4, 6])
    assert_eq([1, 2, 3].take_while($ > 10).list(), [])

test_skip_while = () =>
    # Uses skip_while() method
    assert_eq([1, 2, 3, 4, 5].skip_while($ < 4).list(), [4, 5])
    assert_eq([2, 4, 6, 7, 8].skip_while($ %% 2 == 0).list(), [7, 8])
    assert_eq([1, 2, 3].skip_while($ > 10).list(), [1, 2, 3])

test_enumerate = () =>
    assert_eq((..3).enumerate().list(), [(0, 0), (1, 1), (2, 2)])
    assert_eq(["a", "b", "c"].enumerate().list(), [(0, "a"), (1, "b"), (2, "c")])

test_zip = () =>
    assert_eq([1, 2, 3].zip([4, 5, 6]).list(), [(1, 4), (2, 5), (3, 6)])
    assert_eq([1, 2].zip(["a", "b", "c"]).list(), [(1, "a"), (2, "b")])
    assert_eq([].zip([1, 2, 3]).list(), [])

test_chain = () =>
    assert_eq([1, 2].chain([3, 4]).list(), [1, 2, 3, 4])
    assert_eq([1].chain([]).chain([2, 3]).list(), [1, 2, 3])
    assert_eq([].chain([]).list(), [])

test_cycle = () =>
    assert_eq([1, 2, 3].cycle().take(7).list(), [1, 2, 3, 1, 2, 3, 1])
    assert_eq([5].cycle().take(3).list(), [5, 5, 5])

test_reverse = () =>
    # reverse() is a method on Iterable trait, accessed via .iter
    assert_eq(list([1, 2, 3, 4, 5].iter.reverse()), [5, 4, 3, 2, 1])
    assert_eq(list([1].iter.reverse()), [1])
    assert_eq(list([].iter.reverse()), [])

test_sort = () =>
    assert_eq([3, 1, 4, 1, 5].iter.sort().list(), [1, 1, 3, 4, 5])
    assert_eq(["c", "a", "b"].iter.sort().list(), ["a", "b", "c"])
    assert_eq([].iter.sort().list(), [])

test_join = () =>
    assert_eq(["a", "b", "c"].join(", "), "a, b, c")
    assert_eq([1, 2, 3].map(str).join("-"), "1-2-3")
    assert_eq(["hello"].join(", "), "hello")
    assert_eq([].join(", "), "")

test_associate = () =>
    assert_eq((..3).associate($ * 2), {0: 0, 1: 2, 2: 4})
    assert_eq(["a", "bb", "ccc"].associate(len), {"a": 1, "bb": 2, "ccc": 3})
    assert_eq([].associate($ + 1), {})

test_group_by = () =>
    assert_eq([1, 2, 3, 4, 5].group_by($ %% 2), {0: [2, 4], 1: [1, 3, 5]})
    assert_eq(["a", "bb", "ccc", "dd"].group_by(len), {1: ["a"], 2: ["bb", "dd"], 3: ["ccc"]})
    assert_eq([].group_by($ > 0), {})

test_list_conversion = () =>
    assert_eq((..5).list(), [0, 1, 2, 3, 4])
    assert_eq([1, 2, 3].map($ * 2).list(), [2, 4, 6])

test_record_conversion = () =>
    # Dict.map returns a dict directly (not an iterator)
    assert_eq({1: 2, 3: 4}.map([k, v] => [k * 2, v * 3]), {2: 6, 6: 12})
    assert_eq([[1, "a"], [2, "b"]].record(), {1: "a", 2: "b"})

test_record_take = () =>
    assert_eq({0: 2}.take(1).list(), [(0, 2)])

test_for_each = () =>
    let test_list = []
    [1, 2, 3].for_each(test_list.append)
    assert_eq(test_list, [1, 2, 3])

test_fold = () =>
    assert_eq([1, 2, 3, 4].fold(0, (acc, x) => acc + x), 10)
    assert_eq([1, 2, 3].fold(1, (acc, x) => acc * x), 6)
    assert_eq([5, 3, 8, 1].fold(0, (acc, x) => max(acc, x)), 8)

# Record/dict methods - defined in both prelude/__init__.tl (Extension.method for dict)
# and runtime/record.py (Record class methods)
test_record_map = () =>
    # map transforms both keys and values
    assert_eq({1: 10, 2: 20}.map([k, v] => [k + 1, v + 5]), {2: 15, 3: 25})
    assert_eq({"a": 1, "b": 2}.map([k, v] => [k.upper(), v * 10]), {"A": 10, "B": 20})

test_record_filter = () =>
    # filter based on key-value pairs (defined in prelude/__init__.tl for dict)
    assert_eq({1: 10, 2: 20, 3: 30}.filter([k, v] => v > 15), {2: 20, 3: 30})
    assert_eq({1: 10, 2: 20, 3: 30}.filter([k, v] => k %% 2 == 1), {1: 10, 3: 30})

test_record_filter_keys = () =>
    # filter_keys - defined in prelude/__init__.tl for dict and record.py for Record
    assert_eq({1: 10, 2: 20, 3: 30}.filter_keys($ > 1), {2: 20, 3: 30})
    assert_eq({"a": 1, "b": 2, "c": 3}.filter_keys($ <> "b"), {"a": 1, "c": 3})

test_record_filter_values = () =>
    # filter_values - defined in prelude/__init__.tl for dict and record.py for Record
    assert_eq({1: 10, 2: 20, 3: 30}.filter_values($ >= 20), {2: 20, 3: 30})
    assert_eq({"a": 5, "b": 10, "c": 25}.filter_values($ %% 10 == 5), {"a": 5, "c": 25})