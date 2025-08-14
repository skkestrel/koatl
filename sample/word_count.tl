import requests
import re

word_count = url =>
    let result = async memo Async.from_sync(() =>
        print("Fetching URL:", url)
        try requests.get(url)
    )

    result.map($
        .text
        .split()
        .map(re.sub("[^a-zA-Z]", "", $))
        .count_by($.lower())
        .sorted([_, value] => value, reverse=True)
        .take(10)
        .list()
    )

books = {
    r_and_j: "https://www.gutenberg.org/cache/epub/1513/pg1513.txt"
    moby_dick: "https://www.gutenberg.org/cache/epub/2701/pg2701.txt"
    error: "https://www.fdiofadijafod.com/"
}

let rec = books.map([key, value] => (key, @word_count(value))).traverse().run()
    | Record

rec.for_each(print)