# Web Scraper with Content Analysis
# Demonstrates async operations, error handling, text processing, and data pipelines

import math
import random
import sys
import requests
import json
import re
import bs4.BeautifulSoup
import urllib.parse.(urljoin, urlparse)
import time
import hashlib

# URL and content validation using pattern matching
validate_url = url =>
    match url:
        url_str if url_str.startswith(("http://", "https://")) =>
            check urlparse(url_str).netloc != "" ?? False
        _ => False

# Content extraction with pattern matching on HTML elements
extract_content = (soup, content_type="text") =>
    match content_type:
        "text" =>
            # Extract clean text content
            for script_or_style in soup(["script", "style"]):
                script_or_style.decompose()

            soup.get_text().split().join(" ").strip()

        "links" =>
            # Extract all links with their text
            soup.find_all("a", href=True).map(link => {
                url: link["href"]
                text: link.get_text().strip()
                title: link.get("title", "")
            }).filter(link => link.text != "")

        "images" =>
            # Extract image information
            soup.find_all("img").map(img => {
                src: img.get("src", "")
                alt: img.get("alt", "")
                title: img.get("title", "")
            }).filter(img => img.src != "")

        "headings" =>
            # Extract heading hierarchy
            ["h1", "h2", "h3", "h4", "h5", "h6"].iter.flat_map(tag =>
                soup.find_all(tag).map(heading => {
                    level: int(tag[1])
                    text: heading.get_text().strip()
                    id: heading.get("id", "")
                })
            ).filter(heading => heading.text != "")

        "metadata" =>
            # Extract page metadata
            {
                title: soup.find("title")?.get_text?()?.strip?() ?? ""
                description: soup.find("meta", attrs={"name": "description"})?.get?("content", "") ?? ""
                keywords: soup.find("meta", attrs={"name": "keywords"})?.get?("content", "") ?? ""
                author: soup.find("meta", attrs={"name": "author"})?.get?("content", "") ?? ""
                canonical: soup.find("link", attrs={"rel": "canonical"})?.get?("href", "") ?? ""
            }

# Web page record with content analysis
create_page = (url, html_content) => {
    url: url
    html: html_content
    fetched_at: time.time()
    content_hash: hashlib.md5(html_content.encode()).hexdigest()

    # Lazy parsing - only parse when needed
    _soup: None

    soup: Record.method! self =>
        self._soup ?? (
            self._soup = BeautifulSoup(self.html, "html.parser")
            self._soup
        )

    # Content extraction methods
    get_text: Record.method! self => extract_content(self.soup(), "text")
    get_links: Record.method! self => extract_content(self.soup(), "links")
    get_images: Record.method! self => extract_content(self.soup(), "images")
    get_headings: Record.method! self => extract_content(self.soup(), "headings")
    get_metadata: Record.method! self => extract_content(self.soup(), "metadata")

    # Text analysis
    word_count: Record.method! self => self.get_text().split().len

    reading_time: Record.method! self =>
        # Estimate reading time (average 200 words per minute)
        let words = self.word_count()
        math.ceil(words / 200)

    extract_keywords: Record.method! (self, top_n=10) =>
        let text = self.get_text().lower()
        let words = re.findall("\\b[a-z]{3,}\\b", text)

        # Simple keyword extraction (remove common stop words)
        let stop_words = [
            "the", "and", "for", "are", "but", "not", "you", "all", "can", "had", "her", "was", "one",
            "our", "out", "day", "get", "has", "him", "his", "how", "its", "may", "new", "now", "old",
            "see", "two", "way", "who", "boy", "did", "she", "use", "her", "oil", "sit", "set", "run"
        ]

        words.filter(word => word not in stop_words)
            .iter.count_by()  # Count frequency
            .items().iter
            .sorted($[1], reverse=True)  # Sort by frequency
            .take(top_n)
            .map($[0])  # Get just the words

    # Link analysis
    internal_links: Record.method! self =>
        let base_domain = urlparse(self.url).netloc
        self.get_links().filter(link =>
            let link_domain = urlparse(urljoin(self.url, link.url)).netloc
            link_domain == base_domain or link_domain == ""
        )

    external_links: Record.method! self =>
        let base_domain = urlparse(self.url).netloc
        self.get_links().filter(link =>
            let link_domain = urlparse(urljoin(self.url, link.url)).netloc
            link_domain != base_domain and link_domain != ""
        )
}

# Async web scraper with rate limiting and error handling
create_scraper = (delay=1.0, max_retries=3, timeout=10) => {
    delay: delay
    max_retries: max_retries
    timeout: timeout
    session: requests.Session()
    cache: {}

    # User agent rotation for better success rate
    user_agents: [
        "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36"
        "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36"
        "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36"
    ]

    fetch_page: Record.method! (self, url) =>
        Async.from_sync(() =>
            if validate_url(url) then None else:
                return Err(ValueError(f"Invalid URL: {url}"))

            # Check cache first
            let cache_key = url
            match check self.cache[cache_key]:
                Ok(cached_page) =>
                    print(f"📄 Using cached: {url}")
                    return Ok(cached_page)

                Err() =>
                    print(f"🌐 Fetching: {url}")

                    # Retry logic with exponential backoff
                    let fetch_with_retry = attempt =>
                        if attempt > self.max_retries:
                            return Err(f"Max retries exceeded for {url}")
                        else:
                            try:
                                let headers = {
                                    "User-Agent": random.choice(self.user_agents)
                                    "Accept": "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8"
                                    "Accept-Language": "en-US,en;q=0.5"
                                }

                                let response = self.session.get(url, headers=headers, timeout=self.timeout)

                                return match response.status_code:
                                    200 =>
                                        let page = create_page(url, response.text)
                                        self.cache = {**self.cache, (cache_key): page}
                                        Ok(page)

                                    404 => Err(f"Page not found: {url}")
                                    403 => Err(f"Access forbidden: {url}")
                                    _ => Err(f"HTTP {response.status_code}: {url}")

                            except requests.RequestException(e) =>
                                print(f"⚠️  Retry {attempt}/{self.max_retries} for {url}: {e}")
                                time.sleep(self.delay * (2 ** (attempt - 1)))  # Exponential backoff
                                return fetch_with_retry(attempt + 1)
                            except Exception(e) =>
                                return Err(f"Unexpected error fetching {url}: {e}")
        )

    # Crawl multiple pages with depth control
    crawl_site: Record.method! (self, start_url, max_depth=2, max_pages=10) =>
        let visited = set([start_url])
        let results = {}
        let queue = [(start_url, 0)]  # (url, depth)

        let crawl_recursive = () =>
            if queue.len == 0 or results.len >= max_pages:
                results.values().iter.list()
            else:
                let (current_url, depth) = queue.pop(0)

                match self.fetch_page(current_url).run():
                    Ok(page) =>
                        results = {**results, (current_url): page}
                        print(f"✅ Crawled: {current_url} (depth {depth})")

                        # Add internal links to queue if within depth limit
                        if depth < max_depth:
                            let new_links = page.internal_links()
                                .iter.map(link => urljoin(current_url, link.url))
                                .filter(url => url not in visited and validate_url(url))
                                .take(5)  # Limit links per page

                            new_links.for_each(url =>
                                visited.add(url)
                                queue.append((url, depth + 1))
                            )

                        time.sleep(self.delay)  # Rate limiting
                        crawl_recursive()

                    Err(error) =>
                        print(f"❌ Failed: {current_url} - {error}")
                        crawl_recursive()

        crawl_recursive()
}

# Content analysis pipeline
analyze_content = pages =>
    if pages.len == 0:
        {error: "No pages to analyze"}
    else:
        let total_words = pages.map($.word_count()).iter.sum()
        let all_keywords = pages.iter.flat_map(page => page.extract_keywords(5))
        let keyword_freq = all_keywords.count_by()

        let link_analysis = pages.iter.flat_map(page =>
            page.get_links().map(link => {
                link: link,
                page_url: page.url
            })
        ).group_by(item =>
            let domain = urlparse(urljoin(item.page_url, item.link.url)).netloc
            if domain != "" then domain else "internal"
        )

        {
            page_count: pages.len
            total_words: total_words
            avg_words_per_page: if pages.len > 0 then total_words / pages.len else 0
            total_reading_time: pages.map($.reading_time()).iter.sum()

            # Top keywords across all pages
            top_keywords: keyword_freq.items().iter
                .sorted($[1], reverse=True)[..10]
                .iter.map([keyword, count] => {keyword: keyword, frequency: count})

            # Page analysis
            pages_by_size: pages.iter.sorted($.word_count(), reverse=True).map(page => {
                url: page.url
                title: page.get_metadata().title
                word_count: page.word_count()
                reading_time: page.reading_time()
                internal_links: page.internal_links().len
                external_links: page.external_links().len
            })

            # Link analysis
            link_domains: link_analysis.iter.map([domain, links] => {
                domain: domain
                link_count: links.len
                unique_texts: links.map($.link.text).iter.unique().tally()
            }).iter.sorted($.link_count, reverse=True)

            # Content patterns
            common_headings: pages.iter.flat_map($.get_headings())
                .map($.text.lower())
                .count_by()
                .items().iter
                .filter($[1] > 1)  # Appear on multiple pages
                .sorted($[1], reverse=True)[..5]
        }

# Report generation with pattern matching
generate_report = (analysis, format="text") =>
    match format:
        "text" =>
            f"""
📊 Website Content Analysis Report
{"=" * 50}

📈 Overview:
  • Pages analyzed: {analysis.page_count}
  • Total words: {analysis.total_words}
  • Average words per page: {analysis.avg_words_per_page:.1f}
  • Total reading time: {analysis.total_reading_time} minutes

🔑 Top Keywords:
{analysis.top_keywords.map(kw => f"  • {kw.keyword} ({kw.frequency} times)").join("\n")}

📄 Pages by Size:
{analysis.pages_by_size.iter.take(5).map(page =>
    f"  • {page.title or page.url} - {page.word_count} words ({page.reading_time}min read)"
).join("\n")}

🔗 Link Analysis:
{analysis.link_domains.iter.take(5).map(domain =>
    f"  • {domain.domain}: {domain.link_count} links"
).join("\n")}

📋 Common Headings:
{analysis.common_headings.map([heading, count] =>
    f"  • \"{heading}\" (appears {count} times)"
).join("\n")}
"""

        "json" =>
            json.dumps(analysis, indent=2)

        "html" =>
            f"""
<!DOCTYPE html>
<html>
<head>
    <title>Content Analysis Report</title>
    <style>
        body {{ font-family: Arial, sans-serif; margin: 40px; }}
        .metric {{ background: #f5f5f5; padding: 10px; margin: 10px 0; }}
        .keyword {{ display: inline-block; background: #007acc; color: white;
                   padding: 5px 10px; margin: 5px; border-radius: 15px; }}
    </style>
</head>
<body>
    <h1>📊 Website Content Analysis</h1>

    <div class="metric">
        <h2>Overview</h2>
        <p>Pages: {analysis.page_count} | Words: {analysis.total_words} |
           Reading Time: {analysis.total_reading_time} min</p>
    </div>

    <div class="metric">
        <h2>Top Keywords</h2>
        {analysis.top_keywords.map(kw => f"<span class=\"keyword\">{kw.keyword} ({kw.frequency})</span>").join("")}
    </div>

    <div class="metric">
        <h2>Page Analysis</h2>
        <ul>
        {analysis.pages_by_size.map(page => f"<li>{page.title or page.url} - {page.word_count} words</li>").join("")}
        </ul>
    </div>
</body>
</html>
"""

# Demo scenarios
run_single_page_demo = () =>
    print("🔍 Single Page Analysis Demo")
    let scraper = create_scraper(delay=0.5)

    let demo_urls = [
        "https://httpbin.org/html"
        "https://example.com"
        "https://httpbin.org/robots.txt"
    ]

    demo_urls.iter.for_each(url =>
        print(f"\n🌐 Analyzing: {url}")

        match scraper.fetch_page(url).run():
            Ok(page) =>
                let metadata = page.get_metadata()
                print(f"📄 Title: {metadata.title}")
                print(f"📝 Words: {page.word_count()}")
                print(f"⏱️  Reading time: {page.reading_time()} minutes")
                print(f"🔗 Links: {page.get_links().len}")
                print(f"🖼️  Images: {page.get_images().len}")

                let keywords = page.extract_keywords(5)
                print(f"🔑 Keywords: {keywords.join(", ")}")

            Err(error) =>
                print(f"❌ Error: {error}")

            other =>
                print(f"Unknown error occurred: {other}")
    )

run_crawling_demo = () =>
    print("\n🕷️  Website Crawling Demo")
    let scraper = create_scraper(delay=1.0)

    let target_url = "https://httpbin.org/"
    print(f"🎯 Crawling: {target_url}")

    let pages = scraper.crawl_site(target_url, max_depth=2, max_pages=5)

    if pages.len > 0:
        print(f"\n✅ Successfully crawled {pages.len} pages")

        let analysis = analyze_content(pages)
        let report = generate_report(analysis, "text")
        print(report)

        # Save detailed report
        try:
            with f = open("crawl_report.json", "w"):
                json.dump(analysis, f, indent=2)
            print("💾 Detailed report saved to crawl_report.json")
        except Exception(e) =>
            print(f"⚠️  Could not save report: {e}")
    else:
        print("❌ No pages successfully crawled")

# Main demo selector
if __name__ == "__main__":
    print("🕸️  Web Scraper ! Content Analyzer")
    print("Choose demo:")
    print("1. Single Page Analysis")
    print("2. Website Crawling")
    print("3. Both")

    let choice = (if len(sys.argv) > 1 then sys.argv[1] else "1").strip()

    match choice:
        "1" => run_single_page_demo()
        "2" => run_crawling_demo()
        "3" => (
            run_single_page_demo()
            run_crawling_demo()
        )
        _ =>
            print("Invalid choice, running single page demo...")
            run_single_page_demo()
