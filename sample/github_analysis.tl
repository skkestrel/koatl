# GitHub Repository Analysis Tool
# A focused demo showing Koatl's power for API data processing and analysis

import requests
import json
import re
import datetime.(datetime, timedelta)

create_repo = data =>
    {
        name: data["name"]
        full_name: data["full_name"]
        description: data["description"] ?? "No description"
        language: data["language"] ?? "Unknown"
        stars: data["stargazers_count"]
        forks: data["forks_count"]
        issues: data["open_issues_count"]
        created_at: data["created_at"]
        updated_at: data["updated_at"]
        size_kb: data["size"]

        # Computed properties
        age_days: Record.method& self =>
            let created = datetime.fromisoformat(self.created_at.replace("Z", "+00:00"))
            (datetime.now(created.tzinfo) - created).days

        activity_score: Record.method& self =>
            # Simple activity heuristic
            let days_old = self.age_days() + 1
            let recent_factor = days_old <= 30 then 1. else 365. / days_old
            (self.stars + self.forks * 2 + (100 - self.issues) * 0.1) * recent_factor
    }

fetch_repos = query =>
    async memo @Async.from_sync(() =>
        print(f"🔍 Searching for: {query}")
        let url = f"https://api.github.com/search/repositories?q={query}&sort=stars&per_page=30"

        try requests.get(url).json()
    )

classify_language = lang =>
    lang.lower() match:
        "python" | "javascript" | "typescript" | "java" | "c#" | "go" | "rust" => "Backend/System"
        "html" | "css" | "vue" | "react" | "angular" => "Frontend"
        "jupyter notebook" | "r" | "matlab" => "Data Science"
        "shell" | "dockerfile" | "yaml" | "makefile" => "DevOps"
        "c" | "c++" | "assembly" => "Low Level"
        "" | "unknown" => "Unknown"
        _ => "Other"

analyze_repos = query =>
    let result = @fetch_repos(query)

    result.map(response =>
        response["items"]
            .map(create_repo)
            .filter(repo => repo.stars > 10)  # Filter noise
            .map(repo => {
                **repo
                category: classify_language(repo.language)
                activity: repo.activity_score()
            })
            .sort(repo => repo.activity, reverse=True)
    )

compute_stats = repos =>
    let by_language = repos.group_by($.language)
    let by_category = repos.group_by($.category)

    {
        total_repos: repos.len
        total_stars: repos.map($.stars).sum()
        avg_stars: repos.map($.stars).mean()

        top_languages: by_language
            .map([lang, repos] => {
                language: lang
                count: repos.len
                total_stars: repos.map($.stars).sum()
                avg_activity: repos.map($.activity).mean()
            })
            .sort($.total_stars, reverse=True)
            .take(5),

        categories: by_category
            .map([cat, repos] => {
                category: cat
                count: repos.len
                languages: repos.map($.language).unique().take(3)
            })
            .sort($.count, reverse=True),

        most_active: repos.take(3).map(repo => {
            name: repo.name
            language: repo.language
            stars: repo.stars
            activity_score: repo.activity
        })
    }

display_results = (query, result) =>
    print(f"\n📊 Analysis Results for '{query}'")
    print("=" * 50)

    result match:
        Ok(repos) if repos.len > 0 =>
            let stats = compute_stats(repos)

            print(f"Found {stats.total_repos} repositories")
            print(f"Total stars: {stats.total_stars}")
            print(f"Average stars: {stats.avg_stars}")

            print(f"\n🔥 Top Languages:")
            stats.top_languages.for_each(lang =>
                print(f"  {lang.language}: {lang.count} repos, {lang.total_stars} stars")
            )

            print(f"\n📂 Categories:")
            stats.categories.for_each(cat =>
                let langs = cat.languages.join_str(", ")
                print(f"  {cat.category}: {cat.count} repos ({langs})")
            )

            print(f"\n⭐ Most Active:")
            stats.most_active.for_each(repo =>
                print(f"  {repo.name} ({repo.language}): {repo.stars} stars, score: {repo.activity_score}")
            )

        Ok([]) =>
            print("❌ No repositories found matching the criteria")

        Err(error) =>
            print(f"💥 Error: {error}")

process_queries = queries =>
    queries
        .map(query =>
            display_results(query, @analyze_repos(query))
            print("\n" + "-" * 50)
        )
        .traverse()

main = () =>
    print("🚀 GitHub Repository Analyzer")
    print("Powered by Koatl functional programming\n")

    let search_queries = [
        "machine learning python"
        "web framework javascript"
        "blockchain rust"
        "cli tools go"
    ]

    process_queries(search_queries).run()

    print("\n✨ Analysis complete!")


if __name__ == "__main__":
    main()
