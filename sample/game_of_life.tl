# Game of Life Simulation with Visualization
# Demonstrates advanced pattern matching, ranges, iterators, and functional composition

import random
import time

# Cell states using pattern matching and enums
create_cell = (x, y, alive=False) => {
    x: x, y: y, alive: alive

    # Extension method for neighbor calculation
    neighbors: Record.method! (self, grid_size) =>
        [
            (self.x - 1, self.y - 1), (self.x, self.y - 1), (self.x + 1, self.y - 1)
            (self.x - 1, self.y),                           (self.x + 1, self.y)
            (self.x - 1, self.y + 1), (self.x, self.y + 1), (self.x + 1, self.y + 1)
        ]
            .filter([x, y] => x >= 0 and y >= 0 and x < grid_size and y < grid_size)

    next_state: Record.method! (self, alive_neighbors) =>
        # Conway's Game of Life rules using pattern matching
        (self.alive, alive_neighbors) match:
            (True, 2) | (True, 3) => True      # Survival
            (False, 3) => True                 # Birth
            _ => False                         # Death or stay dead
}

# Grid operations using functional composition
create_grid = size =>
    (..size).flat_map(y =>
        (..size).map(x => ((x, y), create_cell(x, y, random.random() < 0.3)))
    ).dict()

# Display grid using Unicode and pattern matching
display_grid = (grid, size) =>
    print("\n" + "─" * (size * 2 + 1))

    (..size).for_each(y =>
        let row = (..size)
            .map(x => grid[(x, y)].alive then "██" else "  ")
            .join_str("")
        print(f"│{row}│")
    )

    print("─" * (size * 2 + 1))

# Count living neighbors using iterator methods
count_living_neighbors = (grid, cell, size) =>
    cell.neighbors(size)
        .map(pos => grid[pos].alive)
        .count($ == True)

# Evolution step using monadic operations and pattern matching
evolve_grid = (grid, size) =>
    grid.items()
        .map([pos, cell] =>
            let living_neighbors = count_living_neighbors(grid, cell, size)
            let new_state = cell.next_state(living_neighbors)
            (pos, create_cell(cell.x, cell.y, new_state))
        )
        .dict()

# Pattern recognition for common configurations
analyze_patterns = (grid, size) =>
    let living_cells = grid.values().filter($.alive).list()
    let total_alive = living_cells.len

    # Check for still lifes (blocks, beehives, etc.)
    let still_life_candidates = living_cells
        .group_by(cell => (cell.x // 4, cell.y // 4))  # Group into 4x4 regions
        .filter([_, cells] => cells.len >= 4 and cells.len <= 6)

    # Check for oscillators by looking at neighbor patterns
    let potential_oscillators = living_cells
        .filter(cell =>
            let neighbors = count_living_neighbors(grid, cell, size)
            neighbors == 2 or neighbors == 3
        )

    # Detect gliders (moving patterns)
    let glider_patterns = [
        [(0,0), (1,0), (2,0), (2,1), (1,2)]  # Classic glider shape
        [(0,1), (1,2), (2,0), (2,1), (2,2)]  # Rotated glider
    ]

    let detected_gliders = glider_patterns
        .flat_map(pattern =>
            (..size-2).flat_map(base_x =>
                (..size-2).map(base_y =>
                    pattern.map([dx, dy] => (base_x + dx, base_y + dy))
                        .all(pos => grid[pos]?.alive ?? False) then:
                            Ok((base_x, base_y))
                        else:
                            Err()
                )
            )
        )
        .filter_map()

    {
        total_alive: total_alive
        density: total_alive / (size * size) * 100
        still_life_regions: still_life_candidates.count()
        oscillator_candidates: potential_oscillators.count()
        detected_gliders: detected_gliders.count()

        # Stability analysis
        stability: total_alive match:
            0 => "extinct"
            x if x < size => "dying"
            x if x > size * size * 0.8 => "overpopulated"
            _ => "stable"
    }

# Statistics tracking using fold operations
track_statistics = () => {
    generations: []

    add_generation: Record.method! (self, analysis) =>
        self.generations.append(analysis)

        # Keep only last 10 generations for analysis
        if self.generations.len > 10:
            self.generations = self.generations[-10..]

    get_trends: Record.method! self =>
        self.generations.len >= 3 then:
            let recent = self.generations[-3..]
            let densities = recent.map($.density).list()

            {
                trend: densities[-1] > densities[0] then "growing"
                    else densities[-1] < densities[0] then "shrinking"
                    else "stable"
                avg_density: densities.mean()
                stability_changes: recent.map($.stability).unique().count()
            }
        else:
            {trend: "insufficient_data", avg_density: 0, stability_changes: 0}
}

# Main simulation loop with comprehensive pattern matching
run_simulation = (size=20, max_generations=100, delay=0.5) =>
    print(f"🎮 Conway's Game of Life - {size}x{size} Grid")
    print("Starting simulation with random initial state...\n")

    let grid = create_grid(size)
    let stats = track_statistics()
    let generation = 0

    # Evolution loop using recursive function
    let simulate_generation = (current_grid, gen) =>
        gen >= max_generations then:
            print(f"🏁 Simulation completed after {max_generations} generations")
        else:
            print(f"Generation {gen}")
            display_grid(current_grid, size)

            let analysis = analyze_patterns(current_grid, size)
            stats.add_generation(analysis)

            # Print current analysis
            print(f"Living cells: {analysis.total_alive} ({analysis.density}%)")
            print(f"Status: {analysis.stability}")

            analysis.detected_gliders > 0 then:
                print(f"🚀 Detected {analysis.detected_gliders} glider(s)!")

            # Check for extinction or stability
            analysis.stability match:
                "extinct" =>
                    print("💀 All cells have died. Simulation ended.")

                "overpopulated" =>
                    print("⚠️  Overpopulation detected. Evolution continues...")
                    let next_grid = evolve_grid(current_grid, size)
                    time.sleep(delay)
                    simulate_generation(next_grid, gen + 1)

                _ =>
                    # Show trends if available
                    let trends = stats.get_trends()
                    trends.trend <> "insufficient_data" then:
                        print(f"📈 Trend: {trends.trend} (avg density: {trends.avg_density}%)")

                    # Continue evolution
                    let next_grid = evolve_grid(current_grid, size)
                    time.sleep(delay)
                    simulate_generation(next_grid, gen + 1)

    simulate_generation(grid, generation)

# Interactive pattern editor
create_pattern_editor = size => {
    grid: create_grid(size).map([pos, cell] => (pos, create_cell(cell.x, cell.y, False))).dict()

    toggle_cell: Record.method! (self, x, y) =>
        (x >= 0 and y >= 0 and x < size and y < size) then:
            let pos = (x, y)
            let cell = self.grid[pos]
            self.grid = {**self.grid, (pos): create_cell(x, y, not cell.alive)}
            True
        else:
            False

    load_pattern: Record.method! (self, pattern, offset_x=0, offset_y=0) =>
        pattern.for_each([x, y] =>
            self.toggle_cell(x + offset_x, y + offset_y)
        )

    get_grid: Record.method! self => self.grid
}

# Predefined interesting patterns
PATTERNS = {
    glider: [(1,0), (2,1), (0,2), (1,2), (2,2)]
    block: [(0,0), (0,1), (1,0), (1,1)]
    blinker: [(1,0), (1,1), (1,2)]
    toad: [(1,0), (2,0), (3,0), (0,1), (1,1), (2,1)]
    beacon: [(0,0), (1,0), (0,1), (3,2), (2,3), (3,3)]
}

# Demo with predefined patterns
run_pattern_demo = () =>
    print("🎭 Running Pattern Demonstrations\n")

    PATTERNS.items().for_each([name, pattern] =>
        print(f"Demonstrating: {name.upper()}")

        let editor = create_pattern_editor(10)
        editor.load_pattern(pattern, 3, 3)
        let grid = editor.get_grid()

        display_grid(grid, 10)

        # Show evolution for a few steps
        (..5).fold(grid, (current_grid, step) =>
            time.sleep(0.8)
            let next_grid = evolve_grid(current_grid, 10)
            print(f"Step {step + 1}:")
            display_grid(next_grid, 10)
            next_grid
        )

        print(f"Pattern '{name}' demo complete.\n")
    )

# Main execution with user choice
if __name__ == "__main__":
    print("Conway's Game of Life in Koatl")
    print("1. Run random simulation")
    print("2. Run pattern demos")

    let choice = input("Choose option (1 or 2): ").strip()

    choice match:
        "1" => run_simulation(size=15, max_generations=50, delay=0.3)
        "2" => run_pattern_demo()
        _ => print("Invalid choice. Running random simulation...")
            | run_simulation()
