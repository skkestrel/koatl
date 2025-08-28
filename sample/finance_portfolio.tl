# Financial Portfolio Analyzer
# Demonstrates Koatl's pattern matching, monads, extensions, and data processing

import datetime.(datetime, timedelta)
import random
import math

# Define a Stock record with computed properties
create_stock = (symbol, price, shares, purchase_date) => {
    symbol: symbol
    price: price
    shares: shares
    purchase_date: purchase_date
    purchase_price: price

    # Extension methods using Record.method
    current_value: Record.method& self => self.price * self.shares

    days_held: Record.method& (self, date) =>
        (date - self.purchase_date).days

    annualized_return: Record.method& (self, date) =>
        let days = self.days_held(date)
        days > 0 then (self.price / self.purchase_price - 1) * (365.0 / days) else 0.0
}

# Portfolio management with pattern matching and error handling
update_portfolio = (portfolio, action, date) =>
    @Ok(action match:
        {type: "buy", symbol, shares, price} =>
            let existing = check portfolio[symbol]
            existing match:
                Ok(stock) => {
                    **portfolio
                    (symbol): {
                        **stock
                        shares: stock.shares + shares
                        # Weighted average price
                        price: (stock.price * stock.shares + price * shares) / (stock.shares + shares)
                    }
                }
                Err() => {
                    **portfolio
                    (symbol): create_stock(symbol, price, shares, date)
                }

        {type: "sell", symbol, shares} =>
            let stock = @(check portfolio[symbol]).map_err(_ => KeyError(f"Stock {symbol} not found"))

            stock.shares >= shares then {
                **portfolio
                (symbol): {**stock, shares: stock.shares - shares}
            } else:
                @Err(ValueError(f"Insufficient shares: {stock.shares} < {shares}"))

        {type: "dividend", symbol, amount} =>
            portfolio[symbol]?.(stock => {
                **portfolio
                (symbol): {**stock, dividends: (check stock.dividends ?? 0) + amount}
            }) ?? portfolio

        _ => @Err(ValueError(f"Unknown action: {action}"))
    )

# Market simulation using Async monad for delayed operations
simulate_market_day = portfolio =>
    async memo @Async.from_sync(() =>
        # Simulate random price movements
        portfolio.items()
            .map([symbol, stock] =>
                let volatility = 0.02  # 2% daily volatility
                let change = random.gauss(0.001, volatility)  # Slight upward bias
                let new_price = stock.price * (1 + change)

                (symbol, {**stock, price: new_price})
            )
            .record()
    )

# Portfolio analysis with comprehensive pattern matching
analyze_portfolio = (portfolio, date) =>
    let total_value = portfolio.values().map($.current_value()).sum()
    let stocks = portfolio.values().list()

    # Risk analysis using pattern matching on volatility
    let risk_assessment = stocks
        .map(stock => {
            **stock
            risk_level: stock.annualized_return(date).(abs) match:
                x if x < 0.1 => "Low"
                x if x < 0.3 => "Medium"
                _ => "High"
        })
        .group_by($.risk_level)

    # Sector diversification (simplified)
    let sector_allocation = stocks
        .map(stock => {
            sector: stock.symbol.startswith("TECH") then "Technology"
                else stock.symbol.startswith("FIN") then "Finance"
                else stock.symbol.startswith("HEALTH") then "Healthcare"
                else "Other"
            value: stock.current_value()
        })
        .group_by($.sector)
        .map([sector, holdings] => {
            sector: sector
            total_value: holdings.map($.value).sum()
            percentage: holdings.map($.value).sum() / total_value * 100
        })

    # Performance metrics
    let performance = stocks.map(stock => {
        symbol: stock.symbol
        return_rate: stock.annualized_return(date)
        days_held: stock.days_held(date)
        profit_loss: stock.current_value() - (stock.purchase_price * stock.shares)
    }).list()

    {
        total_value: total_value
        stock_count: stocks.len
        best_performer: performance.max($.return_rate)
        worst_performer: performance.min($.return_rate)
        risk_breakdown: risk_assessment.map([level, stocks] => {
            risk_level: level
            count: stocks.len
            total_value: stocks.map($.current_value()).sum()
        })
        sector_allocation: sector_allocation
        avg_return: performance.map($.return_rate).mean()
        total_profit_loss: performance.map($.profit_loss).sum()
    }

run_portfolio_simulation = () =>
    print("🏦 Starting Portfolio Simulation")

    let date = datetime.now()

    # Initial portfolio setup
    let initial_actions = [
        {type: "buy", symbol: "TECH001", shares: 100, price: 150.0}
        {type: "buy", symbol: "FIN002", shares: 50, price: 80.0}
        {type: "buy", symbol: "HEALTH003", shares: 75, price: 120.0}
        {type: "buy", symbol: "TECH004", shares: 200, price: 45.0}
    ]

    # Build portfolio with error handling
    let portfolio = initial_actions
        .fold({}, (portfolio, action) =>
            update_portfolio(portfolio, action, date) match:
                Ok(new_portfolio) => new_portfolio
                Err(error) => (
                    print(f"❌ Error: {error}")
                    portfolio
                )
        )

    print(f"📊 Initial portfolio created with {portfolio.len} stocks")

    # Simulate 30 days of trading
    let final_portfolio = (..30).fold(portfolio, (current_portfolio, day) =>
        print(f"📅 Day {day + 1}")
        date = date + timedelta(days=1)

        # Simulate market changes
        let updated_portfolio = simulate_market_day(current_portfolio).run()

        # Random trading actions
        let action = random.choice([
            {type: "buy", symbol: "TECH001", shares: 10, price: updated_portfolio["TECH001"].price * 1.01}
            {type: "sell", symbol: "FIN002", shares: 5}
            {type: "dividend", symbol: "HEALTH003", amount: 50.0}
            None  # No action
        ])

        action match:
            None => updated_portfolio
            action => update_portfolio(updated_portfolio, action, date) match:
                Ok(new_portfolio) =>
                    print(f"✅ Executed: {action.type} {action.symbol ?? ""}")
                    new_portfolio
                Err(error) =>
                    print(f"❌ Failed: {error}")
                    updated_portfolio
    )

    # Final analysis
    let analysis = analyze_portfolio(final_portfolio, date)

    print("\n📈 Final Portfolio Analysis:")
    print(f"Total Value: ${analysis.total_value}")
    print(f"Total P&L: ${analysis.total_profit_loss}")
    print(f"Average Return: {analysis.avg_return * 100}%")

    let bp = analysis.best_performer
    print(f"Best Performer: {bp.symbol} ({bp.return_rate * 100}%)")
    let wp = analysis.worst_performer
    print(f"Worst Performer: {wp.symbol} ({wp.return_rate * 100}%)")

    print("\n🎯 Sector Allocation:")
    analysis.sector_allocation.for_each(sector =>
        print(f"  {sector.sector}: {sector.percentage}% (${sector.total_value})")
    )

    print("\n⚠️  Risk Breakdown:")
    analysis.risk_breakdown.for_each(risk =>
        print(f"  {risk.risk_level} Risk: {risk.count} stocks, ${risk.total_value}")
    )

# Run the simulation
if __name__ == "__main__":
    run_portfolio_simulation()
