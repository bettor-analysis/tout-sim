# Tout Betting Package Simulation

## Overview

This Shiny application reveals the **true cost of buying picks from touts** by simulating both individual betting packages and large-scale results. It compares a tout’s reported outcomes against a bettor’s real-world experience, factoring in **line slippage**, **missed bets**, and **package costs**.

The tool is designed to educate bettors on how even winning touts can lead subscribers to **long-term losses** due to inefficient pricing and logistical constraints.

## Features

- **Simulated Pick Package Results**: See bet-by-bet outcomes for both the tout and the bettor.
- **Line Slippage Simulation**: Adjust bettor odds to reflect worse prices received after tout release.
- **Missed Bets Mechanic**: Simulate real-world scenarios where the bettor misses placing some picks.
- **Package Cost Accounting**: Deducts the pick package cost from the bettor and credits it to the tout.
- **Graphical Comparison**: Bankroll progression graph comparing tout vs bettor per package.
- **Aggregate Simulation Engine**: Run 1,000 simulations to evaluate long-term ROI trends of buying pick packages.
- **Summary Card**: View bankroll outcomes, ROI, missed bet stats, and additional funds required.
- **Clean Reactable UI**: Interactive, scrollable table of picks and outcomes.

## Application Usage

### Inputs

- **Starting Bankroll**: Initial amount the bettor has before purchasing the package.
- **Pick Package Cost**: Cost of the pick package paid to the tout.
- **Number of Picks**: Total number of picks in the package.
- **Tout Unit Size**: Size of each pick in dollars.
- **Line Slippage (%)**: % worse odds received by the bettor vs the tout.
- **Chance Bettor Misses a Pick (%)**: Simulates inability to place all picks.
- **Tout's Claimed Win Rate (%)**: Sets the win rate the tout claims (default 55%).
- **Number of Simulations**: Number of packages to simulate in the background.

### Outputs

- **Pick Table**: Displays each pick’s odds, win/loss, missed status, and bankroll progression.
- **Bankroll Plot**: Line graph of bankroll changes for both tout and bettor.
- **Summary Card**: Final bankrolls, ROI comparison, total missed bets, extra funds needed.
- **Simulation Distribution**: Density plot of profit across all simulated packages.
- **Aggregate Summary**: Shows mean and median bankrolls and ROI over all simulations.

## Technical Information

### Dependencies

- `shiny`
- `ggplot2`
- `reactable`
- `bslib`
- `dplyr`

### Simulation Logic

- **Odds Conversion**:
  - Positive odds: `1 + (odds / 100)`
  - Negative odds: `1 + (100 / abs(odds))`
- **Profit Calculation**:
  - Win: `(decimal_odds - 1) * unit_size`
  - Loss: `-unit_size`
- **Missed Picks**: Simulated via `rbinom()`
- **Line Slippage**: Odds are worsened by a user-selected percentage.

## Installation

1. Clone the repository:
   ```bash
   git clone https://github.com/bettor-analysis/tout-simulation-tool.git
   ```

2. Install dependencies in R:
   ```r
   install.packages(c("shiny", "ggplot2", "reactable", "bslib", "dplyr"))
   ```

3. Run the Shiny app:
   ```r
   library(shiny)
   runApp("path_to_app_directory")
   ```

## Disclaimer

This tool is for **educational purposes only**. It does not endorse or promote gambling or the use of tout services. Use responsibly and stay within your financial means.

## License

This project is licensed under the MIT License.
