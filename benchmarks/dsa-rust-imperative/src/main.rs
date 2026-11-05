//  Copyright (c) 2026 Andrey Dubovik <andrei at dubovik dot eu>

// Standard library
use std::fmt;
use std::fs::File;
use std::hint::black_box;
use std::io::BufReader;
use std::ops::{Index, IndexMut};
use std::time::Instant;

// Crates
use serde::Deserialize;


// Labelled dataset columns
const DEBT: usize                =  0;
const DEBT_GDP: usize            =  1;
const DEBT_MATURING: usize       =  2;
const GDP: usize                 =  3;
const PRIMARY_SURPLUS: usize     =  4;
const PRIMARY_SURPLUS_GDP: usize =  5;
const RATE: usize                =  6;
const RHO: usize                 =  7;
const SHARE_MATURING: usize      =  8;
const SURPLUS: usize             =  9;
const NO_COLUMNS: usize          = 10;

// Simulation scenario
enum Scenario {
    Low,
    Medium,
    High,
}

// A rudimentary dataframe with an offset row index
struct Table {
    lb: i64,
    no_cols: usize,
    data: Vec<f64>,
}

impl Table {
    // Compute an offset row index
    fn ntv_index(lb: i64, i: i64) -> usize {
        (i - lb).try_into().expect("i >= lb")
    }

    // Initialize a new table of a given size
    fn new(lb: i64, ub: i64, no_cols: usize) -> Table {
        let no_rows = Table::ntv_index(lb, ub);
        Table {
            lb: lb,
            no_cols: no_cols,
            data: vec![f64::NAN; no_rows*no_cols],
        }
    }

    // Clone a table
    fn clone(&self) -> Self {
        Table {
            lb: self.lb,
            no_cols: self.no_cols,
            data: self.data.clone(),
        }
    }
}

// Indexing operations
impl Index<(usize, i64)> for Table {
    type Output = f64;

    fn index(&self, (j, i): (usize, i64)) -> &Self::Output {
        let i = Table::ntv_index(self.lb, i);
        &self.data[i*self.no_cols + j]
    }
}

impl IndexMut<(usize, i64)> for Table {
    fn index_mut(&mut self, (j, i): (usize, i64)) -> &mut Self::Output {
        let i = Table::ntv_index(self.lb, i);
        &mut self.data[i*self.no_cols + j]
    }
}

// Data loading

// Raw JSON value
#[derive(Deserialize)]
struct RawValue {
    year: i64,
    value: f64,
}

// Raw JSON series
#[derive(Deserialize)]
struct RawSeries {
    values: Vec<RawValue>,
}

// Load any given historical series
fn load_series(path: &str) -> RawSeries {
    println!("reading {path}");
    let file = File::open(path).expect("JSON file exists");
    let reader = BufReader::new(file);
    serde_json::from_reader(reader).expect("JSON file contains valid series")
}

// Statistics functions

// Compute a sample quantile
fn quantile(mut x: Vec<f64>, k: usize, q: usize) -> f64 {
    x.sort_unstable_by(|a, b| a.partial_cmp(b).unwrap());
    let n = x.len() - 1;
    let i0 = n*k/q;
    let i1 = (i0 + 1).min(n);
    let w1 = (n*k%q) as f64 / q as f64;
    let w0 = 1.0 - w1;
    w0*x[i0] + w1*x[i1]
}

impl Table {
    // Compute quantiles over a given column
    fn quantile(&self, start: i64, end: i64, j: usize, k: usize, q: usize) -> f64 {
        let x = (start..end).map(|i| self[(j, i)]).collect();
        quantile(x, k, q)
    }
}

// A dataset with all historical data
struct DataSet {
    table: Table,
}

impl DataSet {
    // Initialize a dataset and load all historical series
    fn prepare() -> DataSet {
        // Preload historical series
        println!("# Data Loading");
        let series = vec![
            (DEBT,            load_series("data/debt.json")),
            (DEBT_MATURING,   load_series("data/debt-maturing.json")),
            (GDP,             load_series("data/gdp.json")),
            (PRIMARY_SURPLUS, load_series("data/primary-surplus.json")),
            (RATE,            load_series("data/rate-newdebt.json")),
            (RHO,             load_series("data/rate-implicit.json")),
        ];

        // Determine index boundaries
        let mut tmin = i64::MAX;
        let mut tmax = i64::MIN;
        for s in &series {
            for &RawValue {year, ..} in &s.1.values {
                tmin = tmin.min(year);
                tmax = tmax.max(year);
            }
        }

        // Construct and populate a dataframe
        let mut ds = DataSet {
            table: Table::new(tmin, tmax + 1, NO_COLUMNS),
        };

        for s in &series {
            for &RawValue {year, value} in &s.1.values {
                ds.table[(s.0, year)] = value;
            }
        }
        ds
    }

    // Clone a dataset
    fn clone(&self) -> Self {
        DataSet {
            table: self.table.clone(),
        }
    }

    // Run complete simulation
    fn run_simulation(&self, start: i64, end: i64, s: Scenario) -> DataSet {
        let mut ds = self.clone();
        let x = &mut ds.table;

        // Compute historical series
        for t in x.lb..(start + 1) {
            // Maturing debt
            x[(SHARE_MATURING, t)] = x[(DEBT_MATURING, t)]/x[(DEBT, t)];

            // Primary budget surplus (share of GDP)
            x[(PRIMARY_SURPLUS_GDP, t)] = x[(PRIMARY_SURPLUS, t)]/x[(GDP, t)];

            // Budget surplus
            x[(SURPLUS, t)] = x[(PRIMARY_SURPLUS, t)] - x[(DEBT, t)]*x[(RHO, t)];
        }

        // Compute model estimates based on historical series
        let window = 30;
        let k = match s {
            Scenario::Low => 1,
            Scenario::Medium => 5,
            Scenario::High => 9,
        };
        let primary_surplus_gdp = x.quantile(start - window, start, PRIMARY_SURPLUS_GDP, k, 10);

        // Compute recursive forecast series
        for t in start..(end + 1) {
            // Debt forecasting starts one period later due to debt being start-of-period
            if t > start {  
                // Outstanding debt
                x[(DEBT, t)] = x[(DEBT, t-1)] - x[(SURPLUS, t-1)];

                // Maturing debt
                x[(SHARE_MATURING, t)] = x[(SHARE_MATURING, t-1)];
            }

            // GDP
            let nominal_growth = 1.03;
            x[(GDP, t)] = x[(GDP, t-1)]*nominal_growth;

            // Primary budget surplus (share of GDP)
            x[(PRIMARY_SURPLUS_GDP, t)] = primary_surplus_gdp;

            // Primary budget surplus (nominal)
            x[(PRIMARY_SURPLUS, t)] = x[(PRIMARY_SURPLUS_GDP, t)]*x[(GDP, t)];

            // Implicit interest rate
            let debt_total = x[(DEBT, t)];
            let debt_deferred = (1.0 - x[(SHARE_MATURING, t-1)])*x[(DEBT, t-1)];
            let interest_new = (debt_total - debt_deferred)*x[(RATE, t)];
            let interest_deferred = debt_deferred*x[(RHO, t-1)];
            x[(RHO, t)] = (interest_deferred + interest_new)/debt_total;

            // Budget surplus
            x[(SURPLUS, t)] = x[(PRIMARY_SURPLUS, t)] - x[(DEBT, t)]*x[(RHO, t)];
        }

        // Compute auxiliary series
        for t in x.lb..end {
            // End-of-year debt to GDP (aligns with official reporting)
            x[(DEBT_GDP, t)] = x[(DEBT, t+1)]/x[(GDP, t)];
        }
        ds
    }

    // Pull specific series out of a dataset
    fn pull_series(&self, start: i64, end: i64, j: usize) -> Vec<f64> {
        (start..end).map(|t| self.table[(j, t)]).collect()
    }
}

// Reporting

// A summary table
struct Summary {
    year: Vec<i64>,
    low: Vec<f64>,
    medium: Vec<f64>,
    high: Vec<f64>,
}

impl fmt::Display for Summary {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "\n# Summary Table\n")?;
        write!(f, "{:>4}{:>12}{:>12}{:>12}", "Year", "Low", "Medium", "High")?;
        for (i, year) in self.year.iter().enumerate() {
            write!(f, "\n{:4}{:12.1}{:12.1}{:12.1}",
                year,
                100.0*self.low[i],
                100.0*self.medium[i],
                100.0*self.high[i],
            )?;
        }
        Ok(())
    }
}

// Compute a summary table
fn summary(ds: &DataSet, fc_start: i64, start: i64, end: i64) -> Summary {
    let run_scenario = |s| ds.run_simulation(fc_start, end, s).pull_series(start, end, DEBT_GDP);
    Summary {
        year: (start..end).collect(),
        low: run_scenario(Scenario::Low),
        medium: run_scenario(Scenario::Medium),
        high: run_scenario(Scenario::High),
    }
}

fn main() {
    // Example usage
    let ds = DataSet::prepare();
    println!("{}", summary(&ds, 2026, 2025, 2036));

    // Benchmark
    println!("\n# Benchmark");
    let timer = Instant::now();
    for _ in 0..100000 {
        let _ = black_box(summary(&ds, 2026, 2025, 2036));
    }
    let elapsed = timer.elapsed();
    println!("Finished 100,000 runs in {:.3} s", elapsed.as_secs_f64());
}
