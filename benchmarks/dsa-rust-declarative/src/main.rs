//  Copyright (c) 2026 Andrey Dubovik <andrei at dubovik dot eu>

// Standard library
use std::cell::RefCell;
use std::collections::BTreeMap;
use std::fmt;
use std::fs::File;
use std::hint::black_box;
use std::io::BufReader;
use std::ops::{Add, Sub, Mul, Div};
use std::time::Instant;

// Crates
use serde::Deserialize;

// Local modules
mod window;
use window::Window;


// Tags for distinguishing caches of combined series
#[derive(PartialEq, Eq, PartialOrd, Ord)]
enum CacheTag {
    Debt,
    Gdp,
    PrimarySurplus,
    PrimarySurplusGdp,
    Rate,
    Rho,
    ShareMaturing,
}

// Simulation scenario
#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
enum Scenario {
    Low,
    Medium,
    High,
}

// Period index, a year in our case
type Index = i64;

// Account value, in million euros or as a ratio
type Value = f64;

// Optional account value for historical series
struct HValue(Option<Value>);

// Algebraic operations over historical values
fn lift(f: fn(x: Value, y: Value) -> Value, x: HValue, y: HValue) -> HValue {
    HValue(match (x.0, y.0) {
        (Some(x), Some(y)) => Some(f(x, y)),
        _ => None,
    })
}

impl Add for HValue {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        lift(Value::add, self, rhs)
    }
}

impl Sub for HValue {
    type Output = Self;

    fn sub(self, rhs: Self) -> Self::Output {
        lift(Value::sub, self, rhs)
    }
}

impl Mul for HValue {
    type Output = Self;

    fn mul(self, rhs: Self) -> Self::Output {
        lift(Value::mul, self, rhs)
    }
}

impl Div for HValue {
    type Output = Self;

    fn div(self, rhs: Self) -> Self::Output {
        lift(Value::div, self, rhs)
    }
}

// Data for historical series
type Table = Window<Value>;

// A dataset with all historical data
struct DataSet {
    debt:            Table,
    debt_maturing:   Table,
    gdp:             Table,
    primary_surplus: Table,
    rate:            Table,
    rho:             Table,

    cache: RefCell<BTreeMap<(CacheTag, Scenario, Index), Value>>,
}

// A function signature for any historical series
type HistoricalSeries = fn(&DataSet, Index) -> HValue;

// A function signature for any simulated series
type ForecastSeries = fn(&DataSet, Scenario, Index) -> Value;

// A function signature for a specific forecast
// (This is a forecast that can return None for years where it is not defined)
type SpecificForecast = fn(&DataSet, Scenario, Index) -> HValue;

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

// Construct a table from JSON data
fn series_window(series: &RawSeries) -> Table {
    let lb = series.values.iter().map(|v| v.year).min().expect("JSON series are non-empty");
    let ub = series.values.iter().map(|v| v.year).max().expect("JSON series are non-empty") + 1;
    let mut table = Table::new(lb, ub);
    for v in &series.values {
        table[v.year] = v.value;
    }
    table
}

// Load a table from a JSON file
fn load_table(path: &str) -> Table {
    println!("reading {path}");
    let file = File::open(path).expect("JSON file exists");
    let reader = BufReader::new(file);
    let series = serde_json::from_reader(reader).expect("JSON file contains valid series");
    series_window(&series)
}

// Load all historical data
fn load_dataset() -> DataSet {
    println!("# Data Loading");
    DataSet {
        debt:            load_table("data/debt.json"),
        debt_maturing:   load_table("data/debt-maturing.json"),
        gdp:             load_table("data/gdp.json"),
        primary_surplus: load_table("data/primary-surplus.json"),
        rate:            load_table("data/rate-newdebt.json"),
        rho:             load_table("data/rate-implicit.json"),

        cache: RefCell::new(BTreeMap::new()),
    }
}

// Get a table element or None if out of bounds on the right
trait IndexMaybe {
    fn index_maybe(&self, index: i64) -> HValue;
}

impl IndexMaybe for Table {
    fn index_maybe(&self, index: i64) -> HValue {
        HValue(
            if index < self.lb {
                panic!("left out-of-bounds")
            } else if index < self.ub {
                let index = (index - self.lb) as usize; // always safe in this branch
                Some(self.data[index])
            } else {
                None
            }
        )
    }
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


impl DataSet {
    // Concatenate historical and simulated series
    fn with_history(&self, tag: CacheTag, h: HistoricalSeries, f: ForecastSeries, s: Scenario, t: Index) -> Value {
        let k = (tag, s, t);
        if let Some(v) = self.cache.borrow().get(&k) {
            return *v;
        }
        let v = h(self, t).0.unwrap_or_else(|| f(self, s, t));
        self.cache.borrow_mut().insert(k, v);
        v
    }

    // Stack forecast series
    fn stack_forecasts(&self, f1: SpecificForecast, f2: ForecastSeries, s: Scenario, t: Index) -> Value {
        f1(self, s, t).0.unwrap_or_else(|| f2(self, s, t))
    }

    // Compute quantiles over historical series
    fn quantile(&self, h: HistoricalSeries, start: Index, end: Index, k: usize, q: usize) -> HValue {
        let s = (end - start).try_into().expect("end >= start");
        let mut x = vec![Value::default(); s];
        for (xi, t) in x.iter_mut().rev().zip((start..end).rev()) {
            match h(self, t).0 {
                Some(v) => *xi = v,
                None => return HValue(None),
            };
        }

        HValue(Some(quantile(x, k, q)))
    }

    // Outstanding debt

    fn debt_hs(&self, t: Index) -> HValue {
        self.debt.index_maybe(t)
    }

    fn debt_fc(&self, s: Scenario, t: Index) -> Value {
        self.debt(s, t-1) - self.surplus(s, t-1)
    }

    fn debt(&self, s: Scenario, t: Index) -> Value {
        self.with_history(CacheTag::Debt, DataSet::debt_hs, DataSet::debt_fc, s, t)
    }

    // Maturing debt

    fn debt_maturing_hs(&self, t: Index) -> HValue {
        self.debt_maturing.index_maybe(t)
    }

    fn share_maturing_hs(&self, t: Index) -> HValue {
        self.debt_maturing_hs(t)/self.debt_hs(t)
    }

    fn share_maturing_fc(&self, s: Scenario, t: Index) -> Value {
        self.share_maturing(s, t-1)
    }

    fn share_maturing(&self, s: Scenario, t: Index) -> Value {
        self.with_history(CacheTag::ShareMaturing, DataSet::share_maturing_hs, DataSet::share_maturing_fc, s, t)
    }

    // Interest rate on new debt

    fn rate_hs(&self, t: Index) -> HValue {
        self.rate.index_maybe(t)
    }

    fn rate_fc(&self, _s: Scenario, _t: Index) -> Value {
        panic!("undefined interest rate on new debt")
    }

    fn rate(&self, s: Scenario, t: Index) -> Value {
        self.with_history(CacheTag::Rate, DataSet::rate_hs, DataSet::rate_fc, s, t)
    }

    // GDP

    fn gdp_hs(&self, t: Index) -> HValue {
        self.gdp.index_maybe(t)
    }

    fn gdp_fc(&self, s: Scenario, t: Index) -> Value {
        let nominal_growth = 1.03;
        self.gdp(s, t-1)*nominal_growth
    }

    fn gdp(&self, s: Scenario, t: Index) -> Value {
        self.with_history(CacheTag::Gdp, DataSet::gdp_hs, DataSet::gdp_fc, s, t)
    }

    // Primary budget surplus (nominal)

    fn primary_surplus_hs(&self, t: Index) -> HValue {
        self.primary_surplus.index_maybe(t)
    }

    fn primary_surplus_fc(&self, s: Scenario, t: Index) -> Value {
        self.primary_surplus_gdp(s, t)*self.gdp(s, t)
    }

    fn primary_surplus(&self, s: Scenario, t: Index) -> Value {
        self.with_history(CacheTag::PrimarySurplus, DataSet::primary_surplus_hs, DataSet::primary_surplus_fc, s, t)
    }

    // Primary budget surplus (share of GDP)

    fn primary_surplus_gdp_hs(&self, t: Index) -> HValue {
        self.primary_surplus_hs(t)/self.gdp_hs(t)
    }

    fn primary_surplus_gdp_fc1(&self, s: Scenario, t: Index) -> HValue {
        let window = 30;
        let k = match s {
            Scenario::Low => 1,
            Scenario::Medium => 5,
            Scenario::High => 9,
        };
        self.quantile(DataSet::primary_surplus_gdp_hs, t - window, t, k, 10)
    }

    fn primary_surplus_gdp_fc(&self, s: Scenario, t: Index) -> Value {
        self.primary_surplus_gdp(s, t-1)
    }

    fn primary_surplus_gdp(&self, s: Scenario, t: Index) -> Value {
        // This was that much nicer in Haskell thanks to currying
        self.with_history(
            CacheTag::PrimarySurplusGdp,
            DataSet::primary_surplus_gdp_hs,
            |ds, s, t| ds.stack_forecasts(
                DataSet::primary_surplus_gdp_fc1,
                DataSet::primary_surplus_gdp_fc,
                s, t,
            ),
            s, t,
        )
    }

    // Budget surplus

    fn surplus(&self, s: Scenario, t: Index) -> Value {
        self.primary_surplus(s, t) - self.debt(s, t)*self.rho(s, t)
    }

    // Implicit interest rate

    fn rho_hs(&self, t: Index) -> HValue {
        self.rho.index_maybe(t)
    }

    fn rho_fc(&self, s: Scenario, t: Index) -> Value {
        let debt_total = self.debt(s, t);
        let debt_deferred = (1.0 - self.share_maturing(s, t-1))*self.debt(s, t-1);
        let interest_new = (debt_total - debt_deferred)*self.rate(s, t);
        let interest_deferred = debt_deferred * self.rho(s, t-1);
        (interest_deferred + interest_new)/debt_total
    }

    fn rho(&self, s: Scenario, t: Index) -> Value {
        self.with_history(CacheTag::Rho, DataSet::rho_hs, DataSet::rho_fc, s, t)
    }

    // Auxiliary series

    // End-of-year debt to GDP (aligns with official reporting)
    fn debt_gdp(&self, s: Scenario, t: Index) -> Value {
        self.debt(s, t+1)/self.gdp(s, t)
    }
}

// Reporting

// A row in a summary table
struct SummaryRow {
    year:   Index,
    low:    Value,
    medium: Value,
    high:   Value,
}

impl fmt::Display for SummaryRow {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:4}{:12.1}{:12.1}{:12.1}",
            self.year,
            100.0*self.low,
            100.0*self.medium,
            100.0*self.high,
        )
    }
}

// A summary table
struct Summary(Vec<SummaryRow>);

impl fmt::Display for Summary {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "\n# Summary Table\n")?;
        write!(f, "{:>4}{:>12}{:>12}{:>12}", "Year", "Low", "Medium", "High")?;
        for row in &self.0 {
            write!(f, "\n{row}")?;
        }
        Ok(())
    }
}

// Compute a summary table
impl DataSet {
    fn summary(&self, start: Index, end: Index) -> Summary {
        Summary((start..end).map(|t| SummaryRow {
            year:   t,
            low:    self.debt_gdp(Scenario::Low, t),
            medium: self.debt_gdp(Scenario::Medium, t),
            high:   self.debt_gdp(Scenario::High, t),
        }).collect())
    }
}

fn main() {
    // Example usage
    let ds = load_dataset();
    println!("{}", ds.summary(2025, 2036));

    // Benchmark
    println!("\n# Benchmark");
    let timer = Instant::now();
    for _ in 0..100000 {
        ds.cache.borrow_mut().clear();
        let _ = black_box(ds.summary(2025, 2036));
    }
    let elapsed = timer.elapsed();
    println!("Finished 100,000 runs in {:.3} s", elapsed.as_secs_f64());
}
