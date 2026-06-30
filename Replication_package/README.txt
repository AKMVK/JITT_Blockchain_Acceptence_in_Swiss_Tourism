# Replication package — Blockchain Acceptance in Swiss Tourism

Reproduces all computed statistics in the manuscript from the cleaned survey data.

## Files

| File | Description |
|------|-------------|
| `blockchain_acceptance_analysis.R` | All analyses, organised by table (final manuscript numbering). |
| `data/Zukunftstechnologien_bereinigt.xlsx` | Cleaned data set, N = 1244, complete on all acceptance items. |
| `construct_item_map.csv` | Mapping of each construct to its survey items. |
| `documentation/Codebuch.xlsx` | Codebook: variable descriptions and value labels. |
| `documentation/Variablenbeschreibungen_und_-werte.xlsx` | Variable descriptions and values (full export). |
| `documentation/Fragebogen_Blockchain.pdf` | Original questionnaire (item wording, German). |

## Requirements

R ≥ 4.2 with: `readxl`, `dplyr`, `lavaan`, `semTools`, `psych`.

```r
install.packages(c("readxl","dplyr","lavaan","semTools","psych"))
```

## How to run

1. Keep the folder structure: the script reads `data/Zukunftstechnologien_bereinigt.xlsx`
   (relative path), so set the working directory to **this** `replication_package/` folder.
2. Open `blockchain_acceptance_analysis.R` and run it top to bottom. Each block is
   headed by the table it produces and prints a labelled result to the console.

## Which block produces which table

| Script block | Manuscript table |
|--------------|------------------|
| Descriptives & Welch t-tests (UC1 / UC2) | Table 4 / Table 5 |
| Hierarchical OLS regression (UC1 / UC2)  | Table 6 / Table 8 (Model 1 = baseline) |
| CB-SEM treatment model, pooled (UC1 / UC2) | Table 7 / Table 9 |
| CFA reliability & validity (UC1 / UC2)   | Table A1 / Table A4 |
| Fornell–Larcker (UC1 / UC2)              | Table A2 / Table A5 |
| Standardised loadings (UC1 / UC2)        | Table A3 / Table A6 |
| Structural paths by blockchain condition | Table A7 / Table A8 |
| Item-level descriptives                  | Table A10 |

Tables 1–3 (dimensions, personas, demographics) and A9 (construct→item map) are
definitional/descriptive; the construct→item map is also provided as
`construct_item_map.csv`.

## Group coding (intentional asymmetry)

| Use Case | Variable | "With blockchain" | "Without blockchain" |
|----------|----------|-------------------|----------------------|
| 1 (hotel) | `HOTEL` | `HOTEL == 1` (n = 656) | `HOTEL == 2` (n = 588) |
| 2 (tour)  | `TOUR`  | `TOUR == 2` (n = 631) | `TOUR == 1` (n = 613) |

The coding is confirmed by manipulation checks: the with-blockchain hotel group
(`HOTEL == 1`) rates review **credibility** higher (3.39 vs. 3.28, t = 2.33, p = .020),
and the with-collectible tour group (`TOUR == 2`) rates the **attractiveness of the
additional offer** higher (3.30 vs. 3.03, t = 4.64, p < .001).

## Methods summary

* Composite (regression) scores are unweighted means of the items (1–5 Likert).
* CFA / CB-SEM: maximum likelihood, FIML for missing data (`lavaan`).
* Reliability/validity: Cronbach's α (`psych`), composite reliability ω and AVE
  (`semTools`), discriminant validity via the Fornell–Larcker criterion.
* Group comparisons: Welch's two-sample t-tests.
* Hierarchical OLS: M1 predictors → M2 + blockchain dummy → M3 + interactions.

A Python cross-check (`semopy`, `statsmodels`, `scipy`) was used during development;
all reported figures match across the two implementations within rounding.
