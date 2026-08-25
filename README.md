# Delay Sensitivity in Stated Vaccine Choice

A discrete choice experiment examining how waiting time shapes vaccination preferences in Wuhan, China.

**Target journal:** Journal of Behavioral Medicine

## Repository structure

```
.
├── manuscript/              # LaTeX source + figure
│   ├── main.tex             # Main manuscript
│   ├── appendix_*.tex       # Online supplementary material (A–F)
│   ├── references_cleaned.bib
│   └── effect_waiting_time_vaccine_choice.jpg  # Figure 1
├── analysis/
│   ├── 01_mixed_logit.R     # Primary mixed logit model
│   ├── 02_descriptive_stats.R   # Sample descriptives
│   └── 03_discount_rate.R       # Discount rate estimation
├── data/
│   ├── dce_data_with_optout.xlsx  # Respondent-level DCE data
│   ├── Fielded_DCE_Design.csv     # Authoritative design matrix
│   └── gen_appendix_b.py          # Auto-generates Appendix B Table from CSV
└── .gitignore
```

## Reproducibility

1. **Data:** `data/dce_data_with_optout.xlsx` — respondent-level choice data (N=1,027)
2. **Design:** `data/Fielded_DCE_Design.csv` — the 96-row fielded design matrix
3. **Analysis:** Run `analysis/01_mixed_logit.R` in R with `mlogit` and `data.table`
4. **Manuscript:** Compile `manuscript/main.tex` with `pdflatex + bibtex + pdflatex`

## Key findings

- Waiting time significantly reduces stated vaccine choice (mixed logit, β=−0.297, p<0.001)
- Substantial heterogeneity in delay sensitivity (SD of random coefficient = 1.272)
- Exploratory trust subgroup analyses suggest variation in sensitivity, but not robustly supported by continuous specification tests
- Respondents require ~99 RMB/month compensation to accept additional waiting time

## Contact

Yichao Jin — Yichao.Jin@UTDallas.edu