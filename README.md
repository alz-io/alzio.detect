# alzio.detect

**Early diagnosis assessment for Alzheimer's disease — an R analytical toolkit.**

The gap this package fills isn't data access. The USC alzverse project (Donohue et al., 2026, *Alzheimer's & Dementia*, doi:10.1002/alz.71152) already does that well—ADNIMERGE2 and A4LEARN bundle ADNI and A4 trial data into portable R packages. What's been missing is a *unified analytical layer* that sits on top of any Alzheimer's dataset and produces clinically interpretable diagnosis assessments: cognitive test scoring with proper normative adjustments, biomarker classification following current frameworks, diagnostic accuracy with publication-ready output, risk stratification for MCI-to-dementia progression, and visualisations designed for the kinds of questions researchers actually ask.

alzio.detect is that layer. It works with data from ADNI, NACC, your own study, or the alzverse packages. Everything runs in base R with no heavy dependencies.

---

## What the package does

The package is organised around the workflow of a typical early-AD diagnostic assessment:

### 1. Data import and standardisation

`read_ad_data()` maps column names from ADNI and NACC CSV exports to a consistent internal naming scheme (20+ ADNI variables, 7 NACC variables, with room to add more). If your study uses different names, pass `source = "custom"` and it reads the file as-is. The standardised names—`id`, `visit`, `dx`, `age`, `mmse`, `cdr_sb`, `ptau`, `nfl`, and so on—are the same ones used internally by all downstream functions, so you can move from raw CSV to analysis in one call.

### 2. Cognitive test scoring

Four widely-used instruments are covered:

- **`score_mmse()`** — Sums the 30 binary items of the Mini-Mental State Examination (Folstein et al., 1975). Pass 30 item-level 0/1 responses, or a single total score that gets validated and passed through.
- **`score_moca()`** — Sums MoCA items and applies the standard +1 education adjustment for ≤12 years of schooling (Nasreddine et al., 2005, *Journal of the American Geriatrics Society*, doi:10.1111/j.1532-5415.2005.53221.x). Capped at 30.
- **`score_cdr()`** — Implements the Washington University algorithm (Morris, 1993, *Neurology*, doi:10.1212/WNL.43.11.2412) to compute the global CDR from six domain ratings (memory, orientation, judgment, community affairs, home/hobbies, personal care). Returns both the global score and the sum of boxes.
- **`score_adas_cog13()`** — Sums 13 ADAS-Cog items with flexible column-name matching, supporting both the original ADNI column conventions and bare numeric vectors (Rosen et al., 1984, *American Journal of Psychiatry*).

### 3. Normative comparison

Raw scores mean little without context. **`compute_zscore()`** converts raw test scores to Z-scores using either the internal sample distribution (useful for group comparisons) or, in future releases, external reference tables adjusted for age, education, and sex. **`compute_rci()`** implements the Jacobson-Truax Reliable Change Index (Jacobson & Truax, 1991, *Journal of Consulting and Clinical Psychology*, doi:10.1037/0022-006X.59.1.12) for longitudinal monitoring—the standard formula:

RCI = (X₂ − X₁) / (SD × √[2 × (1 − rₓₓ)])

Values outside ±1.96 indicate statistically reliable change at α = 0.05.

### 4. Biomarker analysis

**`classify_csf()`** applies the NIA-AA AT(N) research framework (Jack et al., 2018, *Alzheimer's & Dementia*, doi:10.1016/j.jalz.2018.02.018) to CSF biomarkers. Amyloid positivity (A) is determined by low Aβ42 *or* elevated p-tau/Aβ42 ratio; tau positivity (T) by elevated p-tau; neurodegeneration (N) by elevated t-tau. Default cutoffs are calibrated for the Roche Elecsys platform based on two large clinical cohorts—the Mayo Clinic Study of Aging (Campuzano et al., 2023, *DADM*, doi:10.1002/dad2.12446) which found an optimal p-tau/Aβ42 ratio of 0.023 for amyloid-PET agreement, and a Danish real-world dementia clinic study (Abildgaard et al., 2023, *Clinica Chimica Acta*, doi:10.1016/j.cca.2022.12.023) which independently found 0.029. Both were within the same Elecsys assay platform. The 0.024 default splits the difference. All cutoffs are adjustable—the function accepts them as parameters because no single threshold works across assays or populations.

**`classify_apoe_risk()`** maps APOE genotypes to risk categories using the meta-analytic odds ratios from Farrer et al. (1997, *JAMA*, doi:10.1001/jama.1997.03550160069041): ε4/ε4 carries approximately 12–15× risk versus ε3/ε3, ε3/ε4 about 3–4×, and ε2 confers a protective effect. It accepts formats like "3/4", "E3/E4", "34", and "3,4".

**`interpret_blood_biomarkers()`** handles three blood-based markers—p-tau217, NfL, and GFAP—each with assay-specific defaults and evidence-based cutoffs:

- **p-tau217** uses a 0.30 pg/mL primary cutoff, consistent with the ALZpath assay. Gonzalez-Ortiz et al. (2024, *Journal of Neurology*, doi:10.1007/s00415-023-12148-5) reported an optimal cutoff of 0.27 pg/mL for amyloid-PET positivity (6.7% FPR, 7.1% FNR). Palmqvist et al. (2025, *Nature Medicine*, doi:10.1038/s41591-025-03622-w) demonstrated AUC 0.93–0.96 across four secondary-care cohorts using the fully automated Lumipulse platform.
- **NfL** uses age-stratified cutoffs (15–45 pg/mL) derived from normative data in Simrén et al. (2024, *IJMS*, doi:10.3390/ijms25147808) and Abu-Rumeileh et al. (2023, *Scientific Reports*, doi:10.1038/s41598-023-29704-8).
- **GFAP** uses a primary cutoff of 280 pg/mL, based on the 90th percentile for healthy adults >55 years (Simrén et al., 2024).

Each returns normal/borderline/abnormal categories with the cutoff used, so you see exactly how the interpretation was reached.

### 5. Diagnostic classification

**`compute_diagnostic_accuracy()`** builds a full ROC curve from scratch in base R—no pROC dependency required. It computes the non-parametric trapezoidal AUC (Fawcett, 2006, *Pattern Recognition Letters*, doi:10.1016/j.patrec.2005.10.010), finds the optimal cutoff via Youden's J (Youden, 1950, *Cancer*, doi:10.1002/1097-0142(1950)3:1<32::AID-CNCR2820030106>3.0.CO;2-3), and returns sensitivity, specificity, PPV, NPV, positive and negative likelihood ratios, and accuracy at that cutoff—plus the full ROC curve data frame for custom plotting.

**`train_classifier()`** wraps `glm()` for logistic regression (Hosmer, Lemeshow & Sturdivant, 2013, *Applied Logistic Regression*, 3rd ed., Wiley) and `ranger::ranger()` for random forests (Wright & Ziegler, 2017, *Journal of Statistical Software*, doi:10.18637/jss.v077.i01). **`predict_classifier()`** returns predicted probabilities with confidence intervals via the delta method for GLM models.

### 6. Risk stratification

**`compute_progression_risk()`** estimates the probability of MCI-to-dementia progression within a given time horizon. It uses a logistic risk score with coefficients consistent with ADNI-based progression models—Li et al. (2019, *Journal of Alzheimer's Disease*, doi:10.3233/JAD-181025) and Gomperts et al. (2013, *Alzheimer Disease & Associated Disorders*, doi:10.1097/WAD.0b013e31826a3d21). The predictors (age, MMSE, CDR-SB, APOE4, amyloid status) were identified as strong, consistent predictors across multiple cohorts by Barnes et al. (2014, *Neurology*, doi:10.1212/WNL.0000000000000037). The intercept is calibrated for a 36-month horizon by default, with log-linear scaling for other windows.

**`classify_cognitive_stage()`** assigns one of four categories—cognitively normal, preclinical AD (Sperling et al., 2011, *Alzheimer's & Dementia*, doi:10.1016/j.jalz.2011.03.003), MCI, or dementia—using the combination of CDR global score and amyloid status. It flags inconsistencies between CDR and MMSE using the mapping established by Perneczky et al. (2006, *American Journal of Geriatric Psychiatry*, doi:10.1097/01.JGP.0000192478.82189.a8).

### 7. Visualisation

All plots use base R graphics—no ggplot2 dependency. **`plot_cognitive_profile()`** draws a radar/spider plot of domain Z-scores (Lezak et al., 2012, *Neuropsychological Assessment*, 5th ed., Oxford University Press). **`plot_longitudinal()`** displays individual and group-mean trajectories over time. **`summary_diagnostic_table()`** formats the output of `compute_diagnostic_accuracy()` into a clean 9-row table.

---

## Quick start

```r
# Install from source (once built)
install.packages("alzio.detect", repos = NULL, type = "source")

library(alzio.detect)

# Score a cognitive test
mmse_total <- score_mmse(c(1,1,1,1,1, 1,1,1,1,1, 1,1,1,1,1,
                            1,1,1,1,1, 1,1,1,1,1, 1,1,1,1,0))  # 29/30

# Or pass a total directly
score_mmse(27)

# MoCA with education adjustment
score_moca(c(5, 3, 4, 5, 3, 5), education_years = 11)  # +1 adjustment

# Classify CSF biomarkers
csf <- classify_csf(abeta42 = 850, ttau = 400, ptau = 35)
csf$ATN_class  # "A+T+N+"

# ROC analysis
truth <- c(1,1,1,0,0,1,0,0,1,0)
pred  <- c(0.9,0.8,0.7,0.3,0.2,0.85,0.1,0.4,0.75,0.25)
acc <- compute_diagnostic_accuracy(truth, pred)
summary_diagnostic_table(acc)

# Radar plot
plot_cognitive_profile(c(Memory = -1.8, Executive = -0.5,
                          Language = -0.2, Visuospatial = -1.2,
                          Attention = -0.8))
```

---

## Current status

This is an early research release (v0.0.0.9000). The scaffolding and core functions are implemented; what's still in development:

- Regression-based normative tables (currently Z-scores use sample means)
- Built-in reference datasets
- Bootstrap confidence intervals for random forest predictions
- Expanded biomarker assay coverage (Lumipulse, Mass Spec cutoffs)
- Integration examples with the alzverse package data

---

## References

The complete set of references supporting the implemented thresholds and methods is embedded in the package documentation. Key works:

| Topic | Reference |
|---|---|
| NIA-AA framework | Jack et al., 2018, *Alzheimer's & Dementia* |
| CSF Elecsys cutoffs | Abildgaard et al., 2023, *Clin Chim Acta*; Campuzano et al., 2023, *DADM* |
| Plasma p-tau217 | Palmqvist et al., 2024, *JAMA*; Palmqvist et al., 2025, *Nat Med*; Gonzalez-Ortiz et al., 2024, *J Neurol* |
| NfL / GFAP norms | Simrén et al., 2024, *IJMS*; Abu-Rumeileh et al., 2023, *Sci Rep* |
| APOE risk | Farrer et al., 1997, *JAMA*; Liu et al., 2013, *Nat Rev Neurol* |
| CDR algorithm | Morris, 1993, *Neurology* |
| MoCA validation | Nasreddine et al., 2005, *JAGS* |
| Reliable Change Index | Jacobson & Truax, 1991, *JCCP* |
| ROC methodology | Fawcett, 2006, *PRL*; Youden, 1950, *Cancer* |
| Progression models | Li et al., 2019, *JAD*; Gomperts et al., 2013, *ADAD* |
| Cognitive staging | Sperling et al., 2011, *Alzheimer's & Dementia* |
| Logistic regression | Hosmer, Lemeshow & Sturdivant, 2013, Wiley |
| Random forest (ranger) | Wright & Ziegler, 2017, *JSS* |
| alzverse data packages | Donohue et al., 2026, *Alzheimer's & Dementia* |

---

## License

GPL-3.
