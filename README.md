# Gone Golfing

A national valuation of U.S. golf course land at its Highest and Best Use - with a Honolulu County reality check.

This repo is the home of my master's thesis at the University of Hawai`i at Mānoa. The short version: I took every golf course in the United States I could find (16,297 of them), figured out roughly what each one's land would be worth if it were used for something other than golf, and added it all up. The number turned out to be much larger than I expected, but the more interesting story is what happened when I tried to validate that number against the parcel-level tax records for the island I actually live on.

A heads-up before you dive in: this isn't a small project. There are seven phases, a Hawaii micro-study that anchors the whole thing empirically, and - as a check against implementation error - the entire pipeline written independently three times, in Python, R, and Julia. If you only have time for one document, read [`Phase7_Summary.md`](https://github.com/mmanolak/Gone_Golfing/blob/main/2%20-%20Work/Phase%207%20Documentation%2C%20Discussion%20and%20Write%20Up/other/Phase7_Summary.md) - it's the cover-all overview.

## The findings, very briefly

The aggregate national figure landed at approximately **$936 billion** under a full Multiple Imputation pipeline ($M = 100$, pooled via Rubin's Rules), with an observed-only floor of **$790 billion** when I restricted the analysis to courses with both directly measured polygon acreage and observed county land prices.

The Honolulu County validation is where it gets interesting. Comparing the model's Highest-and-Best-Use estimate against the City and County's own assessment records, golf land is carried on the tax rolls at **roughly one to three percent** of what the residential market would pay for the same acreage - a model-to-assessed ratio of **33.8× at Waialae Country Club and 90.8× at Turtle Bay**. That gap isn't a modeling error. Both courses are assessed under Honolulu tax class 6, *Preservation*, and it's that classification - not the land's location or physical characteristics - that produces the number.

Which leads to the finding I call the **Preservation Paradox**: **81.0% of Oahu's golf footprint sits in Preservation or Federal/Military zones where residential redevelopment is statutorily prohibited**, leaving roughly 4.7% of the gross opportunity cost legally realizable under current zoning. The economic potential is real and concentrated; the share of it that current law actually permits to be unlocked is much smaller.

For the full numbers and how I got them, the thesis PDF and the Phase Summary files are the right place to look.

## What you'll find here

The work is organized into seven phases, each with its own folder, its own Documentation file (the detailed lab-notebook version), and its own Summary file (the short README-style version you're probably looking for first):

- **Phase 1** - Spatial parsing and economic baseline valuation. Joining 16,297 golf courses to U.S. counties and merging in FHFA residential and USDA agricultural land prices.
- **Phase 2** - OSM polygon extraction and acreage matching. Pulling golf course geometries out of an 11 GB OpenStreetMap extract and computing per-course acreage.
- **Phase 3** - MICE imputation and Rubin's Rules pooling. Closing the missing-data gap and producing the headline national figure.
- **Phase 4** - Econometric modeling. The OLS regression that decomposes opportunity cost into structural and geographic drivers.
- **Phase 5** - Hawaii micro-study and empirical validation. The parcel-level Honolulu County analysis that anchors the national figure.
- **Phase 6** - Visualization and publication-ready output. The maps, charts, and LaTeX table fragments that appear in the thesis.
- **Phase 7** - Cross-phase synthesis. The meta-summary that ties everything together, plus the AI tool disclosure.

## Getting set up

If you want to run the pipelines yourself, head to the `0 - Scripts and Codes` folder. There are three setup scripts - one per language - that will install all the required packages:

- `install_packages.py` for Python
- `install_packages.r` for R
- `install_packages.jl` for Julia

Run whichever ones correspond to the languages you actually want to use. You don't need all three to do anything useful - the figures reported in the thesis come from the R implementation - but if you want to reproduce the cross-language consistency check, you'll need all three.

After installation, each phase folder has its own ordered scripts (typically `Phase_X.py`, `Phase_X.R`, `Phase_X.jl`). The pipelines are designed to be run in phase order - Phase 1 produces files that Phase 2 reads, and so on.

## Reading deeper

For someone landing on the repo cold, I'd suggest this reading order:

1. **[`Phase7_Summary.md`](https://github.com/mmanolak/Gone_Golfing/blob/main/2%20-%20Work/Phase%207%20Documentation%2C%20Discussion%20and%20Write%20Up/other/Phase7_Summary.md)** - the cover-all overview, including the arc across all phases and the AI tool disclosure.
2. **[`Phase5_Summary.md`](https://github.com/mmanolak/Gone_Golfing/blob/main/2%20-%20Work/Phase%205%20The%20Hawaii%20Micro-Case%20Study/00%20-%20Phase5_Summary.md)** - the Hawaii micro-study, which is where the thesis actually earns its claims.
3. **The thesis PDF** - the formal write-up. The Phase Summaries condense it; the PDF has the full theoretical framework, literature review, and discussion.
4. **The other Phase Summaries** in numerical order, if you want to see how each step of the pipeline works.
5. **The Documentation files** - the long-form lab-notebook versions for anyone who wants the full technical detail, including script inventories, code review notes, and debugging history.

## Corrections and issue tracking

Errors found during development are logged, investigated, and - where they don't survive scrutiny - retracted, in [`Issue_Register.md`](https://github.com/mmanolak/Gone_Golfing/blob/main/Issue_Register.md). Two worth flagging for anyone reading the code:

- **P3-09** - the Rubin's Rules pooling routine originally computed the within-imputation variance term from cross-sectional dispersion across courses rather than treating the estimand as a census total. Point estimates are unaffected; reported confidence intervals change. Corrected across all six call sites.
- **P5-20** - an apparent reproducibility discrepancy in the Oahu Step 3 figure, opened and then **retracted**: it was a transposition in my own note-taking, not a pipeline fault. All three implementations reproduce their own frozen output to within 0.002%.

## A note on AI tools

I used several large language models as writing aids and coding assistants throughout this project - both web-hosted (Claude Opus, Claude Sonnet, Gemini 2.5 Pro, Gemini 3.1 Pro) and locally-hosted on my own hardware (Kimi-Dev-72B, Qwen3-Coder-Next, GPT-OSS-120B Heretic, Qwen2.5-Coder-1.5B, Granite-4.0-H-Tiny). The full disclosure, including roles and hardware specifics, is in [`Phase7_Summary.md`](https://github.com/mmanolak/Gone_Golfing/blob/main/2%20-%20Work/Phase%207%20Documentation%2C%20Discussion%20and%20Write%20Up/other/Phase7_Summary.md). Final responsibility for all analytical decisions, data interpretation, and prose authorship rests with me.

## Author

**Michael Manolakis** - University of Hawai`i at Mānoa