# Phase 7 - Cross-Phase Synthesis

Phase 7 is the meta-phase: it does not run new analyses or produce new outputs. Instead, it captures the arc that ties Phases 1 through 6 into a single coherent research project, and frames the work for readers landing on this repository for the first time.

The thesis answers two coupled questions. The first is **macroeconomic**: what is the aggregate financial opportunity cost of U.S. golf courses when each course is valued at its Highest and Best Use (HBU) counterfactual instead of its current recreational use? The second is **methodological**: when that national-scale HBU model is validated against parcel-level municipal records in a single high-value coastal county, does the model's estimate hold up, and what share of the modeled opportunity cost is actually unlockable under current zoning law? The answer to the first is approximately **$944 billion**, with an observed-only floor of **$788 billion**. The answer to the second is that the model exceeds municipal tax assessments by a factor of approximately **1.33×** in the Hawaii pilot, and that **81.7% of Oahu's golf footprint sits in Preservation or Federal/Military zones** where redevelopment is currently statutorily prohibited.

These two answers are the load-bearing findings of the thesis. Everything between them is a pipeline that produces them defensibly.

## The arc across the six computational phases

**Phase 1** ingests a raw CSV of 16,297 U.S. golf courses with GPS coordinates and produces a per-course `Baseline_Value_Per_Acre` by spatially joining each course to its U.S. county and merging in two economic proxies - the FHFA Residential Land Price index for urban counties (RUCC 1–3) and the USDA Agricultural Land Value for rural counties (RUCC 4–9). This dual-proxy approach is the methodological foundation of the entire project: it is what allows the same HBU framework to apply to a Manhattan urban course and a rural Iowa course without nationally over- or under-stating opportunity cost.

**Phase 2** measures each course's physical footprint by extracting golf course boundary polygons from an 11 GB OpenStreetMap PBF file and spatially matching them to the Phase 1 course list. A two-pass spatial join (direct intersect, then nearest-neighbor with a 500-meter cap) recovers acreage for 71.2% of the courses, leaving 28.8% flagged as imputation targets.

**Phase 3** closes the missing-data gap. Multiple Imputation by Chained Equations (MICE) with a Random Forest backend, run independently in three languages with $M = 100$ imputations per language and pooled via Rubin's Rules, produces the headline national aggregate of approximately $944 billion. The cross-language spread of 1.6% on a $940B base, with overlapping 95% confidence intervals across all three implementations, is the robustness check that justifies trusting the figure.

**Phase 4** decomposes the per-course opportunity cost into its structural and geographic determinants via a logarithmic OLS regression with HC1 robust standard errors, fit on each of the 100 imputed datasets per language and pooled via Rubin's Rules. The two-covariate model (Holes + Urban indicator) produces coefficients that are extraordinarily consistent across the three languages: the urban coefficient is approximately $\beta_2 \approx 4.0$–$4.2$ in all three, implying that urban courses are valued at approximately 60× rural courses on a per-acre basis. The Holes coefficient carries a circularity caveat the urban coefficient doesn't: `Holes` is also a MICE predictor for the imputed acreage/value columns the dependent variable is built from, so the full-sample estimate partly reflects that dependency for the imputed rows — Appendix A.4 reports the observed-only estimate as the substantive Holes finding.

**Phase 5** is the empirical anchor. The Honolulu County micro-study integrates the national pipeline outputs with parcel-level cadastral, tax-roll, and zoning data published by the City and County of Honolulu, producing the 1.33× model-to-assessed ratio (the gross-vs-current-use anchor) and the 81.7% Preservation/Federal share (the legally-permissible HBU bound). The Phase 5b automated pipeline operates at parcel resolution across 1,072 unique TMK parcels and 33 deduplicated Oahu courses, producing an aggregate Oahu opportunity cost that aligns closely with what the national pipeline predicts for Honolulu County independently.

**Phase 6** transforms the numerical outputs into publication-ready figures, maps, and LaTeX table fragments. R produces the cartographic output (national choropleths, Oahu maps, bivariate maps); Julia produces the statistical charts (forest plot, density diagnostics, Hawaii Gap dumbbell, Preservation Paradox waffle, Lorenz curve) and the LaTeX table fragments that drop directly into the thesis source.

## Why the design is shaped this way

Three structural choices distinguish this project from a more typical single-language land-use econometric study, and each is deliberate.

**Tri-language implementation across Phases 1 through 5.** Every step from data ingestion through the Hawaii micro-study is implemented independently in Python, R, and Julia. The point is robustness rather than redundancy: any divergence in aggregate estimates across the three languages signals a pipeline defect rather than a substantive empirical finding, and the cross-language convergence on $943B / $936B / $951B (with overlapping 95% CIs) is what gives the headline figure its defensibility against backend-specific implementation choices in MICE, OLS, or spatial join routines.

**Explicit two-scale design.** The national pipeline establishes breadth across 16,297 courses and 50 states; the Hawaii micro-study establishes depth at parcel resolution within a single county. Neither alone would carry the thesis: a national figure with no parcel-level anchoring would be a theoretical exercise, and a Hawaii-only study would be too small to support a national policy conclusion. The two scales are complementary, with the national pipeline's geographic resolution validated by the Hawaii pipeline's parcel-level checks.

**Honest treatment of what the model does and does not measure.** The national-scale figure is the *gross HBU counterfactual*, not the net opportunity cost. The current-use value $V_{Current}$ is not directly observable in federal land valuation datasets, so the headline figure is an upper bound until empirically anchored against the Hawaii tax-assessment data. Similarly, the unrestricted HBU is bounded by the legally-permissible HBU once zoning is taken into account, and the Preservation Paradox finding establishes that the unrestricted figure substantially overstates the realizable opportunity cost in at least one high-value market. The thesis carries both figures and is explicit about the gap between them.

## What follows from these findings

Three directions for follow-on research are explicit in the thesis. First, the Phase 5b zoning intersection methodology should be replicated in other high-value coastal markets - the Bay Area, Los Angeles, southern Florida, the New York metro - to test whether the Preservation Paradox documented on Oahu generalizes. Second, $V_{Current}$ should be measured directly using transaction data from the National Golf Foundation or income-capitalization methods, refining the 1.33× Hawaii anchor into a market-specific net opportunity cost. Third, a renewable energy counterfactual ($V_{Renewable}$) should be integrated formally into the HBU framework as a third candidate use, particularly relevant in arid urban environments where the ecological cost of maintaining turf is high.

Two further extensions are worth flagging. A reasonable critique of the present framework is that in many rural markets characterized by agricultural saturation, the current recreational use of a golf course may itself represent the local Highest and Best Use - particularly where surrounding agricultural land already exceeds regional demand. A rural-HBU inversion analysis would partition the dataset by regional ag-land saturation indices and recompute the aggregate excluding courses where rural HBU is plausibly already met by current use. Separately, the present analysis is anchored to 2022 FHFA residential land prices to maintain temporal consistency; a targeted sensitivity check using 2025 FHFA data for the urban subset would test whether the headline figure is robust to recent residential land price movements.

The dataset, the pipeline, and the validation logic are all structured to support these extensions without re-architecting the project.

## Disclosure of generative AI tool use

For academic transparency, the following large language models contributed to this project at various points in its development. The models are organized into web-hosted agents (where queries traverse a third-party cloud provider) and locally-hosted agents (where inference runs on my own hardware), reflecting the different confidentiality and reproducibility implications of each.

### Web-hosted agents

- **Claude Opus 4.7** - Anthropic - accessed via the Claude web app on a Windows 11 PC. Used as a writing aid, redundancy checker, and structural reviewer; ideas and draft passages were exchanged with the model to surface critical flaws or productive next directions.
- **Claude Sonnet 4.6** - Anthropic - accessed via the Claude web app on a Windows 11 PC. Used for code verification and bug-fixing across the Python, R, and Julia pipelines.
- **Gemini 2.5 Pro** - Google - accessed via Google AI Studio web app on a Windows 11 PC. Used as a research-management assistant, primarily to track sources, document why each was used, and maintain a working index of the project's reference material.
- **Gemini 3.1 Pro** - Google - accessed via Google AI Studio web app on a Windows 11 PC. Used as a writing aid, redundancy checker, and structural reviewer; complementary to Claude Opus 4.7 to provide a second model's perspective on draft passages and analytical decisions.

### Locally-hosted agents

All locally-hosted models run on the FishTex Nimo PC system specified below, with inference handled by `llama.cpp` (Vulkan, ROCm, and CUDA backends) under Fedora 43 Server.

- **Kimi-Dev-72B (Q8_K_XL)** - Moonshot AI, with quantization by unsloth (`https://huggingface.co/unsloth/Kimi-Dev-72B-gguf`). Used for code writing, scaffolding new functions, validation, and error correction.
- **Qwen3-Coder-Next (UD Q8_K_XL)** - Alibaba, with quantization by unsloth (`https://huggingface.co/unsloth/Qwen3-Coder-Next-gguf`). Used for code writing, scaffolding, validation, and error correction; complementary to Kimi-Dev-72B as a second-opinion code generator.
- **GPT-OSS-120B Heretic (Q8_0)** - OpenAI base weights with kldzj fine-tune, distributed via Bartowski (`https://huggingface.co/bartowski/kldzj_gpt-oss-120b-heretic-GGUF`). Used as a fully private personal assistant for thought processing and information tracking - the locally-hosted counterpart to the web-hosted Gemini 2.5 Pro role.
- **Qwen2.5-Coder-1.5B-Instruct (Q8_0)** - Alibaba, with quantization by Bartowski (`https://huggingface.co/bartowski/Qwen2.5-Coder-1.5B-Instruct-GGUF`). Used for in-editor text prediction and ghost-text autocomplete as a Copilot replacement; runs on the 3060ti via the Thunderbolt 4 eGPU dock for low-latency response.
- **Granite-4.0-H-Tiny (Q5_K_M)** - IBM Research (`https://huggingface.co/ibm-granite/granite-4.0-h-tiny-GGUF`). Used for in-editor text prediction and ghost-text autocomplete as a Copilot replacement; complementary to Qwen2.5-Coder-1.5B and likewise runs on the 3060ti via the Thunderbolt 4 eGPU dock.

### Hardware

**FishTex** - Nimo PC AMD Ryzen AI Max 395 system (`https://www.nimopc.com/products/nimo-ai-mini-pc-amd-ryzen-ai-max-395-128gb-ram?variant=47848771846395`); 128 GB LPDDR5 RAM at 8000 MHz; 2 × 2 TB NVMe SSDs (WD Black for OS, Crucial P3 for `.gguf` model storage); Fedora 43 Server; `llama.cpp` with Vulkan, ROCm, and CUDA backends; supporting software including Docker, OpenWebUI, and SSH; a Gigabyte 3060ti Vision 8 GB GPU connected via Thunderbolt 4 to a Razer Core X eGPU dock for the small autocomplete models.

The remaining three machines below are my general work environments; they were used for thesis writing, code editing, and analytical work but did not host any local LLM inference.

**PadTex** - Lenovo P52; Xeon E2176M; 56 GB SODIMM DDR4 (3 × 16 GB + 8 GB); 500 GB Samsung MZVLB512HAJQ + Sabrent Rocket 4.0 2 TB; Quadro P2000; Intel AX210 Wi-Fi; Windows 11 25H2 with AtlasOS modifications.

**ThinkTex** - Lenovo L15; AMD Ryzen 5 PRO 4650U; 24 GB SODIMM DDR4 (16 GB + 8 GB); 512 GB Kioxia XG6 KXG60ZNV512G; Windows 11 25H2 with AtlasOS modifications.

**MikTex** - Custom tower PC; AMD Ryzen 9 3900XT; Gigabyte B550M Aorus Elite (Rev. 1.3); DarkRock D360 liquid cooler; Lian Li A3 case; Montech Centru II 1050 W PSU; Quadro P2000 5 GB; 64 GB G.Skill Ripjaws V DDR4 (2 × 32 GB at 3600 MHz CL18); 1 TB Sabrent Rocket 4.0.

The majority of the data processing and code authorship was conducted on **MikTex**, with lighter analytical work performed on **PadTex** as a portable alternative. **ThinkTex** served primarily as a portable client to SSH into the FishTex or MikTex systems for remote work.

### Statement of authorial responsibility

Final responsibility for all analytical decisions, data interpretation, empirical claims, and prose authorship rests with me. The use of these tools is disclosed here in keeping with emerging academic norms around AI assistance. Citation formats for individual model contributions will be finalized in alignment with the requirements of the eventual publication venue.