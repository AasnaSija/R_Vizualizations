# R Data Visualizations: Remittances, Migration & Gender

This repository contains R code for 8 data visualizations exploring remittance patterns, migration, and gender development outcomes. The work draws on survey and panel data from the Philippines and Nepal, as well as a cross-country dataset on remittance dependence.

---

## File

### `AasnaSijapati_DataViz.R`

The script is organized into three thematic sections:

**Section 1 — Philippine Overseas Workers** *(Survey of Overseas Filipino Workers, PSA 2023)*
- Stacked bar chart: average remittances by marital status and gender
- Boxplot: remittance distribution by education level and gender

**Section 2 — Nepal, Chitwan Valley** *(Chitwan Valley Family Study, 1995–2019)*
- Line chart: average remittances over time with earthquake annotation (2015)
- Faceted line chart: remittance trends by ethnic group (2006–2015)
- Treemap: household income composition (crop sales, remittances, livestock, rental)

**Section 3 — Cross-Country Gender Development Outcomes**
- Scatter plot: Female HDI vs remittance dependence (% of GDP), by income group
- Scatter plot: Female labor force participation vs remittance dependence
- Horizontal bar chart: mean years of schooling by gender in high-remittance countries (≥15% of GDP)

---

## Key Packages

`ggplot2` · `dplyr` · `tidyr` · `haven` · `readxl` · `scales` · `treemapify` · `stringr`

---

## Data Sources

- [Survey of Overseas Filipino Workers](https://psa.gov.ph/) — Philippine Statistics Authority, 2023
- [Chitwan Valley Family Study](https://www.icpsr.umich.edu/web/DSDR/studies/36755) — ICPSR / Data Sharing for Demographic Research, 1995–2019
- Cross-country dataset: compiled from World Bank and UNDP indicators
