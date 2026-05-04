# AEM 7010: AI-tools running exercise

This repository holds the multi-department PhD placements study built across the AI-tools module of AEM 7010.

## Project structure

Functional folders, no chronology.

- `code/`: R scripts, one per task. Each script must run end-to-end from a fresh R session.
- `data/`: input data and processed CSVs. Schemas below.
- `data/cache/`: committed cache files for LLM-in-the-loop steps.
- `output/`: AI-drafted intermediate reports, tables, and figures.
- `paper/`: researcher-authored prose. The agent assists with code only here. (Empty for now.)

## The five departments

| dept code | URL |
|---|---|
| `dyson` | https://dyson.cornell.edu/programs/graduate/placements/ |
| `berkeley` | https://are.berkeley.edu/graduate/job-market-placement |
| `davis` | https://are.ucdavis.edu/graduate/phd-program/placement |
| `minnesota` | https://apec.umn.edu/graduate/job-placements |
| `wisconsin` | https://aae.wisc.edu/graduate-programs/placement/ |

## Data schemas

`data/placements_<dept>.csv` (per-department scraper output): `name`, `year`, `placement`, `source_url` in this order. The `placement` column is the job title joined to the institution by " at ", e.g. "Assistant Professor at University of Illinois Urbana-Champaign". The `source_url` column is the dept's page URL repeated on every row.

`data/placements_all.csv` (stacked): `dept`, `name`, `year`, `placement`, `source_url`.

`data/placements_all_classified.csv` (classified): the columns above plus `class_llm` with values in {`academic`, `government`, `industry`, `other`}.

`data/cache/llm_responses.csv` (LLM cache, one row per unique placement string): `placement`, `model`, `date_run`, `raw_response`, `label`.

## Scraping conventions

- Use only `rvest` and `readr`.
- Anchor selectors on stable text (heading text, recognizable column header), not CSS class names.
- Drop rows where all data cells are empty.
- End each script with a `message()` reporting the row count.

## LLM classifier conventions

- Use only `ellmer`.
- Pin the model to `claude-haiku-4-5-20251001`.
- Set sampling to deterministic: `params = params(temperature = 0)`.
- Always go through the cache at `data/cache/llm_responses.csv`. Cache hit means no API call. Cache miss means one API call and one new cache row.
- Store the raw response, not just the label.
- Commit the cache.

## Reports versus papers

- `output/`: AI-drafted intermediate reports. Agent writes prose; you verify numbers.
- `paper/`: researcher-authored prose. The agent does not draft prose that survives into the manuscript. It assists with code, tables, and figures only. The voice and the argument are the researcher's.

## Scope rules for any task

- Stay inside the task. Do not modify files outside the requested scope.
- Do not edit `.gitignore` or `README.md` unless the task says to.
- Do not install packages beyond `tidyverse`, `rvest`, `readr`, `ellmer`.
- Every script must run end-to-end from a fresh R session.