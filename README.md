# pmcfigurebot
Query figures indexed by PubMed Central and collect metadata

## PubMed Image Query
The GitHub Action is configured to run `scripts/fetch_figures_v2.R` on demand and
quarterly in order to query PMC for any new
figures published since the last run. The script downloads .jpg files and 
prepares a .yml file with metadata for each figure and its parent paper.

## Request Your Own Figure
Open an issue using the `Fetch this figure` providing a PMCID and a figure number. This will trigger the repo to collect this figure and add it to the queue for the next run of the PFOCR pipeline.

## Known Issues

Found while running a full catch-up cycle (2025-07 → 2026-08) in `scripts/fetch_figures_v2.R` / `scripts/LOCAL_fetch_figures_v2.R`. None of these have been fixed yet — noted here so they don't need re-discovering.

1. **`exit_flag` (1-hour job safety cutoff) doesn't actually work.** It's set inside `extract_figures()` and `process_figures()` via plain `exit_flag <- TRUE`, which only creates a local variable in each function's own scope — it never reaches the top-level `exit_flag` that the final `if (!exit_flag)` check (guarding whether `last_run` gets advanced) actually reads. In practice this means `last_run` gets advanced even when a run was truncated by the time limit, silently leaving whatever wasn't reached in that window unqueried by any future run. Only affects the GitHub Action (6-hour job cap); `LOCAL_fetch_figures_v2.R` has no such cutoff to begin with.

2. **No pagination past the 3,000-article search cap.** `search_pmc()` calls `entrez_search`/`entrez_fetch` with `retmax = 3000` and no `retstart` loop. When a date range's true match count exceeds 3,000, only the first 3,000 are ever examined — the log line `"PMC search completed. Fetched 3000 results (max set to 3000)"` is the only signal, and it looks identical whether the true count is 3,001 or 30,000. Confirmed this cap was hit on nearly every monthly window from 2025/06 through 2026/04 during catch-up.

3. **Failed downloads are never recorded in `prior_figid_results.tsv`** (that file only records successes), so a failure isn't excluded from future runs — but it's also not automatically retried, since a future run only revisits a given `PUBDATE` window if its query range happens to overlap that window again (windows normally move forward, not backward). The full history is always recoverable, though: `figure_fetch.log` is append-only and never truncated, so `grep "Failed to download" figure_fetch.log` finds every failed figid ever logged — recovering one just requires manually re-fetching it (e.g. via the "Fetch this figure" issue flow), not simply re-running the bot.

4. **Re-querying an "already done" window can surface genuinely new figures.** Confirmed by re-running `2025/06/01 → 2025/07/01` in August 2026 after it was already run in May 2026 (same window, both times hit the 3,000-article retmax cap above). Two combined causes, both verified: (a) no explicit `sort` param in `search_pmc()`, so NCBI's "first 3000" ordering isn't stable across separate query runs — a different slice of a >3000-match window can come back each time; (b) PMC's index genuinely grows for old publication dates over time (e.g. NIH's embargo policy can delay a manuscript's appearance in PMC's searchable index by up to 12 months after its nominal publish date) — so old windows aren't ever permanently "finished."

5. **Root cause of most download failures: the search source and the file source are two different, independently-synced systems.** The bot searches/fetches article metadata from NCBI PMC (via E-utilities) but downloads the actual image bytes from EuropePMC's `fulltextRepo` API — a separate database that mirrors NCBI PMC on its own lag. Verified directly: querying EuropePMC's own REST search API (`.../webservices/rest/search?query=EXT_ID:{id}%20AND%20SRC:PMC`) for 4 PMCIDs that failed with `"Impossible to retrieve file"` returned `hitCount: 0` for all 4 — the articles simply aren't in EuropePMC's index yet, regardless of filename pattern. NIHMS manuscripts correlate with this mainly because they tend to be newer/recently-embargo-lifted, not because of anything specific to that naming convention.

   **Future improvement (not implemented):** either (a) query EuropePMC's own search API directly for both discovery *and* download, so both steps read from the same, internally-consistent system instead of two independently-synced ones, or (b) keep the current NCBI-search-based approach but add a fallback to try EuropePMC first and fall back to NCBI's own CDN (`pmc.ncbi.nlm.nih.gov`) when EuropePMC doesn't have the file yet. The CDN-fallback approach was investigated and verified working (byte-identical content, confirmed via matching MD5 checksums) but deliberately not implemented, to keep the fetch logic unchanged.

6. **A failed download still leaves a bogus `.jpg` file behind.** `process_figures()` uses `httr::write_disk(filepath, overwrite=TRUE)`, which writes the HTTP response body to disk unconditionally, before the status code is even checked — so a failed attempt still creates `figures/{figid}.jpg`, just containing the JSON error body (`{"error": "Impossible to retrieve file..."}`, 94 bytes) or nothing (0 bytes, on a timeout), not a real image. A real, successful download always writes the `.jpg` and its paired `.yml` together in the same step, so any `.jpg` with no matching `.yml` is guaranteed to be one of these — safe to identify and move aside on that basis. Confirmed this isn't new: hundreds of these were already sitting in `figures/` from past catch-up runs, undiscovered until this session. Handled downstream in `pfocr-pipeline` by [`1_images/images_step_1.R`](../pfocr-pipeline/1_images/images_step_1.R), which mirrors `1_metadata/metadata_step_1.R`'s existing prune-and-stash pattern for the reverse case.

## Running a Catch-Up Cycle

Start from **one month before the last shipped release's date**, not the release date itself — e.g. last release `2025/07/01` → start `last_run` at `2025/06/01`. This gives a safety margin for figures that were found-but-failed or not yet embargo-lifted around the previous release's own cutoff (verified: the pre-`2025/07/01` release's final fetch run itself started from `2025/06/03`, one month back, and succeeded at 99.92%).

If the current release target is a fixed date (e.g. `2026/08/01`) rather than "everything through today," use an explicit bounded `date_range: ["<start>", "<release date>"]` for the last chunk instead of letting `last_run`'s auto-derivation fall into the open-ended "final stretch" mode (see Known Issue #4) — there's no need to chase whatever today's date happens to be once the release's own cutoff is fixed.

### Handing off to `pfocr-pipeline`

> **🔀 Repo switch happens here.** Everything above this line runs in `pfocr-pmcfigurebot`. Everything from this point on — copying the files, and all of Steps 1–6 — happens in the separate `pfocr-pipeline` repo. There's no automated sync between the two; the copy below *is* the hand-off.

Once fetching is done, copy `figures/*.jpg` and `figures/*.yml` out of this repo and into `pfocr-pipeline`'s `1_images/` and `1_metadata/` folders — either a local checkout (for testing) or the shared Dropbox release folder (e.g. `Ayushi Agrawal/pfocr-pipeline/{release-date}/1_images` and `1_metadata`, for an actual release). A plain shell glob (`cp figures/*.jpg ...`) will fail with "argument list too long" once there are tens of thousands of files — `rsync` doesn't have that limit:

```bash
# <PFOCR_ROOT> = the parent folder holding both repo checkouts side by side
#                (e.g. ~/Downloads/pfocr — swap in wherever you actually cloned them)
# For a real release, replace the pfocr-pipeline destination below with the
# dated Dropbox release folder instead of a local checkout.

rsync -av --include='*.jpg' --exclude='*' \
  <PFOCR_ROOT>/pfocr-pmcfigurebot/figures/ \
  <PFOCR_ROOT>/pfocr-pipeline/1_images/

rsync -av --include='*.yml' --exclude='*' \
  <PFOCR_ROOT>/pfocr-pmcfigurebot/figures/ \
  <PFOCR_ROOT>/pfocr-pipeline/1_metadata/
```

Trailing slashes on all four paths matter — dropping the source-side slash makes `rsync` test the `figures` directory *name itself* against the `--include='*.jpg'` filter (which never matches), so the whole copy silently does nothing. Verify first with `rsync -avn ...` (dry run, no changes) before dropping the `-n`.

Then, in `pfocr-pipeline`: run [`1_images/images_step_1.R`](../pfocr-pipeline/1_images/images_step_1.R) against the copied files first, to stash any bogus jpgs (Known Issue #6) out of the way, before running `1_metadata/metadata_step_1.R` to compile everything into `pfocr_figures_raw.rds`. Confirmed working end-to-end this session.

**When to clean up this repo's `figures/`:** once the copy above is verified (file counts match) and Step 1 has succeeded on the `pfocr-pipeline` side — not before, since `figures/` is otherwise the only copy of that data. Then `git add -A`/commit the deletion here, matching this repo's own git history (periodic bulk-deletion commits after past hand-offs), to keep it from growing unboundedly. **Never delete `prior_figid_results.tsv`** — the permanent dedupe ledger, must survive indefinitely across every release.
