## Resubmission / update

This is version 0.2.0. Data are now downloaded from a public Hugging Face dataset instead of
Google Drive, and older data releases are supported. See NEWS.md.

## R CMD check results

0 errors | 0 warnings | 0 notes

Tests that download data are skipped on CRAN (`skip_on_cran()`). Examples that need an
internet connection or a WRDS account are in `\dontrun{}`; offline examples use mock data.

## Acronyms
- CRSP: Center for Research in Security Prices
- WRDS: Wharton Research Data Services
