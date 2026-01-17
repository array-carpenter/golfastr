# golfastr 0.1.0

## Initial Release

### Main Features

* `load_pga_pbp()` - Main function for loading PGA Tour data (similar to nflfastR::load_pbp())
* `load_pga_leaderboards()` - Fast leaderboard loading
* `load_pga_holes()` - Detailed hole-by-hole scoring
* `load_pga_schedule()` - Tournament schedule data

### Data Features

* Automatic caching for fast repeat access
* Auto-save to CSV for easy sharing
* Multiple season support
* Tournament filtering

### Utilities

* `cache_info()` - View cache status
* `clear_cache()` - Clear cached data
* `pga_field_descriptions()` - Field documentation
* `pga_score_types()` - Score type reference
* `pga_majors()` - Major championship info

### Data Sources

* ESPN Golf API
* 2025 PGA Tour season coverage
* Historical data support

### Performance

* Local caching (instant repeat loads)
* Hosted data downloads (1-2 seconds)
* ESPN API fallback (30-60 seconds)

### Testing

* testthat test suite
* Manual verification CSVs
* Continuous integration ready
