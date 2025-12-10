# CEDAR Test Suite

This directory contains unit and integration tests for the CEDAR application.

## Test Structure

```
tests/
├── testthat.R              # Test runner entry point
├── testthat/
│   ├── setup.R             # Test fixtures and helper functions
│   ├── test-filtering.R    # Data filtering tests
│   ├── test-enrollment.R   # Enrollment analysis tests
│   ├── test-seatfinder.R   # Seatfinder-specific tests
│   ├── test-headcount.R    # Headcount analysis tests
│   ├── test-grades.R       # Grade analysis tests
│   ├── test-rollcall.R     # Student demographic tests
│   ├── test-regstats.R     # Registration statistics tests
│   ├── test-forecast.R     # Forecasting tests
│   ├── test-data-loading.R # Data loading tests
│   ├── test-utils.R        # Utility function tests
│   └── test-integration.R  # End-to-end workflow tests
└── README.md               # This file
```

## Running Tests

### Run all tests
```r
# From R console
devtools::test()

# Or using testthat directly
testthat::test_dir("tests/testthat")
```

### Run specific test file
```r
testthat::test_file("tests/testthat/test-seatfinder.R")
```

### Run tests from command line
```bash
Rscript -e "devtools::test()"
```

## Test Coverage

### Current Status
Most tests are currently **skipped** with placeholder implementations. This provides a comprehensive testing framework that needs to be filled in as functions are verified.

### Coverage Areas

#### Core Data Operations (High Priority)
- ✅ **Filtering**: Department, term, campus, PT, IM, level filters
- ✅ **Enrollment Analysis**: get_enrl(), aggregation, calculations
- ✅ **Seatfinder**: Term comparisons, course differences, gen ed filtering
- ✅ **Headcount**: Major/minor counting, department aggregation
- ✅ **Grades**: DFW calculations, instructor aggregation

#### Reporting Functions (Medium Priority)
- ✅ **Rollcall**: Student demographics by course
- ✅ **Regstats**: Enrollment anomaly detection, threshold application
- ✅ **Forecast**: Enrollment predictions, accuracy methods

#### Infrastructure (Lower Priority)
- ✅ **Data Loading**: Global data objects, file structure validation
- ✅ **Utilities**: Term parsing, mappings, crosslist handling
- ✅ **Integration**: Complete workflow execution

## Test Data

Test fixtures are created in `setup.R` with minimal but realistic data:
- `test_students`: 5 students across multiple terms/courses
- `test_courses`: 5 course sections with enrollment data
- `test_academic_studies`: Academic program enrollments
- `test_degrees`: Sample degree completion data

### Creating Additional Test Data

```r
# In your test file
custom_courses <- test_courses %>%
  mutate(enrolled = c(50, 60, 70, 80, 90))
```

## Implementing Tests

### Priority Order for Implementation

1. **Start with core filtering** (`test-filtering.R`)
   - These are foundational to all other functions
   - Relatively straightforward to verify

2. **Move to enrollment** (`test-enrollment.R`)
   - Verify get_enrl() aggregation logic
   - Test grouping and calculation accuracy

3. **Implement seatfinder tests** (`test-seatfinder.R`)
   - Complex but well-documented
   - Recently refactored, good opportunity to verify correctness

4. **Add grade/headcount tests** (`test-grades.R`, `test-headcount.R`)
   - Medium complexity
   - Important for data accuracy

5. **Complete integration tests** (`test-integration.R`)
   - Verify end-to-end workflows
   - Catch interaction bugs

### Un-skipping Tests

To activate a test, replace:
```r
skip("Function needs verification")
```

With the actual test implementation:
```r
result <- function_to_test(test_data, opt)
expect_equal(result$value, expected_value)
```

### Example: Implementing a Real Test

```r
test_that("get_enrl calculates availability correctly", {
  opt <- create_test_opt()
  
  result <- get_enrl(test_courses, opt)
  
  # Verify availability = capacity - enrolled
  expect_equal(
    result$avail,
    result$cap - result$enrolled
  )
  
  # Verify no negative availability
  expect_true(all(result$avail >= 0))
})
```

## Best Practices

### Test Independence
- Each test should be completely independent
- Use test fixtures from `setup.R`
- Don't rely on previous test state

### Descriptive Names
```r
# Good
test_that("get_enrl filters by department and returns only matching courses", {})

# Bad
test_that("filtering works", {})
```

### Arrange-Act-Assert Pattern
```r
test_that("function does something", {
  # Arrange: Set up test data
  opt <- create_test_opt(list(dept = "HIST"))
  
  # Act: Call the function
  result <- my_function(test_data, opt)
  
  # Assert: Verify results
  expect_equal(result$dept, "HIST")
})
```

### Test Edge Cases
- Empty data frames
- NULL parameters
- Missing columns
- Invalid input values
- Extreme values (very large/small numbers)

## Continuous Integration

### GitHub Actions (Future)
Add `.github/workflows/test.yml`:
```yaml
name: R Tests
on: [push, pull_request]
jobs:
  test:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v2
      - uses: r-lib/actions/setup-r@v2
      - name: Install dependencies
        run: Rscript -e "install.packages(c('devtools', 'testthat'))"
      - name: Run tests
        run: Rscript -e "devtools::test()"
```

## Test Maintenance

### When Adding New Functions
1. Create test file if needed
2. Add test cases for main functionality
3. Add edge case tests
4. Update this README

### When Modifying Existing Functions
1. Update relevant tests
2. Add regression tests for bug fixes
3. Ensure tests still pass

### Test Review Checklist
- [ ] All required columns tested
- [ ] Edge cases covered
- [ ] Error handling tested
- [ ] Performance acceptable (< 1 second per test)
- [ ] Documentation updated

## Resources

- [testthat documentation](https://testthat.r-lib.org/)
- [R Packages - Testing](https://r-pkgs.org/testing-basics.html)
- [Writing Good Tests](https://testthat.r-lib.org/articles/custom-expectation.html)

## Notes

**Important**: Most tests are currently skipped because function signatures and behaviors need verification against actual CEDAR codebase. Un-skip and implement tests progressively as you work through each module.

The test framework is comprehensive and ready to use - it just needs the actual assertions filled in based on how your functions actually work.
