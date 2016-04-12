library(surveytools)
context('survey size')

test_that("sample_size returns a list of 6 numeric items", {
    expect_equal(class(survey_size(population_size = 100)), 'ssize')
    expect_equal(survey_size(population_size = 20000)[[2]], 377)
    expect_equal(length(survey_size(population_size = 100)), 6)
})
