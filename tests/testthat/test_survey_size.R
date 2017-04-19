library(surveytools)

context('survey_size')

test_that("sample_size returns a 5 x 2 dataframe", {
    expect_equal(class(survey_size(population_size = 100)), 'data.frame')
    expect_equal(survey_size(population_size = 20000)$Value[[1]], 377)
    expect_equal(nrow(survey_size(population_size = 100)), 5)
})
