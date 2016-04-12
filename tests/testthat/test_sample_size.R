library(surveytools)
context('survey size')

test_that("sample_size returns an integer", {
    expect_equal(dim(survey_size()), c(5,2))
    expect_equal(survey_size()[5,2], c('377'))
    expect_equal(dim(survey_size(error_margin = 0.1, conf_level = 0.99, population_size = 450)), c(5,2))
    expect_equal(survey_size(error_margin = 0.1, conf_level = 0.99, population_size = 450)[5,2], c('121'))
})
