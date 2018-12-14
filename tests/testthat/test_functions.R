library(testthat)
context("Test functions")

filename <- system.file("extdata/signif.txt", package = "RCapstone")
raw_data <- readr::read_delim(filename, delim = "\t")

test_that("eq_clean_data returns a 'data.frame' object", {
        expect_is(eq_clean_data(raw_data), "data.frame")
})

test_that("eq_clean_data$DATE is of 'Date' type", {
        expect_is(eq_clean_data(raw_data)$DATE, "Date")
})

test_that("eq_clean_data returns numeric coordinates", {
        expect_is(eq_clean_data(raw_data)$LATITUDE, "numeric")
        expect_is(eq_clean_data(raw_data)$LONGITUDE, "numeric")
})

test_that("eq_location_clean returns a 'data.frame' object", {
        expect_is(eq_location_clean(raw_data), "data.frame")
})

test_that("geom_timeline returns a 'ggplot' object", {
        g <- raw_data %>% eq_clean_data() %>%
                dplyr::filter(COUNTRY %in% c("USA", "CHINA"), YEAR >= 2000) %>%
                ggplot2::ggplot(ggplot2::aes(x = DATE,
                                             y = COUNTRY,
                                             color = as.numeric(TOTAL_DEATHS),
                                             size = as.numeric(EQ_PRIMARY)
                )) +
                geom_timeline()
        expect_is(g, "ggplot")
})

test_that("geom_timeline_label returns a 'ggplot' object", {
        g <- raw_data %>% eq_clean_data() %>%
                dplyr::filter(COUNTRY %in% c("USA", "CHINA"), YEAR >= 2000) %>%
                ggplot2::ggplot(ggplot2::aes(x = DATE,
                                             y = COUNTRY,
                                             color = as.numeric(TOTAL_DEATHS),
                                             size = as.numeric(EQ_PRIMARY)
                )) +
                geom_timeline_label(ggplot2::aes(label = LOCATION_NAME))
        expect_is(g, "ggplot")
})

test_that("theme_timeline returns a 'ggplot' object", {
        g <- raw_data %>% eq_clean_data() %>%
                dplyr::filter(COUNTRY %in% c("USA", "CHINA"), YEAR >= 2000) %>%
                ggplot2::ggplot(ggplot2::aes(x = DATE,
                                             y = COUNTRY,
                                             color = as.numeric(TOTAL_DEATHS),
                                             size = as.numeric(EQ_PRIMARY)
                )) +
                theme_timeline()
        expect_is(g, "ggplot")
})

test_that("eq_map returns a 'leaflet' object", {
        l <- raw_data %>%
                eq_clean_data() %>%
                dplyr::filter(COUNTRY == "MEXICO" & lubridate::year(DATE) >= 2000) %>%
                dplyr::mutate(popup_text = eq_create_label(.)) %>%
                eq_map(annot_col = "popup_text")
        expect_is(l, "leaflet")
})

test_that("eq_create_label returns a 'character' vector", {
        expect_is(eq_create_label(raw_data), "character")
})

test_that("eq_create_label has the same length as input data frame", {
        expect_equal(length(eq_create_label(raw_data)), dim(raw_data)[1])
})
