library(testthat)
library(HurricaneWindGeom)

test_that("object classes are correct",{
    expect_is(get_stormdata("ebtrk_atlc_1988_2015.txt"),c("tbl_df","tbl","data.frame"))
    expect_is(GeomHurricane,c("GeomHurricane","Geom","ggproto"))
    expect_is(geocode_radii(c(1,1)),"data.frame")
})
