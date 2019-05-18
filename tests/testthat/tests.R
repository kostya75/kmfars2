# Test if a file name is created according to the template
expect_that(make_filename(2014),is_identical_to("accident_2014.csv.bz2"))
