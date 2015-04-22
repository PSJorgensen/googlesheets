context("download sheets")

test_that("Spreadsheet can be exported", {

  temp_dir <- tempdir()

  # bad format
  expect_error(download_ss(pts_title, to = "pts.txt"),
               "Cannot download Google spreadsheet as this format")

  # good formats
  expect_message(download_ss(pts_title, to = file.path(temp_dir, "pts.xlsx")),
                 "successfully downloaded")
  expect_message(download_ss(pts_title, to = file.path(temp_dir, "pts.pdf")),
                 "successfully downloaded")
  expect_message(download_ss(pts_title, to = file.path(temp_dir, "pts.csv")),
                 "successfully downloaded")

  expect_true(all(file.exists(file.path(temp_dir,
                                        c("pts.xlsx", "pts.pdf", "pts.csv")))))

  expect_true(all(file.remove(file.path(temp_dir,
                                        c("pts.xlsx", "pts.pdf", "pts.csv")))))

})

test_that("Old Sheets can be exported", {

  temp_dir <- tempdir()
  ss <- register_ss(old_title)

  # csv should not work
  ## BUT IT DOES NOW?
  #expect_error(download_ss(old_title, to = file.path(temp_dir, "old.csv")),
  #             "not supported")

  # good formats and different specifications
  expect_message(ss %>% download_ss(to = file.path(temp_dir, "old.xlsx"),
                                    overwrite = TRUE),
                 "successfully downloaded")
  expect_message(download_ss(old_title, to = file.path(temp_dir, "old.xlsx"),
                                                       overwrite = TRUE),
                 "successfully downloaded")
  expect_message(download_ss(old_title, to = file.path(temp_dir, "old.pdf"),
                             overwrite = TRUE),
                 "successfully downloaded")
  ## TO DO: download_ss(old_url, to = file.path(temp_dir, "old.pdf"), overwrite = TRUE) was not working: " Error in identify_ss(.) : Identifying info "0Audw-qi1jh3fdHhQWWg0T2tZdkZqT2VMeDZnOHM4NUE" doesn't match title, key, or worksheets feed for any sheet listed in the Google sheets home screen for authorized user. "

  expect_true(all(file.exists(file.path(temp_dir, c("old.xlsx", "old.pdf")))))

  expect_true(all(file.remove(file.path(temp_dir, c("old.xlsx", "old.pdf")))))

})
