context("utility functions")

ss <- register_ss(ws_feed = pts_ws_feed)

test_that("We can obtain worksheet info from a registered spreadsheet", {

  ## retrieve by worksheet title
  africa <- get_ws(ss, "Africa")
  expect_equal(africa$ws_title, "Africa")
  expect_equal(africa$row_extent, 1000L)
  
  ## retrieve by positive integer
  europe <- get_ws(ss, 4)
  expect_equal(europe$ws_title, "Europe")
  expect_equal(africa$col_extent, 26L)
  
  ## doubles get truncated, i.e. 1.3 --> 1
  asia <- get_ws(ss, 1.3)
  expect_equal(asia$ws_title, "Asia")

})

test_that("We throw error for bad worksheet request", {

  expect_error(get_ws(ss, -3))
  expect_error(get_ws(ss, factor(1)))
  expect_error(get_ws(ss, LETTERS))
  
  expect_error(get_ws(ss, "Mars"), "not found")
  expect_error(get_ws(ss, 100L), "only contains")
    
})

test_that("We can get list of worksheets in a spreadsheet", {
  ws_listing <- ss %>% list_ws()
  expect_true(all(c('Asia', 'Africa', 'Americas', 'Europe', 'Oceania') %in%
                    ws_listing))
})

test_that("We can a extract a key from a URL", {
  
  # new style URL
  expect_equal(extract_key_from_url(pts_url), pts_key)
  
  # old style URL
  #expect_equal(extract_key_from_url(old_url), old_key)
  # 2015-02-27 Anecdotally it appears you cannot extract current keys for use
  # with the API from old style Sheets URLs ... must identify via title, I
  # guess?
  
  # worksheets feed
  expect_equal(extract_key_from_url(pts_ws_feed), pts_key)
  
})
