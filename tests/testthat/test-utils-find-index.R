data <- data.frame(
  day = c(1,1,1,2,2,2),
  beep = c(1,2,3,1,2,4),
  mood = c(1,2,3,1,2,3),
  mood2 = c(1,2,3,1,2,3)
)

dayvar = "day"
beepvar = "beep"

var = c("mood", "mood2")

test_that("correct indexes are found by `find_index()`", {
  expect_identical(
    find_index(data, dayvar, beepvar),
    list(c(1L,2L,4L), c(2L,3L,5L))
  )
})
