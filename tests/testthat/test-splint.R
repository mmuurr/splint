test_that("splint_simple", {

  splint <- splint_simple()
  expect_identical(splint(), NULL)
  purrr::walk(
    list(NULL, c(TRUE, FALSE), 1L:3L, 1:3, LETTERS, list(), list(foo = "bar"), mtcars),
    \(x) expect_identical(splint(x), x)
  )

  splint <- splint_simple(as.logical, missing = na_chr)
  checkmate::expect_function(splint)
  expect_identical(splint %@% missing, na_chr)
  expect_error(splint())
  expect_identical(splint(NULL), logical(0))
  expect_identical(splint(c(TRUE, FALSE)), c(TRUE, FALSE))
  expect_identical(splint(-1:1), c(TRUE, FALSE, TRUE))
  expect_identical(splint(c("TRUE", "FALSE", "true", "false")), c(TRUE, FALSE, TRUE, FALSE))

  splint <- splint_simple(as.integer, missing = na_int)
  checkmate::expect_function(splint)
  expect_identical(splint %@% missing, na_int)
  expect_error(splint())
  expect_identical(splint(NULL), integer(0))
  expect_identical(splint(c(FALSE, TRUE)), 0L:1L)
  expect_identical(splint(-1L:1L), -1L:1L)
  expect_identical(splint(1:3 + 0.1), 1L:3L)
  expect_warning(
    expect_identical(splint(c("1", "foo", "1.1", "1.9", "2")), as.integer(c(1, NA, 1, 1, 2)))
  )

  splint <- splint_simple(as.double, missing = na_dbl)
  checkmate::expect_function(splint)
  expect_identical(splint %@% missing, na_dbl)
  expect_error(splint())
  expect_identical(splint(NULL), double(0))
  expect_identical(splint(c(FALSE, TRUE)), as.double(0:1))
  expect_identical(splint(-1L:1L), as.double(-1:1))
  expect_identical(splint(1:3 + 0.1), c(1.1, 2.1, 3.1))
  expect_warning(
    expect_identical(splint(c("1", "foo", "1.1", "1.9", "2")), as.double(c(1, NA, 1.1, 1.9, 2)))
  )
  
  splint <- splint_simple(as.character, missing = "<MISSING>")
  checkmate::expect_function(splint)
  expect_identical(splint %@% missing, "<MISSING>")
  expect_error(splint())
  expect_identical(splint(NULL), character(0))
  expect_identical(splint(c(TRUE, FALSE)), c("TRUE", "FALSE"))
  expect_identical(splint(1L:3L), c("1","2","3"))
  expect_identical(splint(1:3 + 0.5), c("1.5", "2.5", "3.5"))
  
  expect_error(splint_simple("foo"))
})


test_that("splint_dict", {

  expect_error(
    splint_dict(list(
      foo = 1:3
    ))
  )

  expect_error(
    splint_dict(list(
      foo = as.integer
    ))
  )

  splint <-
    splint_dict(list(
      foo = splint_simple(as.integer),
      bar = splint_simple(as.character)
    ))
  expect_identical(
    splint(NULL),
    list(foo = integer(0), bar = character(0))
  )
  expect_identical(
    splint(list(foo = 1:10 + 0.5)),
    list(foo = 1:10, bar = character(0))
  )
  expect_identical(
    splint(list(baz = "qux")),
    list(foo = integer(0), bar = character(0))
  )
  expect_identical(
    splint(list(bar = 1:3, baz = "qux")),
    list(foo = integer(0), bar = as.character(1:3))
  )

  splint <- splint_dict(
    list(
      foo = splint_simple(as.integer),
      bar = splint_simple(as.character)
    ),
    keep_all = TRUE
  )
  expect_identical(
    splint(list(bar = 1:3, baz = "qux")),
    list(foo = integer(0), bar = as.character(1:3), baz = "qux")
  )

  ## non-recursive: bar is simply a list.
  splint <-
    splint_dict(list(
      foo = splint_simple(as.integer),
      bar = splint_simple(as.list)
    ))
  expect_identical(
    splint(NULL),
    list(foo = integer(0), bar = list())
  )
  expect_identical(
    splint(list()),
    list(foo = integer(0), bar = list())
  )
  expect_identical(
    splint(list(foo = 1:3 + 0.1, bar = list(baz = "qux"))),
    list(foo = 1:3, bar = list(baz = "qux"))
  )

  ## recursive
  splint <-
    splint_dict(list(
      foo = splint_simple(as.integer),
      bar = splint_dict(list(
        baz = splint_simple(as.character)
      ))
    ))
  expect_identical(
    splint(NULL),
    list(foo = integer(0), bar = list(baz = character(0)))
  )
  expect_identical(
    splint(list()),
    list(foo = integer(0), bar = list(baz = character(0)))
  )
  expect_error(
    splint(list(bar = 1:3))
  )
  expect_identical(
    splint(list(bar = list())),
    list(foo = integer(0), bar = list(baz = character(0)))
  )
  expect_identical(
    splint(list(bar = list(baz = 1:3))),
    list(foo = integer(0), bar = list(baz = as.character(1:3)))
  )
})


test_that("splint_tbl", {

  expect_error(
    splint_tbl(list(foo = 1:3))
  )

  expect_error(
    splint_tbl(list(foo = as.integer))
  )

  splint <-
    splint_tbl(list(
      foo = splint_simple(as.integer),
      bar = splint_simple(as.character)
    ))
  expect_identical(
    splint(),
    tibble::tibble(foo = integer(0), bar = character(0))
  )
  expect_identical(
    splint(NULL),
    tibble::tibble(foo = integer(0), bar = character(0))
  )
  expect_identical(
    splint(list()),
    tibble::tibble(foo = integer(0), bar = character(0))
  )
  expect_identical(
    splint_get_ptype_tbl(splint),
    tibble::tibble(foo = integer(0), bar = character(0))
  )
  expect_identical(
    splint(tibble::tibble()),
    splint_get_ptype_tbl(splint)
  )
  expect_identical(
    splint(data.frame()),
    splint_get_ptype_tbl(splint)
  )
  expect_identical(
    splint(iris),
    tibble::tibble(
      foo = rep(na_int, nrow(iris)),
      bar = rep(na_chr, nrow(iris))
    )
  )
  expect_identical(
    splint(list(foo = 1:3 + 0.1)),
    tibble::tibble(
      foo = as.integer(1:3),
      bar = rep(na_chr, 3)
    )
  )  

  splint2 <- splint_tbl(splint_get_splints(splint), keep_all = TRUE)
  expect_identical(
    splint2(iris),
    tibble::tibble(
      foo = rep(na_int, nrow(iris)),
      bar = rep(na_chr, nrow(iris))
    ) |> dplyr::bind_cols(iris)
  )
})


test_that("splint_map", {

  expect_error(splint_map(NULL))

  expect_identical(splint_map()(NULL), list())
  expect_identical(splint_map()(list()), list())

  dict_splint <- splint_dict(list(
    foo = splint_simple(as.integer),
    bar = splint_simple(as.character)
  ))
  splint <- splint_map(dict_splint)
  expect_error(splint())
  expect_identical(
    splint(list(NULL, NULL)),
    list(
      list(foo = integer(0), bar = character(0)),
      list(foo = integer(0), bar = character(0))
    )
  )
  expect_identical(
    splint(list(list(), list())),
    list(
      list(foo = integer(0), bar = character(0)),
      list(foo = integer(0), bar = character(0))
    )
  )
  expect_identical(
    splint(list(list(), NULL)),
    list(
      list(foo = integer(0), bar = character(0)),
      list(foo = integer(0), bar = character(0))
    )
  )

  ## non-recursive with tbl:
  splint <- splint_tbl(list(
    foo = splint_simple(as.list),
    bar = splint_simple(as.list)
  ))
  expect_identical(
    splint(tibble::tibble(
      foo = list(NULL, NULL, NULL),
      baz = 1:3
    )),
    tibble::tibble(
      foo = list(NULL, NULL, NULL),
      bar = list(NULL, NULL, NULL)
    )
  )

  ## err if trying to use a splint_dict as a col directly:
  expect_error(splint_tbl(list(foo = dict_splint)))
  
  ## recurse by either explicitly splinting with map or use the splint_map wrapper:
  splint <- splint_tbl(list(
    foo = splint_map(dict_splint),
    bar = splint_simple(\(x) purrr::map(x, dict_splint))
  ))
  expect_identical(
    splint(NULL),
    tibble::tibble(
      foo = list(),
      bar = list()
    )
  )
  expect_identical(
    splint(tibble::tibble(baz = 1:3)),
    tibble::tibble(
      foo = rep(list(list(foo = integer(0), bar = character(0))), 3),
      bar = rep(list(list(foo = integer(0), bar = character(0))), 3)
    )
  )

})


test_that("nested answer_log", {
  as_datetime <- function(x) lubridate::as_datetime(x, tz = "UTC")
  
  answer_log_splint <- splint_tbl(list(
    question_ix   = splint_simple(as.integer),
    player_id     = splint_simple(as.character),
    submission_id = splint_simple(as.character),
    grade         = splint_map(splint_dict(list(
      grader_type                   = splint_simple(as.character, "<MISSING>"),
      requested_player_answer_count = splint_simple(as.integer, 1L),
      submitted_player_answer_count = splint_simple(as.integer, 0L),
      correct_match_count           = splint_simple(as.integer, 0L),
      correct_match_weightsum       = splint_simple(as.double, 0),
      correct_match_ix_tbl          = splint_tbl(list(
        player_answer_ix   = splint_simple(as.integer),
        question_answer_ix = splint_simple(as.integer),
        weight             = splint_simple(as.double)
      )),
      normalized_player_answer_tbl  = splint_tbl(list(
        player_answer_ix = splint_simple(as.integer),
        raw              = splint_simple(as.character),
        word             = splint_simple(as.character),
        phrase           = splint_simple(as.character)
      ))
    ))),
    `_seqid`      = splint_simple(as.integer),
    `_created_at` = splint_simple(as_datetime),
    `_updated_at` = splint_simple(as_datetime)
  ))

  x <- answer_log_splint(tibble::tibble(`_seqid` = 1:3))
  str(x$grade)
})


test_that("named answer_log", {
  as_datetime <- function(x) lubridate::as_datetime(x, tz = "UTC")

  splints <- tibble::lst(
    correct_match_ix_tbl = splint_tbl(list(
      player_answer_ix     = splint_simple(as.integer),
      question_answer_ix   = splint_simple(as.integer),
      weight               = splint_simple(as.double)
    )),
    normalized_player_answer_tbl = splint_tbl(list(
      player_answer_ix             = splint_simple(as.integer),
      raw                          = splint_simple(as.character),
      word                         = splint_simple(as.character),
      phrase                       = splint_simple(as.character)
    )),
    grade = splint_dict(list(
      grade_type =                    splint_simple(as.character, "<MISSING>"),
      requested_player_answer_count = splint_simple(as.integer, 1L),
      submitted_player_answer_count = splint_simple(as.integer, 0L),
      correct_match_count           = splint_simple(as.integer, 0L),
      correct_match_weightsum       = splint_simple(as.double, 0L),
      correct_match_ix_tbl          = correct_match_ix_tbl,
      normalized_player_answer_tbl  = normalized_player_answer_tbl
    )),
    answer_log = splint_tbl(list(
      question_ix   = splint_simple(as.integer),
      player_id     = splint_simple(as.character),
      submission_id = splint_simple(as.character),
      grade         = splint_map(grade),
      grade2        = splint_simple(\(x) purrr::map(x, grade)),
      `_seqid`      = splint_simple(as.integer),
      `_created_at` = splint_simple(as_datetime),
      `_updated_at` = splint_simple(as_datetime)
    ))
  )
})
