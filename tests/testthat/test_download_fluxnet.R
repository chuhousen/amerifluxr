context("test amerifluxr download data functions")

test_that("check donwload data function", {
  skip_on_cran()

  ## get secret log-in user name and email
  #   Use Git repository secrets
  user <- system("echo $MY_USER", intern = TRUE)
  email <- system("echo $MY_EMAIL", intern = TRUE)
  #   Use local env when testing locally
  if (user == "" | user == "$MY_USER") {
    user <- Sys.getenv("MY_USER")
    email <- Sys.getenv("MY_EMAIL")
  }

  #### tests without valid user & email
  ## test error when invalid input in policy agreement
  expect_error(
    amf_download_fluxnet(
      user_id = "test_not_work",
      user_email = "test_not_work$mail.com",
      site_id = "US-CRT",
      data_product = "FLUXNET",
      data_variant = "SUBSET",
      data_policy = "CCBY4.0",
      intended_use = "other",
      intended_use_text = "testing download",
      out_dir = tempdir(),
      verbose = FALSE
    )
  )

  #### tests without valid user & email
  ## test error and warning return
  # test invalid user id and email
  expect_error(
    amf_download_fluxnet(
      user_id = 1234,
      user_email = "test_not_work$mail.com",
      site_id = "US-CRT",
      data_product = "FLUXNET",
      data_variant = "SUBSET",
      data_policy = "CCBY4.0",
      agree_policy = TRUE,
      intended_use = "other",
      intended_use_text = "testing download",
      out_dir = tempdir(),
      verbose = FALSE
    )
  )

  expect_error(
    amf_download_fluxnet(
      user_id = "test",
      user_email = "test_not_work$mail.com",
      site_id = "US-CRT",
      data_product = "FLUXNET",
      data_variant = "SUBSET",
      data_policy = "CCBY4.0",
      agree_policy = TRUE,
      intended_use = "other",
      intended_use_text = "testing download",
      out_dir = tempdir(),
      verbose = FALSE
    )
  )


  # test invalid data policy
  expect_error(
    amf_download_fluxnet(
      user_id = "test",
      user_email = "test@mail.com",
      site_id = "US-CRT",
      data_product = "FLUXNET",
      data_variant = "SUBSET",
      data_policy = "test_not_working",
      agree_policy = TRUE,
      intended_use = "other",
      intended_use_text = "testing download",
      out_dir = tempdir(),
      verbose = FALSE
    )
  )


  # test invalid intended_use
  expect_error(
    amf_download_fluxnet(
      user_id = "test",
      user_email = "test@mail.com",
      site_id = "US-CRT",
      data_product = "FLUXNET",
      data_variant = "SUBSET",
      data_policy = "CCBY4.0",
      agree_policy = TRUE,
      intended_use = "test_not_working",
      intended_use_text = "testing download",
      out_dir = tempdir(),
      verbose = FALSE
    )
  )


  ## test all invalid site ID
  expect_error(
    amf_download_fluxnet(
      user_id = "test",
      user_email = "test@mail.com",
      site_id = "US-Crt",
      data_product = "FLUXNET",
      data_variant = "SUBSET",
      data_policy = "CCBY4.0",
      agree_policy = TRUE,
      intended_use = "other",
      intended_use_text = "testing download",
      out_dir = tempdir(),
      verbose = FALSE
    )
  )


  ## tests with valid user & email
  if (user != "" & user != "$MY_USER") {
    expect_output(amf_download_fluxnet(
      user_id = user,
      user_email = email,
      site_id = "US-CRT",
      data_product = "FLUXNET",
      data_variant = "SUBSET",
      data_policy = "CCBY4.0",
      agree_policy = TRUE,
      intended_use = "other",
      intended_use_text = "testing download",
      out_dir = tempdir(),
      verbose = TRUE
    ))

    ## test warning return when some invalid site ID
    expect_warning(
      amf_download_fluxnet(
        user_id = user,
        user_email = email,
        site_id = c("US-CRT", "US-Crt", "UScrt"),
        data_product = "FLUXNET",
        data_variant = "SUBSET",
        data_policy = "CCBY4.0",
        agree_policy = TRUE,
        intended_use = "other",
        intended_use_text = "testing download",
        out_dir = tempdir(),
        verbose = FALSE
      )
    )

    # test file return when some invalid site ID
    base_out2 <- suppressWarnings((
      amf_download_fluxnet(
        user_id = user,
        user_email = email,
        site_id = c("US-CRT", "US-Crt", "UScrt"),
        data_product = "FLUXNET",
        data_variant = "SUBSET",
        data_policy = "CCBY4.0",
        agree_policy = TRUE,
        intended_use = "other",
        intended_use_text = "testing download",
        out_dir = tempdir(),
        verbose = FALSE
      )
    ))
    expect_equal(length(base_out2), 1)

    rm(user)
    rm(email)
  }
})
