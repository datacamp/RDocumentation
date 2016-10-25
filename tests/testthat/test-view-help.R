context("view_help")

test_that("build_local_url works", {
  
  old_port <- Sys.getenv("RSTUDIO_SESSION_PORT")
  old_secret <- Sys.getenv("RS_SHARED_SECRET")
  
  Sys.setenv(RSTUDIO_SESSION_PORT = "", RS_SHARED_SECRET = "")
  dir.create(dirname(cred_path), showWarnings = FALSE)
  write("", file = cred_path)
  expect_equal(build_local_url(123),
               "http://127.0.0.1:123/library/RDocumentation/doc/index.html?viewer_pane=1")
  
  Sys.setenv(RSTUDIO_SESSION_PORT = "123", RS_SHARED_SECRET = "secret")
  dir.create(dirname(cred_path), showWarnings = FALSE)
  write("", file = cred_path)
  expect_equal(build_local_url(123),
               paste0("http://127.0.0.1:123/library/RDocumentation/doc/index.html?",
                      "viewer_pane=1&Rstudio_port=123&RS_SHARED_SECRET=secret"))
  
  # Sys.setenv(RSTUDIO_SESSION_PORT = "123",
  #            RS_SHARED_SECRET = "secret")
  # write("sid=456", file = cred_path)
  # expect_equal(build_local_url(123),
  #              paste0("http://127.0.0.1:123/library/RDocumentation/doc/index.html?",
  #                     "viewer_pane=1&Rstudio_port=123&RS_SHARED_SECRET=secret&sid=456"))
  
  Sys.setenv(RSTUDIO_SESSION_PORT = old_port, RS_SHARED_SECRET = old_secret)
  file.remove(cred_path)
})