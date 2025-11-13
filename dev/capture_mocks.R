library(httptest)
library(OpenSourceAP.DownloadR)

capture_requests({
  obj <- OpenAP$new()
  data <- obj$dl_port("op")
})



capture_requests({
  obj <- OpenAP$new()
  obj$dl_signal(c("BM", "Accruals"))
})



