library(httptest)
library(OpenSourceAP.DownloadR)


### list_port()
capture_requests({
  obj <- OpenAP$new()
  data <- obj$list_port()
})


### dl_port()
capture_requests({
  obj <- OpenAP$new()
  data <- obj$dl_port("op")
})

capture_requests({
  obj <- OpenAP$new()
  data <- obj$dl_port("deciles_vw", "AM")
})

capture_requests({
  obj <- OpenAP$new()
  data <- obj$dl_port("deciles_vw", c("AM", "Mom12m"))
})


## dl_signal()

capture_requests({
  obj <- OpenAP$new()
  obj$dl_signal("BM")
})

capture_requests({
  obj <- OpenAP$new()
  obj$dl_signal(c("BM", "Accruals"))
})


capture_requests({
  obj <- OpenAP$new()
  obj$dl_signal("Size")
})


capture_requests({
  obj <- OpenAP$new()
  obj$dl_signal_doc()
})


capture_requests({
  obj <- OpenAP$new()
  obj$dl_all_signals()
})
