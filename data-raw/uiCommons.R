## code to prepare `uiCommons` dataset goes here

uiCommons <- list(
  emojis = list(
    error = "\u274c",
    warning = "\u26A0\uFE0F",
    check = "\u2705"
  ),
  colors = list(
    sexMale = "#2c5e77",
    sexFemale = "#BF616A",
    sexNa = "#8C8C8C",
    timeHist = "#00BFFF"
  )
)

usethis::use_data(uiCommons, overwrite = TRUE)
