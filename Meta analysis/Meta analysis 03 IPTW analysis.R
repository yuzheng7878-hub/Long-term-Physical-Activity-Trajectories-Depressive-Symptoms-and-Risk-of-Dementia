library(readxl)
library(meta)

settings.meta(CIseparator = ", ")

input_file <- "meta_data_iptw.xlsx"
output_dir <- "figures/iptw_analysis"
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

trajectory_map <- data.frame(
  sheet = c(2, 3, 4, 5),
  trajectory = c("Increasingly Active", "Intermittently Active", "Decreasingly Active", "Consistently Inactive"),
  output_file = c("increasing_active.jpg", "intermittently_active.jpg", "decreasing_active.jpg", "consistently_inactive.jpg"),
  stringsAsFactors = FALSE
)

format_count <- function(x) {
  x_num <- as.numeric(gsub(",", "", as.character(x)))
  formatC(x_num, format = "d", big.mark = ",")
}

prepare_meta_data <- function(data) {
  data <- data[1:3, ]
  count_cols <- c("DemN", "N", "DemN (Consistently Active)", "Total (Consistently Active)")
  data[count_cols] <- lapply(data[count_cols], format_count)
  data$HR <- as.numeric(as.character(data$HR))
  data$CI_lower <- as.numeric(as.character(data$CI_lower))
  data$CI_upper <- as.numeric(as.character(data$CI_upper))
  data$logHR <- log(data$HR)
  data$SE <- (log(data$CI_upper) - log(data$CI_lower)) / (2 * qnorm(0.975))
  data
}

run_meta_analysis <- function(data, output_file) {
  meta_result <- metagen(
    TE = logHR,
    seTE = SE,
    studlab = Study,
    data = data,
    common = TRUE,
    random = TRUE,
    sm = "HR",
    backtransf = TRUE
  )

  jpeg(
    filename = file.path(output_dir, output_file),
    res = 500,
    width = 14,
    height = 6,
    units = "in"
  )

  plot.new()

  forest(
    meta_result,
    xlab = "Hazard Ratio (95% CI)",
    leftcols = c("studlab", "DemN", "N", "DemN (Consistently Active)", "Total (Consistently Active)"),
    leftlabs = c("Study", "DemN", "Total", "DemN(ref)", "Total(ref)"),
    col.square = rep("darkgrey", nrow(data)),
    col.square.lines = "black",
    col.diamond = "darkgrey",
    col.diamond.lines = "black",
    col.inside = "black",
    xlim = c(0.5, 2.5),
    at = seq(0.5, 2.5, by = 0.5),
    common = TRUE,
    random = TRUE
  )

  legend(
    x = 1.5,
    y = 7,
    legend = c("Common Effect Model", "Random Effects Model"),
    col = c("darkgrey", "darkgrey"),
    lty = 1,
    lwd = 2,
    bty = "n",
    cex = 0.8
  )

  dev.off()

  invisible(meta_result)
}

meta_results <- list()

for (i in seq_len(nrow(trajectory_map))) {
  raw_data <- read_xlsx(input_file, sheet = trajectory_map$sheet[i])
  analysis_data <- prepare_meta_data(raw_data)
  meta_results[[trajectory_map$trajectory[i]]] <- run_meta_analysis(
    data = analysis_data,
    output_file = trajectory_map$output_file[i]
  )
}
