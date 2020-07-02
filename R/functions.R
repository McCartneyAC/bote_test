is_extant <-function(x) any(!is.na(x))
is_numeric<-function(x) any(is.numeric(x))

dossier<-function(df, id, value, ...){
  id <- substitute(id)
  t(filter(df, !!id  == value))
}


# Data import
use <- function(name) {
  # consider future support for .json?
  csv <- ".csv"
  xlsx <- ".xlsx"
  dta <- ".dta"
  sav <- ".sav"
  rda <- ".rda"
  if (grepl(csv, name)) {
    readr::read_csv(name)
  } else if (grepl(xlsx, name)) {
    readxl::read_xlsx(name)
  } else if (grepl(dta, name)) {
    haven::read_dta(name)
  } else if (grepl(sav, name)) {
    haven::read_spss(name)
  } else if (grep1(rda, name)) {
    load(name)
  } else {
    stop("unknown data type.")
  }
}

gg_added_var <- function(partial, extended, se = TRUE) {
  # In a multiple regression, the added variable plot for a predictor X, say,
  # is the plot showing the residual of Y against all predictors except X against the
  # residual of X on all predictors except X, of course.
  # Adapted from Steven Pollack
  # https://github.com/stevenpollack/stat151a/blob/master/From_Lab/Feb-26-2014.R
  require(ggplot2)
  partial_residuals <- resid(partial)
  full_residuals <- resid(extended)
  avPlot <- ggplot(
    data = data.frame(x = partial_residuals, y = full_residuals),
    aes(x = partial_residuals, y = full_residuals)
  ) +
    labs(title = "Added Variable Plot",
         x = "Residuals Regressing chosen X (on all other predictors)",
         y = "Residuals Regressing Y (without chosen X)") +
    geom_point() +
    theme_light()
  if (se) {
    avPlot <- avPlot +
      stat_smooth(method = "lm")
  } else {
    avPlot <- avPlot +
      stat_smooth(method = "lm", se = FALSE)
  }
  return(avPlot)
}
