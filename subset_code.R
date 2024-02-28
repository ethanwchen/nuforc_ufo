# keyword definitions
keywords <- list(
  food_pills = c("food", "pills", "nutrition", "ingestible"),
  bullet = c("bullet", "projectile", "missile"),
  force_field = c("force field", "barrier", "shield", "protection"),
  blinding_light = c("blinding", "intense light", "bright light"),
  odd_anatomy = c("odd anatomy", "strange body", "alien features")
)

check_keywords <- function(text, keywords) {
  if (is.na(text)) {
    return(0)
  }
  # check if keyword is in
  any(sapply(keywords, function(keyword) grepl(keyword, text, ignore.case = TRUE)))
}

# create new columns
for (theme in names(keywords)) {
  data[[theme]] <- apply(data, 1, function(row) {
    max(
      as.integer(check_keywords(row['Summary'], keywords[[theme]])),
      as.integer(check_keywords(row['Narrative'], keywords[[theme]]))
    )
  })
}

# verify
head(data[, c('Summary', 'Narrative', 'food_pills', 'bullet', 'force_field', 'blinding_light', 'odd_anatomy')])
