library(magrittr)
library(tibble)

lines <- readLines("app.R")

# Lop off everything before the server begins
server_start <- which(grepl("# SERVER ------------", lines, fixed=TRUE))
lines <- tail(lines, -server_start)

# Filter out all but the interesting lines
lines <- lines[
    grepl("cod[cod$cause==", lines, fixed=TRUE) |
    grepl("input\\$.*[=><]=", lines) |
    grepl("1[+-]\\(input\\$", lines)
]

# Clean up lines so that they parse cleanly as R code
## "else if" -> "if"
lines <- sub("\\belse if\\b", "if", lines)
## Delete "input$"
lines <- sub("\\binput\\$", "", lines)
## Delete leading "*("
lines <- sub("^\\s*\\*\\(", "", lines)
## Delete trailing ")"
lines <- sub("\\)\\s*$", "", lines)

df <- tibble()
df2 <- tibble()

# Parse each line and add a data frame row if possible
for (line in lines) {
    tryCatch(
        {
            expr <- parse(text=line)[[1]]
            operator <- expr[[1]]

            if (operator == "[") {
                # It's introducing a cause/category. Just record these for
                # subsequent lines.
                cause <- expr[[3]][[3]]
                category <- expr[[4]]
                stopifnot(
                    "Malformed line (cause)"=is.character(cause),
                    "Malformed line (category)"=is.character(category)
                )
            } else if (operator == "if") {
                # It's adding a factor
                comp_operator <- expr[[2]][[1]]
                var <- expr[[2]][[2]]
                value <- expr[[2]][[3]]
                multiplier <- expr[[3]][[2]]
                stopifnot(
                    "Malformed line (var)"=is.name(var),
                    "Malformed line (value)"=is.character(value) || is.numeric(value),
                    "Malformed line (multiplier)"=is.numeric(multiplier)
                )
                var <- as.character(var)
                if (comp_operator == as.symbol("==")) {
                    df <- rbind(df, tibble(category, cause, var, value, multiplier))
                } else if (comp_operator == as.symbol(">=")) {
                    if (var == "hsd") {
                        df <- rbind(df, tibble(category, cause, var, value=value:24, multiplier))
                    } else {
                        stop("Unexpected operator: ", comp_operator)
                    }
                } else {
                    stop("Unexpected operator: ", comp_operator)
                }
            } else if (expr[[2]] == 1 && (operator == "+" || operator == "-")) {
                var <- expr[[3]][[2]][[2]]
                multiplier <- expr[[3]][[2]][[3]]
                stopifnot(
                    "Malformed line (var)"=is.name(var),
                    "Malformed line (multiplier)"=is.numeric(multiplier)
                )

                var <- as.character(var)
                if (operator == "-") {
                    multiplier <- -multiplier
                }
                df2 <- rbind(df2, tibble(category, cause, var, multiplier))
            } else {
                stop("Unexpected line")
            }
        },
        error = function(err) {
            message("Error occurred on line: ", line)
            stop(err)
        }
    )
}

write.csv(df, "factors.csv", row.names = FALSE)
write.csv(df2, "factors_cont.csv", row.names = FALSE)
