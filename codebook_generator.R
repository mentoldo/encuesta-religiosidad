# Write a codebook
class_list <- lapply(religiosidad, class)
class_vector <- do.call(c, class_list)
class_vector <- class_vector[-3]
class_vector

code_table <- cbind(names(classes), names(religiosidad), class_vector)
rownames(code_table) <- NULL
colnames(code_table) <- c("Code", "Nombre", "Tipo")

library(knitr)
writeLines(kable(code_table, format = "markdown"), "code_book.md")
