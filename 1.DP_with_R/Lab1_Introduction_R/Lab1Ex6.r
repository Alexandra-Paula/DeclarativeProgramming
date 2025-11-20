A = matrix(c(1, 10, 5, 7, 2, 12, 9, 1, 3, 13, 8, 4, 5, 22, 34, 3),
  nrow = 4,
  ncol = 4,
  byrow = FALSE
)

rownames(A) = c("a", "b", "c", "d")
colnames(A) = c("a", "b", "c", "d")

cat("The 4x4 matrix: \n")
print(A)

#save A for the last step data.txt
write.table(A, file = "data.txt", row.names = FALSE, col.names = FALSE) 

A[3, 2]

#rand 2 extras
row_vec <- c("b")
rowA <- A[row_vec, ]
print(rowA)
#col 4 extras
col_vec <- c("d")
colA <- A[,col_vec]
print(colA)
#indep primul rand si doua coloana
A <- A[-c(1), -c(2)]
A

