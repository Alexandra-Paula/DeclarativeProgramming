#Ex1 - merge two data frames by common columns
a.data <- data.frame(
  ident = c(1:5),
  sexe = c("H", "F", "F", "H", "H"),
  poids = c(75, 68, 48, 72, 83)
)
a.data

b.data <- data.frame(
  ident = c(1:5),
  sexe = c("H", "F", "F", "H", "H"),
  poids = c(182, 165, 160, 178, 183)
)
b.data
# merge A and B using common columns
merge(a.data, b.data, by = c("ident", "sexe"))

#Ex2 - extract specific values from a vector
x=c(0.2, 0.6, 2.1, 3.7, 2.8, 2.7, 1.9, 2.3, 5.9)
x[2:3]

#Ex3 - create a matrix and calculate determinant & inverse
A = matrix(c(1, 0, 3, 4, 6, 6, 0, 4, 5, 6, 2, 3, 0, 1, 2, 4),
           nrow = 4,
           ncol = 4,
           byrow = FALSE
)

rownames(A) = c("row-1", "row-2", "row-3", "row-4")
colnames(A) = c("column 1", "column 2", "column 3", "column 4")

A
# determinant 
det(A)
# matrix inverse
solve(A)

#Ex4 - basic statistics on Orange dataset
data("Orange")
View(Orange)

# calculate mean, median, sd, and summary
mean(Orange$age)
mean(Orange$circumference)

median(Orange$age)
median(Orange$circumference)

sd(Orange$age)
sd(Orange$circumference)

summary(Orange$age)
summary(Orange$circumference)

# extract selected columns
dataOrange <- Orange[, c("age", "circumference")]
dataOrange

# compute deciles (D1–D9) using apply and quantile
deciles <- apply(dataOrange, 2, quantile, probs = seq(0.1, 0.9, by = 0.1))
deciles

#Ex5 – create vectors using rep()
# repeat sequence three times
k = rep(c(8, 2, 6), times = 3)
k
# first method - rep(x, times)
w <- c(rep(4, 7), rep(9, 5), rep(2, 3))
w
# second method, repeat groups with specific counts
w <- rep(c(4, 9, 2), times = c(7, 5, 3))
w

#Ex6 - combine and save vectors
size <- c(178, 175, 160, 191, 176, 155, 163, 174, 182)
size1 <- c(164, 172, 156, 195, 166)
# repeat size1 twice
repSize1 <- rep(size1, times = 2)
repSize1
# extract last 7 elements of size
last7Size <- tail(size, 7)
last7Size
new.size <- c(repSize1, last7Size)
new.size
# save new.size as CSV in working directory
write.csv(new.size, file = "new_size.csv", row.names = FALSE)
getwd() #saved in /Users/alexandramanea

#Ex7 - work with iris dataset
data("iris")
View(iris)
head(iris, 7)
# create subset for Species == "versicolor"
new.iris <- subset(iris, Species == "versicolor")
new.iris
#sort descending by Sepal.Length
new.iris <- new.iris[order(new.iris$Sepal.Length, decreasing = T), ]
new.iris

#Ex8 - convert character matrix to numeric
#as.numeric
A <- matrix(c("8", "16", "9", "2"), nrow=2, byrow = T)
A
A_numeric_matrix <- matrix(as.numeric(A), nrow=2)
A_numeric_matrix

#Ex9 - manipulate data frame and extract elements
person <- data.frame(
  height = c(160, 180, 175),
  weight = c(52, 96, 60),
  age = c(18, 43, 29),
  c.eyes = c("green", "blue", "blue")
)
person
# rename column 3
colnames(person)[3] <- "new.age"
person
# rename 2nd row
rownames(person)[2] <- "Mary"
person
# remove row names
rownames(person) <- NULL
person
# add new row names
rownames(person) <- c("a", "b", "c")
person
# extract row1,column3
person[1, 3]
# extract 2nd variable (as data.frame)
person[2]
# extract 2nd variable (as vector)
person[[2]]
# extract 2nd variable (as vector)
person$c.eyes[c(1, 3)]
person$height[person$height > 160 & person$height < 180]
person$weight[person$height > 170]
person[person$weight > 52, ]
person$height[1:2] <- c(190, 158)
person

#Ex10 - create and manipulate a list
my_list <- list(
  5, 
  c(160, 180, 175),
  matrix(1:12, nrow = 4, ncol=3 ),
  data.frame(
    height = c(160, 180, 175),
    weight = c(52, 96, 60),
    age = c(18, 43, 29),
    c.eyes = c("green", "blue", "blue")
  )
)

my_list

names(my_list) <- c("five", "vector", "matrix", "dataFrame")
my_list
# extract 2nd element (as vector and list)
my_list[[2]]
my_list[2] 

# extract 1st and 3rd elements (number + matrix)
my_list[c(1, 3)]

# extract 3rd element from 2nd column in 4th element (data frame)
my_list[[4]][3,2] 

