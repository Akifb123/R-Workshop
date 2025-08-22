#EXCERCISE 1

# Create a vector containing the values 1,2,3,4,5
v1 <- c(1, 2, 3, 4, 5)
print(v1)

# Create a vector containing the values 1 to 100
v2 <- 1:100
print(v2)

# Create a vector containing the values 0,5,10,15,20
v3 <- seq(0, 20, by = 5)
print(v3)

# Create a vector containing the values 1,1,2,2,3,3
v4 <- rep(1:3, each = 2)
print(v4)

# Create a vector containing the values 1,1,5,7,9,10
v5 <- c(1, 1, 5, 7, 9, 10)
print(v5)

#EXCERCISE 2

# Create a vector containing the values 1 to 10
v <- 1:10
print(v)

# All but the first and last value
no_first_last <- v[-c(1, length(v))]
print(no_first_last)

# All but the second and fifth value
no_2_5 <- v[-c(2,5)]
print(no_2_5)

# Square root of sixth and seventh positions
squareroot_6_7 <- sqrt(v[c(6,7)])
print(squareroot_6_7)

# Alternating positions (odd indices)
alternate <- v[seq(1, length(v), by = 2)]
print(alternate)


#Excercise 3

# Number of files in present working directory
num_files <- length(list.files())
print(num_files)

# List the first file in present working directory
first_file <- list.files()[1]
print(first_file)

#EXCERCISE 4

# Create gene name vector
genes <- c("Gene_1", "Gene_2", "Gene_3", "Gene_4")

# Expression values
expression <- c(1000, 3000, 10000, 12000)
names(expression) <- genes
print(expression)

# Gene lengths
lengths <- c(100, 3000, 200, 1000)
names(lengths) <- genes
print(lengths)

# Find the longest gene
longest_gene <- names(lengths)[which.max(lengths)]
print(longest_gene)

# Genes with length > 100 and expression > 10000
selected_genes <- genes[lengths > 100 & expression > 10000]
print(selected_genes)

# Length-normalised expression
norm_expr <- expression / lengths
print(norm_expr)

# Genes with normalised expression > average
above_avg <- names(norm_expr)[norm_expr > mean(norm_expr)]
print(above_avg)

