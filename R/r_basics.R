#Basics

#Entering data

#colon
(x1 <- 0:10)
(x2 <- 10:0)

#seq
(x3<-seq(10)) #more specific assignment than colon
(x3<-seq(30, 0, by = -3))

x3 <- c(0.2,4.5,1.3,4,5,2.3,0.7,8.1)
seq_range(x3, 5) #create an equally space vector of length 5 from min to max 
seq_range(x3, 5, pretty=T) #whole number, or 1dp, depending on range of numbers
seq_range(x3, 5,  trim=0.1) #Trim 10% of tail values, especially important if there is an outlier
seq_along(x3) #range(len) python equivalent

#c; concantenate
(x4 <- c(1,2,3,6,7,2))

#scan : interactive user input
x5 <- scan() #double enter to store
x5

#rep ; repetitions
x6 <- rep(TRUE, 5)
(x6 <- rep(c(TRUE,FALSE), 5)) #TRUE FALSE TRUE...
(x6 <- rep(c(TRUE,FALSE), each=5)) #TRUE TRUE ... FALSE FALSE

#Math
max(1,2,3)
min(1,2,3)
sqrt(9)
abs(-5)
ceiling(1.4)
floor(2.3)
2^2

5 %/% 2 #integer divide , =1
5 %% 2 #modulus, =1

#operators
5 %in% c(5,6,7)
x <- 1:10
typeof(x)
is.vector(x)


#Data type
a <- 5
typeof(a)
class(a) #numeric, integer, complex, character, logical
b <- TRUE
typeof(b)
c <- "hello"
typeof(c)
d <- "5"
class(as.numeric(d))

paste("This is", a)

#Data structure


#c is concantenate
#elements in vectors must all have same data type
## atomic types : logical, integer, double, character, complex, raw
v1 <- c(1,2,3,4,5)
fruits <- c("apple", "banana", "mango", "dragonfruit")
v_ <- c(TRUE, FALSE, TRUE)
v_ <- c(1:5, 10:12)
v_
v1
v1[2] #R starts index with 1
is.vector(v1)
length(v1)
sort(v1, decreasing=TRUE)
fruits[c(1,3)] #select 1st and 3rd element
fruits[c(1:2, 4)] #select range
fruits[c(-1)] #access all items except last element
fruits[length(fruits)] #access last item
fruits[1] <- "pear"
fruits
"pear" %in% fruits
append(fruits, "kiwi") #append to last element, not inplace
fruits

v_ <- rep(c(1,2,3), each=3) # each=3 ---> [1,1,1,2,2,2,3,3,3] repeat each element 3 times
v_ <- rep(c(1,2,3), times=3) #[1,2,3,1,2,3,1,2,3];
v_

repeat_indepent <- rep(c(1,2,3), times = c(5,2,1)) # [ 1,1,1,1,1,2,2,3]

v2 <- c("Hello", "A", "C")
v2
is.vector(v2)

v3 <- c("Hello", 2, "C") #converts into a string
v3
is.vector(v3)

## String vectors

text <- "hello"
nchar(text)
grepl("h", text) #check if h in text
text2 <- "world"
paste(text, text2) #concat 2 string
cat(text, text2, sep=" ")

sentence <- "We are the \"Vikings\"" # \escape character; \n new line
cat(sentence)

##naming vectors
v4 <- c(a = 1, b = 2, c = 3)
z <- v4[1]
z <- v4["a"] #returns a named value, same as v4[1]
z <-v4[["a"]] #returns the element 1

#Sequences; vectors too
numbers <- 1:10
numbers <- seq(from=0, to=100, by=20) #equivalent to python range(0,100,20); inclusive both sides


#Lists
#Similar operations to vectors but can contain many different data types
l1 <- list("Hello", 5, "ok")
typeof(l1)
l1[2]
l2 <- append(l1, 7, after = 1) #insert after index 1...index 2
l2[2]
7 %in% l2
l2[-1] #remove first element from list.

thislist <- list("apple", "banana", "cherry", "orange", "kiwi", "melon", "mango")
thislist[-2] #remove 2nd item on the list. cherry now becomes 2nd index
thislist[2:5]

list1 <- list("a", "b", "c")
list2 <- list(1,2,3)
list3 <- c(list1,list2) #list1 + list2 equivalent

list3
list4 <- list(list1, list2) #list.append(list) equivalent
str(list4) #lists are best inspected using str
length(list4)
list4[[1]]
list4[1]
list4[[1]][1] #[[]] to access dim1, [] to access dim2; decreasing order 

list4 <- list(a = 1:3, b = "a string", c = list(list1, list2))
##subsetting a list always return a list
### [] extracts a sublist -> always returns a list
list4[1] 
class(list4[1]) #list
class(list4[[1]]) #integer -> use [[]] to drill down individual components

list4[[1]] #[[]] returns a vector; extract a single component from list; remove a level of hierarchy from list
list4[[3]][[1]][[1]] #get 1st element of list1

list4$c[[1]][[1]] #$ works like [[]], for named vectors
class(list4$c[[1]][[1]]) #char
class(list4$c[[1]][1]) #list

#matrix
m1 <- matrix(c("a", "b", "c", "d"), nrow=2, ncol=2) #automatically divides the elements to fit 2 rows
m1 #fill each column (depth first)
m2 <- matrix(c("a", "b", "c", "d"), nrow=2, byrow=T) #fill each row first, breadth first
m2
m2[2,1] #row index, col index
m2[2,] #get 2nd row
m2[,2] #get 2nd col
my_vector <- c(1:12)
my_matrix <- matrix(my_vector, byrow=T, nrow=3)
my_matrix
my_matrix[c(1,2),c(3,4)] #get 1st and 2nd row, 3rd and 4th column
my_matrix2<- matrix(my_vector, byrow = F, nrow=3) #shape = 3x4
cbind(my_matrix2, c(0,0,0)) #append new column into matrix. shape = 3x5
rbind(my_matrix2, c(0,0,0,0)) #append new row into matrix, shaoe = 4x4
3 %in% my_matrix2
dim(my_matrix) #row column
nrow(my_matrix)
ncol(my_matrix)
length(my_matrix) #row*column

#Loop through matrix
for (rows in 1:nrow(my_matrix)) {
  for (columns in 1:ncol(my_matrix)) {
    print(my_matrix[rows, columns])
  }
}

my_matrix[-2,-1] #remove second row, first column
my_matrix[-c(1,2), -c(3)] #remove first and second row, remove 3rd column
my_matrix + my_matrix2
my_matrix * my_matrix2 #element wie multiplication
(my_matrix3 <- t(my_matrix)) #traspose
dim(my_matrix3)
(my_matrix %*% my_matrix3) #mxn x nxo = mxo. shape = 3x4
my_matrix3 <- rbind(my_matrix, 13:16) #add new row
my_matrix3[,-3] #removes 3rd column
det(my_matrix3) #close to zero, computationally singular. System of linear equations cannot be solved
solve(my_matrix3)

##linear algebra
a = rbind(1:3, c(2,3,4), c(1,2,1))
a
det(a)
solve(a)
a %*% solve(a)

#Solving linear equation example
#  3x1 + 2x2 - 1x3 = 1
#  2x1 - 2x2 + 4x3 = -2
#  -1x1 + 0.5x2 - 1x3 = 0
# A %*% x = b ----> A-1 %*% b = x

(A <- array(c(3,2,-1,2,-2,0.5,-1,4,-1), dim = c(3,3)))
b <- c(1,-2,0)
(x <- solve(A,b)) #returns a vector of len(3)
solve(A) #inverse A matrix
(z <- solve(A) %*% b)
dim(z) #returns a matrix of 3x1
all.equal(z[,1], x)

b <- matrix(c(1,2,3,4,5,6), dim=c(2,2))

#array - matrix but with more than 2 dimensions. Can only have one data type
a1 <- array(c(1:24)) #1d array
a1 <- array(c(1:24), dim= c(4,3,2)) #2 matrices of 4by3 each. Diffeent from numpy. [row, col, ndim]
a1
dim(a1)
length(a1) #4x3x2
nrow(a1)
a1[,,2]
a1[c(1,2), ,2]
2 %in% a1
for (x in a1) {print(x)} #loop each element

#Create dataframe -- can have different data types in it
vNumeric <- c(1,2,3)
vCharacter <- c("a", "b", "c")
vLogical <- c(T,F,T)

dfa <- cbind(vNumeric, vCharacter, vLogical)
dfa #turns everything into the most general, which is character
typeof(dfa) #prints dtype; character
class(dfa) #matrix, array

df <- as.data.frame(cbind(vNumeric, vCharacter, vLogical))
df
df <- data.frame(
  numeric_column = c(1,2,3),
  character_column = c("a", "b", "c"),
  logical_column = c(TRUE, TRUE, FALSE)
)
typeof(df) #list
class(df) #data.frame
is.data.frame(df)
row.names(df) <- c("Index1", "Index2", "Index3") #naming row indices
df
all.equal(df$numeric_column, df[,1], df[["numeric_column"]])
df[1,1] == df['Index1', 1] #access index by either iloc or loc
new_row <- c(4, "d", FALSE)
(df <- rbind(df, Index4=new_row))
(df<- df[-nrow(df), ])
new_column <- c(20,30,40)
(df <- cbind(df, new_col = new_column))
(df <- df[, c(-ncol(df))]) #remove last column

nrow(df)
ncol(df)
length(df) #to find number of columns
dim(df)

df2 <- data.frame(
  numeric_column = c(4,5),
  character_column = c("d", "e"),
  logical_column = c(TRUE, FALSE)
)
rbind(df, df2)

df3 <- df[, c(-1)]
cbind(df, df3)

summary(df) #df.describe()
str(df) #prints out structure. df.info()
names(df) #prints out column names. df.columns()
df[2, ] #2nd row, all columns
df[,3] #all rows, 3rd column
subset(df, vNumeric > 1)
subset(df, vLogical == TRUE)

df2 <- data.frame(rbind(df, c(1, 'a', FALSE)))
df2[order(df2$vNumeric, decreasing=FALSE),] #sort

dim(df2)
nrow(df2)

#Create list
o1 <- c(1,2,2)
o2 <- c("a", "b", "c", "d")
o3 <- c(T,T,F,F)

list1 <- list(o1,o2,o3)
list1
list2 <- list(o1,o2,o3,list1)
list2

#Coerce data types
coerce1 <- 5
typeof(coerce1)
coerce2 <- as.integer(coerce1)
typeof(coerce2)
coerce3 <- c("1","2",3) #combines all into charcter
typeof(coerce3)
coerce4 = as.numeric(coerce3)
coerce4

#coerce matrix to data frame
(coerce6 <- matrix(1:9, nrow=3))
is.matrix(coerce6)
(coerce7 <- as.data.frame(matrix(1:9, nrow=3)))
is.data.frame(coerce7)

#Factors - for categorical data, assign labels to variables; cannot append item that is not in levels
music_genre <- factor(c("Jazz", "Rock", "Classic", "Classic", "Pop", "Jazz", "Rock", "Jazz"), levels = c("Classic", "Jazz", "Pop", "Rock", "Other"))
levels(music_genre)
fruits <- c("apple", "kiwi", "mango", "apple")
fruits <- as.factor(fruits)
levels(fruits)
x1<-1:3
is.vector(x1)
y <- 1:9
df1 <- cbind.data.frame(x1,y) #combines 2 vectors into a dataframe, variable name as columns
df1 #it repeats x1 because x1 has 3 elements but y has 9
df1$x1 #access x1 columns
is.data.frame(df1['x1']) #TRUE
typeof(df1$x1)

is.data.frame(df1$x1) #FALSE
typeof(df1["x1"])

str(df1) #get structure of dataframe
x2 <- as.factor(c(1:3))
typeof(x2)
df2 <- cbind.data.frame(x2,y)
str(df2)

#Define existing variable as factor
x3 <- c(1,2,3,2,1,2,3,2,1)
df3 <- cbind.data.frame(x3,y)
df3$x3 <- factor(df3$x3, levels=c(1,2,3), labels = c("A", "B", "C"))
df3 #automatically label each integer in x3 to its appropriate label
typeof(df3$x3) #however it is still an integer, the factors are given labels
str(df3)

#Vector attributes
##names, dimensions, class -> used to implement the s3 object oriented system

## most important generic functions are print() and the subsetting functions [], [[]], $
as.Date #generic functions will call a specific method based on the class of the first argument
methods("as.Date") #see available methods of the generic function
getS3method("as.Date", "numeric") #to see specific implementation of the method
x<-5
if (inherits(x, "numeric")) {print(class(x))} #inherits --> isinstance based on class(x)

date <- as.Date("24/05/1996", format = "%d/%m/%Y")
attributes(date) #has class attribute

x <- c(a = 1, b=2)
names(x) #a, b
attributes(x) #only names

z <- matrix(c(1,2,3,4), nrow = 2, byrow=T)
attributes(z) #dim attribute
attr(z, "dim")
dim(z)

attributes(cars) #see what avaialable attributes cars dataset have
names(cars) #each column has a name
class(cars)

## Augmented vectors
## 1. Factors -> built on top of integers with levels
x <- factor(c("ab", "cd", "ab"), levels = c("ab", "cd", "ef"))
typeof(x) #integer
attributes(x) #$levels, $class = "factor
class(x)

## 2. date and datetime -> numeric vectors that represent the number of days since 1 Jan 1970
x <- as.Date("1971-01-01")
unclass(x) #365
typeof(x) #double
class(x) #date
x <- lubridate::ymd_hm("1970-01-01 01:00") #datetime
typeof(x) #double
class(x) #POSIxct, represent number of seconds since 1 Jan 1970

## 3. Tibble
tb <- tibble::tibble(x = 1:5, y = 5:1)
typeof(tb) #list -> diff between list and dataframe is that all elements of df must be vectors with same lenth
attributes(tb) #names, row.names, class
names(tb) #column names
row.names(tb) #index
class(tb) #tbl_df, tbl, data.frame #class includes dataframe -> tibble inherits the regular data frame behaviour

#Programming fundamentals

a <- c(16,9,13,5,2,17,14)
b <- c(17,7,5,16,8,13,14)
a <10
c <- c(a,b)
c <- matrix(c(a,b), nrow=2, byrow=TRUE)
c > 14
# & | !

x = 5
# if
if(x > 3){
  "x is larger than three"
  } else if(x < 3) {
    "x is smaller than 3"
    } else {
      "x is 3"
    }


#for (var in seq) {expression}
for (elem in a) {
  if (elem %% 2 == 0) {
    print("even")
  } else {
    print("odd")
  }
}
for (i in 1:10) {
  print(i)
}

fruits_list <- list("apple", "berry", "mango")
class(fruits_list) #list
fruits_char <- c("apple", "berry", "mango")
class(fruits_char) #character, but iterable. Vector
is.vector(fruits_char) #True
for (fruit in fruits) {
  print(paste(fruit, "is good"))
}
adj <- list("red", "blue", "yellow")
for (col in adj){
  for (fruit in fruits_list) {
    print(paste(col, fruit))
  }
}

## append to new vector after each iteration
library(tidyverse)
df <- tibble(
  a = rnorm(10),
  b = rnorm(10),
  c = rnorm(10),
  d = rnorm(10)
)
output <- vector("double", length = ncol(df)) #initialize empty vector with predefined length; type of vector : logical, integer, double, character, etc.. typeof
for (i in seq_along(df)) { #length of df is 4 -> number of columns
  output[i] <- median(df[[i]])
}

output <- double() #initialize empty vector with undetermined length
for (i in seq(10000) ) { #length of df is 4 -> number of columns
  output <- append(output, i)
}

#while loop
## while (logic) {...}
while (x>2) {
  print("x is large than 2"); x = x-1}

i <- 1
while (i<10) {
  
  i <- i +1
  if (i==5) {
    print("Skipping 5")
    next #continue equivalent
  }
  if (i==8) {
    print("Breaking at 8")
    break #end loop
  }
  print(i)
}

#Boolean
10 == 10
if ((10>9) & (5>9)) {
  print("true")
} else if (10>9 | 5>9) {
  print("second")
} else {
  print("false")
}

#Function
##function(arg1, arg2) {body}

squareroot <- function(x) {
  if (x<0) {
    print(paste(x, "is a negative number"))
  }
  else {
    return(sqrt(x))
  }
}
squareroot(9)
squareroot(-9)



my_function <- function(x) {
  print(paste("Hello", x))
  return (paste("good morning", x))
}
a <- my_function("chris")
a

Outer_func <- function(x) {
  Inner_func <- function(y) {
    a <- 2*x + y
    return(a)
  }
  return (Inner_func)
}
output <- Outer_func(3) # To call the Outer_func, define x
output(5) #to call inner func, define y

## functions with for loop
library(tidyverse)
df <- tibble(
  a = rnorm(10),
  b = rnorm(10),
  c = rnorm(10),
  d = rnorm(10)
)
rescale01 <- function(x) { #performs min-max scaling
  rng <- range(x, na.rm = TRUE) #get min and max value; return a vector
  (x - rng[1]) / (rng[2] - rng[1])
}

for (i in seq_along(df)) {
  df[[i]] <- rescale01(df[[i]])
}

## Creating a function with function as input
### functions are S3 objects (OOP), with methods
overall_func <- function(x, fun) {
  y <- x ^ 2
  output <- fun(y)
  return(output)
}

overall_func(c(1,2,3), mean) #input function is mean()

tri_recursion <- function(k) {
  if (k > 0) {
    result <- k + tri_recursion(k - 1) #solve tri_recursion(0), then tri_recursion(1), then ...
    print(result)
  } else {
    result <- 0
    print(result)
    return(result)
  }
}
ans <- tri_recursion(6)

wt_mean <- function(x, w) {
  #  if (length(x) != length(w)) {
  #    stop("`x` and `w` must be the same length", call. = F) #break and error description
  #  }
  #  if (!is.vector(x) | is.vector(w)) {
  #    stop("both x and w have to be vectors")
  #  }
  stopifnot(length(x) == length(w), is.vector(x), is.vector(w)) #stops if any returns FALSE; generic message
  sum(w * x) / sum(w)
}

a <- c(1,2)
b <- 5
wt_mean(a,b)

# Mapping / Applying
## purrr package -- map -> returns a list; map_dbl, mp_lgl, map_int, map_chr; map(data, function). apply lambda function can be written with ~
library(tidyverse)
df <- tibble(a = rep("hello", times=1000),
             x=1:1000,
             y=rnorm(1000),
             z=runif(1000)
             )
map(df, mean) #returns a list; 
map_dbl(df, mean) #returns a numeric named vector; if not a double, will coerce to na

map_dbl(df, median)
map_chr(df, str_to_upper)

models <- mtcars %>% split(.$cyl) %>% map(~ lm(mpg ~ wt, data = .)) #first split dataset by each unique values of cyl, then apply a linear regression model to each dataset
str(models) #. refers to the vector that was passed
models %>% map(summary) %>% map_dbl(~.$r.squared) #apply summary to each model, then access the r-squared named vector using map_dbl to return a vector
models %>% map(summary) %>% map_dbl("r.squared") #shortcut for above
x <- list(list(1, 2, 3), list(4, 5, 6), list(7, 8, 9))
x %>% map_dbl(2) #get 2nd element of each list -> same as for (i in x) {list[[i]][[2]]}


mtcars %>% map_dbl(mean) #get mean of each column
rescale <- function(x) {
  rng <- range(x)
  (x-rng[1]) / (rng[2] - rng[1])
}
mtcars[1:3] %>% map_dfr(rescale) #map into dataframe
mtcars[1:3] %>% sapply(rescale) %>% as_tibble() #same

##map2 -> maps 2 vectors instead of 1; for function that takes 2 vector as argument; both vectors must have same length
mu <- c(1,5,10)
sigma <- c(0.1, 1, 10)
##1 example
rnorm(n=5,mu[[1]], sigma[[1]])
map2(mu, sigma, rnorm, n = 5) %>% str() #returns a list of 3, each list element is rnorm(n=5, mu[[i]], sigma[[i]])

###pmap -> for more than 2 vectors; takes a list of arguments; works like zip
n <- c(1,3,5)
args1 <- list(n = n, mean = mu, sd = sigma) #name the list as the argument for function for safer calls
args1 %>% pmap(rnorm) #returns a list of 3 lists. Each list is output of rnorm(n[[i]], mu[[i]], sigma[[i]])

df <- tibble(x=c(1,2,3),
             y = c(4,5,6))
df %>% map_dbl(mean)

## Invoke different functions; zip(function, parameters)
f <- c("runif", "rnorm", "rpois") #put the functions into a vector
param <- list( #include the parameters for each function
  list(min = -1, max = 1), 
  list(sd = 5), 
  list(lambda = 10)
)
invoke_map(f, param, n=5) %>% str() #list[[1]] -> runif(n=5, min=-1, max=1); list[[2]] -> rnorm(n=5, sd=5)


## Apply family
### lapply : same as map; sapply : wrapper fn that automatically simplify output; vapply : safe alternative to sapply by specifying the output type
x1 <- list(
  c(0.27, 0.37, 0.57, 0.91, 0.20),
  c(0.90, 0.94, 0.66, 0.63, 0.06), 
  c(0.21, 0.18, 0.69, 0.38, 0.77)
)
x2 <- list(
  c(0.50, 0.72, 0.99, 0.38, 0.78), 
  c(0.93, 0.21, 0.65, 0.13, 0.27), 
  c(0.39, 0.01, 0.38, 0.87, 0.11)
)
threshold <- function(x, cutoff = 0.8) x[x > cutoff]
x1 %>% lapply(threshold) #returns a list
x1 %>% sapply(threshold) #returns a list as 3rd element of list is numeric(0) instead of a vector
x2 %>% sapply(threshold) #simplify to vector instead of list, lost the structure
x2 %>% vapply(is.numeric, logical(1)) #same as map_lgl(x2, is.numeric)

# Apply lambda
df <- data.frame(x = runif(100), y = runif(100))
df <- sapply(df, function(x) ifelse(x>0.8, NA, x)) #ifelse is the lambda function as controlled by x
sapply(as_tibble(df), function(x) sum(is.na(x))) #count number of missing values



# Safely --> works like try but return error output; will not break if code fails due to parse issue
safe_log <- safely(log) #eg log(x)
safe_log(10) #outputs a list with result and error, error is null
safe_log("a") #result is null, report error
x <- list(1, 10, "a")
y <- x %>% map(safely(log)) 
str(y) #for each element in y, it outputs a list with [[1]] $result and [[2]] $error
y %>% transpose() %>% .$result #returns the results of all elements in the list

y <- y %>% transpose() 
is_ok <- y$error %>% map_lgl(is_null) #returns True if the elements in list$error does not have any null values
x[!is_ok] #subset those that are not okay

## Possibly - simpler safely -> give a default value if return an error
x <- list(1, 10, "a")
x %>% map_dbl(possibly(log, NA_real_)) #return NA for "a"

## ...
### Most functions in r take arbitary number of inputs
### Useful when you create a function to send to another function
commas <- function(..., message) {paste(message, stringr::str_c(..., collapse = ','))} #... takes in as many input other than message
commas('a', 'b', 'c', message = "Hello", "d") #concat into "a,b,c"; takes in as many input

## invisible return
### useful for subsequent pipe, but dont want the output to be printed
show_missings <- function(df) {
  n <- sum(is.na(df))
  cat("Missing values: ", n, "\n", sep = "")
  
  invisible(df)
}
x <- show_missings(mtcars) #x is the cars df

mtcars %>% 
  show_missings() %>% 
  mutate(mpg = ifelse(mpg < 20, NA, mpg)) %>% 
  show_missings() 

#Walks
## Use this if the action is important but do not care about the return value
## Works like map but only execute action and not return a value
library(ggplot2)
plots <- mtcars %>% 
  split(.$cyl) %>% 
  map(~ggplot(., aes(mpg, wt)) + geom_point()) #Save the scatterplots for each unique cyl
paths <- stringr::str_c("scatter",names(plots), ".pdf") #names(plots) -> c("4", "5", "6")

pwalk(list(paths, plots), ggsave, path = getwd()) #iterate through each plot, then apply ggplot(path[[i]], plots[[i]], path)

# Predicate functions
## keep/discard
library(tidyverse)
flights %>% keep(is.character) #all the character datatypes. keep those that returns T
flights %>% discard(is.character) #exclude those that returns T

##some/every
x <- list(1:5, letters, list(10))
some(x, is.character) #some any(boolean logical)
every(x, is.vector) #returns T only if all are T
every(x, is.character) #False

#Reduce
## Given a complex list, you want to reduce to a simple list by repeatedly applying a function that reduces the pair to a singleton
## reduce(func) takes a binary function (func with 2 inputs) and applies it repeatedly until theres only one left
dfs <- list(
  age = tibble(name = "John", age = 30),
  sex = tibble(name = c("John", "Mary"), sex = c("M", "F")),
  trt = tibble(name = "Mary", treatment = "A")
)
dfs %>% reduce(full_join) #full_join(age, sex) %>% full_join(trt)

## Accumulate is similar to reduce but keep intermediate results
dfs %>% accumulate(full_join) #returns a list of 2 named elements. $sex is full_join(age, sex); $trt is full_join(., trt)


#PLotting
plot(1,3) #plot one point
#scatter plot
plot(c(1,3), c(2,4)) #plot 2 points, (1,2) + (3,4)
x<- c(1,2,3)
y <- c(4,5,6)

plot(x,y, main="Cars vs speed", xlab="Cars", ylab = "Speed", col="red", cex=2)

# day one, the age and speed of 12 cars:
x1 <- c(5,7,8,7,2,2,9,4,11,12,9,6)
y1 <- c(99,86,87,88,111,103,87,94,78,77,85,86)

# day two, the age and speed of 15 cars:
x2 <- c(2,2,8,1,15,8,12,9,7,3,11,4,7,14,12)
y2 <- c(100,105,84,105,90,99,90,95,94,100,79,112,91,80,85)

plot(x1, y1, main="Observation of Cars", xlab="Car age", ylab="Car speed", col="red", cex=2)
points(x2, y2, col="blue", cex=2)

plot(1:10, col='red', cex=2, pch=10) #plot sequence c(1,2,3,...,10), c(1,2,3,...10); cex : shape size, pch : shape

#Line plots
plot(x,y, type = "l", main="Title here", xlab="The x-axis", ylab=("The y-axis"), col='blue', lwd=2, lty=3) #plot a line; lty = weights of line. lwd = line width

l1 <- c(1,2,3,7,10)
l2 <- c(2,5,7,8,9)

plot(l1, type = "l", col="blue") #if x is not given, index is used

l3<- c(1,3,5,7,10)
plot(l1, l2, type="l", col="blue")
lines(l1, l3, type="l", col = "red") #overlay another line

#Bar charts
x <- c("A", "B", "C", "D")
y <- c(2, 4, 6, 8)

width_vec <- rep(1, length(x))
width_vec[3] <- 2
barplot(y, names.arg = x, width = width_vec, col="red", horiz=FALSE)

####modelling

#Hierarchical clustering
library(datasets)
library(dplyr)
?mtcars
head(mtcars)
cars <- mtcars[,c(1:4, 6:7, 9:11)] #select variables
head(cars)

hc <- cars %>% #get cars data
  dist %>% #compute distance matrix
  hclust #compute hierarchical clusters
plot(hc) #dendogram
rect.hclust(hc, k=2, border='gray')
rect.hclust(hc, k=3, border='blue')
rect.hclust(hc, k=4, border='green4')
rect.hclust(hc, k=5, border="darkred")

#Principal components
pacman::p_load(pacman, dplyr, GGally, ggplot2, ggthemes,
               ggvis, httr, libridate, plotly, rio, rmarkdown, shiny,
               stringr, tidyr)
library(datasets)
head(mtcars)
cars <- mtcars[, c(1:4, 6:7, 9:11)]
pc <- prcomp(cars, center=TRUE, scale=TRUE)
pc <- prcomp(~mpg + cyl + disp + hp + wt + qsec + am +gear+ carb,
             data=mtcars,
             centre=TRUE,
             scale=TRUE)
summary(pc)
plot(pc) #scree plot
pc #correlation between pc
biplot(pc) #cars along the same direction are grouped together

#Regression
head(USJudgeRatings)
data <- USJudgeRatings #dataframe
x <- as.matrix(data[-12]) #all columns except column 12
y <- data[,12]
reg1 <- lm(y~x)
reg1 <- lm(RTEN ~ CONT + INTG + DMNR,
           data=USJudgeRatings)
reg1
summary(reg1)
anova(reg1)
coef(reg1)
confint(reg1) #95 % CI
resid(reg1) #for each observation
hist(residuals(reg1))

#lasso regression
p_load(lars)
lasso <- lars(x,y, type="lasso")
r2comp<-c(lasso$R2[6]) %>% round(2)
names(r2comp) <- c("lasso")
r2comp
