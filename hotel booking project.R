?rnorm
x <- c(1.1,2.2,33,45,67,87)
x+1
sin(x)
log(x)
sqrt(x)
exp(x)
m <- mean(x)
x[x<m]
s <-sd(x)
x[x < (m - s) | x > (m + s)]
fruit <- c(5,10,20,25)
names(fruit) <- c("orange","banana","apple","peache")
lunch <- fruit[c("apple","orange")]
print(lunch)


fruit <-c(5, 10, 1, 20)
names(fruit) <-c("orange", "banana", "apple", "peach")
lunch <- fruit[c("apple","orange")]
print(lunch)


z <-c(1:3,NA)
z == NA
x <-c(-5:-1, NA, NA, 1:3)
m <-mean(x)
print(m)
