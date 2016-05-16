# Data Structures In R
# R has a wide variety of objects for holding data, including scalars,vectors,matrices,
# arrays, data frames and lists.

# Vector
# Vectors are one-dimensional arrays that can hold numeric data, character data, 
# or logical data. The combine function c() is used to form the vector.

a <- c(1,2,3,4,5,6,7)
b <- c("abc","edf","hik")
c <- c(TRUE,TRUE,FALSE,FALSE)
a[4]
a[-4] # Except posi
a[1:3]
a[c(1,2,4)]

b[4]
b[1:3]
b[c(1,2,4)]

c[4]
c[1:3]
c[c(1,2,4)]


# Matrix
# MyMatrix <- matrix(vector, nrow=number_of_rows, ncol=number_of_columns,byrow=
# logical_value, dimnames=list(char_vector_rownames,char_vector_colnames))

cell <- c(1,2,3,4)
rowname <- c("R1","R2")
colname <- c("C1","C2")
my_matrix_byrow <- matrix(cell, nrow = 2, ncol = 2, byrow = TRUE, dimnames = list(rowname,colname))
my_matrix_byrow
# Using subscript to find the data. It is the traditional way to present the matrix data.
my_matrix_byrow[1,2]
my_matrix_byrow[,2]
my_matrix_bycol <- matrix(cell, nrow = 2, ncol = 2, byrow = FALSE, dimnames = list(rowname,colname))
my_matrix_bycol

# Arrays
# Arrays are similar to matrices but can have more than two dimensions.
# my_array <- array(vector,dimensions,dimnames=list(dim1, dim2,...))

# Will build 2*3*4 three dimensional array. 4 2*3 matrix.
v_data <- c(1:30)
dim1 <- c("A1","A2")
dim2 <- c("B1","B2","B3")
dim3 <- c("C1","C2","C3","C4")
my_array <- array(v_data,c(2,3,4),dimnames = list(dim1,dim2,dim3))
my_array 
my_array[1,2,3]

# Data Frame
# A data frame is more general than a matrix in that different columns can contain
# different modes of data (numeric, character, etc.). It’s similar to the datasets you’d
# typically see in SAS , SPSS , and Stata. Data frames are the most common data structure
# you’ll deal with in R. Very like table.
# mydata_frame <- data.frame(col1,col2.....)

patientID <- c(1, 2, 3, 4)
age <- c(25, 34, 28, 52)
diabetes <- c("Type1", "Type2", "Type1", "Type1")
status <- c("Poor", "Improved", "Excellent", "Poor")
patientdata <- data.frame(patientID, age, diabetes, status)
str(patientdata)
summary(patientdata)
# Speicifying elements of a data frame.
patientdata[1:2]
patientdata[c("age","diabetes")]
patientdata$patientID
# Name Space in R
attach(patientdata)
patientID
age
table(patientID,age)
detach(patientdata)
















# Factor
# 因子用来存储类别变量(categorical variables)和有序变量，这类变量不能用来计算而只能用来分类或者计数。因子表示分类变量，有序因子表示有序变量

patientID <- c(1, 2, 3, 4)
age <- c(25, 34, 28, 52)
diabetes <- c("Type1", "Type2", "Type1", "Type1")
status <- c("Poor", "Improved", "Excellent", "Poor")

diabetes <- factor(diabetes)
status <- factor(status)

patientdata <- data.frame(patientID, age, diabetes, status)
str(patientdata)
summary(patientdata)

score <- c('A','B','A','C','B')
scoreof <- ordered(score,levels = ('C','B','A'))
scoreof

exam <- c(98, 97, 52, 88, 85, 75, 97, 92, 77, 74, 70, 63, 97, 71, 98, 
          65, 79, 74, 58, 59, 60, 63, 87, 82, 95, 75, 79, 96, 50, 88)
exam1 <- cut(exam, breaks = 3) #切分成3组
exam2 <- cut(exam, breaks = c(0, 59, 69, 79, 89, 100)) #切分成自己设置的组
attr(exam1, 'levels'); attr(exam2, 'levels'); attr(exam2, 'class')
ordered(exam2, labels = c('bad', 'ok', 'average', 'good', 'excellent')) #有序因子

state <- c("tas", "sa", "qld", "nsw", "nsw", "nt", "wa", "wa",
           "qld", "vic", "nsw", "vic", "qld", "qld", "sa", "tas",
           "sa", "nt", "wa", "vic", "qld", "nsw", "nsw", "wa",
           "sa", "act", "nsw", "vic", "vic", "act")
incomes <- c(60, 49, 40, 61, 64, 60, 59, 54, 62, 69, 70, 42, 56,
             61, 61, 61, 58, 51, 48, 65, 49, 49, 41, 48, 52, 46,
             59, 46, 58, 43)
statef <- factor(state)
statef
levels(statef)

stderr <- function(x) sqrt(var(x)/length(x))

incomemean <- tapply(incomes,state,mean)
incomemean

incomestd <- tapply(incomes,state,stderr)
incomestd









# list 
# A list allows you to gather a variety of (possibly unrelated)
# objects  under  one  name.  For  example,  a  list  may  contain  a  combination  of  vectors,
# matrices, data frames, and even other lists.
# mylist <- list(name1 = obj1, name2 = obj2,...)

patientID <- c(1, 2, 3, 4)
age <- c(25, 34, 28, 52)
diabetes <- c("Type1", "Type2", "Type1", "Type1")
status <- c("Poor", "Improved", "Excellent", "Poor")
mylist <- list(ID = patientID, AGE = age, diabetes,status)
mylist
mylist[[2]]
mylist[["AGE"]]

#usful function
length(mylist) #Number of elements/components.
str(mylist) #Structur e of an object.
class(mylist) #Class or type of an object.
mode(mylist) #How an object is sto r ed.
names(mylist) #Names of components in an object.
head(mylist) #Lists the first par t of the object.
tail(mylist) #Lists the last par t of the object.
