#1.	Create these vectors:
#a.	A character vector named ‘fruits’ with these values: ‘Apple’, ‘Guava’, ‘Banana’, ‘Mango’
fruits  <- c('Apple', 'Guava', 'Banana', 'Mango')
fruits
class(fruits)
#b.	A numeric vector named ‘hundred’ comprising of the first 100 natural numbers
hundred <- c (1:100)
hundred

hun = 1:100
hun
#c.	A logical vector named ‘logic_game’ with these values: ‘TRUE’,’TRUE’,’FALSE’,’FALSE’
game <- c ('TRUE','TRUE','FALSE','FALSE')
logic_game <- as.logical(game)
logic_game
is.logical(logic_game)
# or 
game_2 <- c (TRUE,TRUE,FALSE,FALSE)
game_2
#2.	Create a list named ‘jumbo’ which comprises of:
#a.	A character vector comprising of alphabets from A to D
A_V <- c('A','B','C','D')
A_V
#b.	A numeric vector comprising of numbers from 55 to 60
N_V <- c(55:60)
N_V
#c.	A logical vector comprising of just these two values: True, False
C_V <- c ('TRUE','FALSE')
L_V <- as.logical(C_V)
L_V
#List
jumbo <- list(A_V,N_V,L_V)
jumbo
#i.	Now, access the third value from the first element of the list
first_element <- jumbo[[1]]
third_value <- first_element[3]
third_value
#i.	Access the 2nd value from the 2nd element of the list
second_element <- jumbo[[2]]
second_value <- second_element[2]
second_value
#i.	Access the 1st value from the 3rd element of the list
third_element <- jumbo[[3]]
first_value <- third_element[1]
first_value
#3.	Create a matrix named ‘four_trouble’, with the numbers 1 to 16. The matrix should have 4 rows & 4 columns
#a.	Arrange the elements by row
four_trouble <- matrix(c(1:16), ncol = 4, nrow=4, byrow = TRUE)
four_trouble
#4.	Create an array named ‘sky_maze’ with the numbers 1 to 32. The array should comprise of two 4*4 matrices
sky_maze = array (c(1:32),dim =c(4,4,2)) # 4 rows, 4 col, 2 matrix
dimnames(sky_maze) <- list(c('row1','row2','row3','row4'),c('col1','col2','col3','col4'))
sky_maze
sky_maze[2,2,1]
sky_maze[2,3,2]
sky_maze[3, ,2]
sky_maze[3,1:4,2]
