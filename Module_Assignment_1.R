#Assignment

#Set up r studio in your pc.
#Print ‘x’,
y=2
z=7
x=y*z
x

#Print using rep function
#“123123123123”
rep(1:3,4)
#“1111222233334444”
rep(1:4,rep(4,4))
#4. Create a vector ‘x’ and store 21,34,2,5,33,23 in it.
x <- c(21,34,2,5,33,23)
x
#5. Find the level using gl function
#“112233441122334411223”
levels_gl <- gl(4, 2, 21)
levels_gl