#For the car dataset:
head(mtcars)
#The full model
M <- lm(mpg ~ . , data =mtcars)

#Search of the best model
(M1 <- step(lm(mpg ~ . , data = mtcars), 
            scope= "mpg ~ .", direction="both"))

(M2 <- step(lm(mpg ~ . , data = mtcars), 
            scope= "mpg ~ .", direction="backward"))

AIC(M)
AIC(M1)
AIC(M2)
