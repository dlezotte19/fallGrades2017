#Importing Data
decData <- read_excel("C:/Users/Dnllz/Downloads/Total count Dec 20.5.xlsx")

#Renaming column labels to make it easier to reference data.
names(decData) <- c("Location","Ethnicity","Age","Gender","Platform","Grade2","Course","Attendance",
                    "grade","R1","R2","R3","R4","R5","R6","R7","Eval","Response")

#Coerced data from table to data frame in order to work with linear modeling 
decDataDF <- as.data.frame(decData)

#Recieved error message with lm() function, y must be 'numeric'; Filtered out ("I","M","U") grades
decDataDF2 <- decDataDF[decDataDF$grade == c("0","1","2","3","4"),]
#coerced grades to numeric class type.
class(decDataDF2$grade)<-"numeric"

#coerced Ethnicity to character data. As numeric data, ethnicities were treated as "ranks"
#As character data, it will be treated as categories.
class(decDataDF2$Ethnicity)<-"character"

#building different linear models
model1 <- glm(grade~Ethnicity*Age*Gender*Attendance*Platform,data = decDataDF2)
model2 <- glm(grade~Ethnicity*Age*Gender*Platform+Attendance,data = decDataDF2)
model3 <- glm(grade~Ethnicity*Age*Gender+Platform+Attendance,data = decDataDF2)
model4 <- glm(grade~Ethnicity*Age+Gender+Platform+Attendance,data = decDataDF2)
model5 <- glm(grade~Ethnicity+Age+Gender+Platform+Attendance,data = decDataDF2)


#Testing all models against each other to find model of best fit. Using significant level alphav = .05
anova(model1,model2,test = "Chisq") #outputs p-value of .05349, simplier model fits better(model2)
anova(model2,model3, test = "Chisq") #outputs p-value of .2508, simplier model fits better(model3)
anova(model3,model4, test = "Chisq") #outputs p-value of .8637, simplier model fits better(model4)
anova(model4,model5, test = "Chisq") #outputs p-value of .1587, simplier model fits better(model5)

#Model5 proved to fit the data the best:
#grade = 3.461 + .00163Hispanic + .0327Asian - .03311Black + .000316Age +.0227Gender + .017Platform + .00003447Attendance

#RESULTS#
#None of the variables proved to show a significant effect on the grade recieved.
summary(model5)


#testing the 2nd grade column in the data, originally labeled "gr4de"
model21 <- glm(Grade2~Ethnicity*Age*Gender*Attendance*Platform,data = decDataDF2)
model22 <- glm(Grade2~Ethnicity*Age*Gender*Platform+Attendance,data = decDataDF2)
model23 <- glm(Grade2~Ethnicity*Age*Gender+Platform+Attendance,data = decDataDF2)
model24 <- glm(Grade2~Ethnicity*Age+Gender+Platform+Attendance,data = decDataDF2)
model25 <- glm(Grade2~Ethnicity+Age+Gender+Platform+Attendance,data = decDataDF2)

#testing models for the best fit model.
anova(model21,model22, test = "Chisq") #P-value = .01446, complex model fits better, model21
anova(model21,model23, test = "Chisq") #P-value = 2.2e-16, complex model fits better, model21
anova(model21,model24, test = "Chisq") #P-value = 2.2e-16, copmlex model fits better, model21
anova(model21,model25, test = "Chisq") #P-value = 2.2e-16, complex model fits better, model21

#Model21 proved to fit the data the best.
#variables shown here:
summary(model21)

#after seeing which variables were most significant, I went back and added these models based on..
#..which I thought would test well. However, none proved to fit better than the saturated model(model21).
model26 <- glm(Grade2~Age+Gender+Platform+Attendance,data = decDataDF2)
model27 <- glm(Grade2~Age+Gender*Platform*Attendance,data = decDataDF2)
model28 <- glm(Grade2~Age+Gender*Platform+Attendance,data = decDataDF2)

#RESULTS#

##highly significant variables, over 99% confidence:
#Asian males performed better than Asian Females
#Asian Males performed better taking an online class as opposed to a face to face class.

##significant variables, over 95% confidence:
#Hispanics performed better than other ethnicities
#Older students did not perform as well as younger students.
#Attending class improved grades for Asian Students, even more so for female Asian than Male Asians.
#Asian females struggled in online classes.
#Older Black students performed worse taking classes Online as opposed to face to face.
#Older Black male students performed better taking online classes than older black females.