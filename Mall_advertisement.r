
#1)  Store this data in a data frame called mall

mall <- read.csv("Mall_Customers.csv")

attach(mall)
#a)	Investigate data by using head(), summary() and class() functions.

head(mall)
summary(mall)

class(mall)

class(CustomerID)
class(Genre)
class(Age)
class(Annual.Income..k..)
class(Spending.Score..1.100.)


#b)	Create a boxplot of Spending score by Genre (male, female). Remember , 
#boxplot requires one of your variables to be a categorical variable (factor), 
#thus you need to do the necessary conversion. This conversion is required for 
#every categorical variable you will create in the following steps when new 
#variables are derived.

Genre <- as.factor(Genre) ; is.factor(Genre)
boxplot(Spending.Score..1.100. ~ Genre, main="Spending score by Genre", xlab="Genre", ylab="Spending Score")
#The female genre has a higher spending score than male

#c)	Create a new variable called  age_group which will divide customers into 3 
#different age groups based on their age. People younger than 30 (<30) will 
#belong to age_group 1, older than 30 (>=30) and younger than 50 will belong 
#age_group 2, and older than 50 (>=50) will belong group3. Add this variable 
#into your data frame

age_group_character <- ifelse(Age<30,"1", ifelse(Age>=50,"3","2"));
age_group <- factor(age_group_character); is.factor(age_group)
mall <- data.frame(mall,age_group); head(mall)


#d)	Create another new variable called high_spender which will indicate if a 
#person has spending score greater than or equal to 80 or not. Add this 
#variable to the mall data frame.

high_spender_character <- ifelse(Spending.Score..1.100.>=80,"Yes","No");
high_spender <- factor(high_spender_character); is.factor(high_spender)
mall <- data.frame(mall, high_spender); head(mall)


#Question 3) Build a logistic regression model to predict whether a person is 
#a high spender based on their age_group, Genre, and Annual income. 

high_spender_logistic <- glm(high_spender~age_group+Genre+Annual.Income..k..,data=mall,family="binomial")

high_spender_logistic

summary(high_spender_logistic)


#according to the Coefficients table:
#Being on age_group 2 decreases the log(odds) of being a high spender by -0.443801, for the same Annual Income and Genre
#Being on age_group 3 decreases the log(odds) of being a high spender by -17.311702,for the same Annual Income and Genre
#Being genre male increases the log(odds) of being a high spender by 0.139617, for the same Annual Income and age group
#Increasing Annual Income by 1 unit, change the log(odds) of being a high spender by 0.012921, for the same Genre and age_group

#the age_group2 and age_group3 are less likely to be a high spender than the age_group 1.
#The genre Male is correlated with being a high spend



#Question 4) 
#a)	Predict probability of being high spender using the model you fitted in 
#Question 3, assuming a cutoff probability of 0.5 (if probability >=0.5 
#classify as high_spender). Create the confusion matrix. Comment on the accuracy
#on your model using three different metrics.

predicted_high_spender_prob<-predict(high_spender_logistic,data=mall, type="response")
head(predicted_high_spender_prob)

predicted_high_spender_05<-ifelse(predicted_high_spender_prob>=0.5,"Yes","No")
head(predicted_high_spender_05)

#Confusion Matrix:
table(high_spender, predicted_high_spender_05)
?table

#              predicted_high_spender_05
#high_spender  No
#          No  170
#         Yes  30

#Accuracy metrics:
misclassified_05 <- (30 + 0)/200 *100 ; misclassified_05 #15% of predicted were misclassified
#or
mean(high_spender!=predicted_high_spender_05) #fraction of data that is not correctly predicted
# 15% of predictions were missclassified.

# False positive rate: wrongly predicted as yes
false_positive_rate = 0 ; false_positive_rate
#0% of negative were predicted as positive

#False negative rate= % Of the true Yes's.
false_negative_rate <- 30 / 30 *100 ; false_negative_rate
#100% of positive were predicted as negative



#b)	Predict probability of being high spender using the model you fitted in 
#Question 3, assuming a cutoff probability of 0.3 (if probability >=0.3 classify
#as high_spender). Create the confusion matrix. Comment on the accuracy on your 
#model using three different metrics.

predicted_high_spender_03<-ifelse(predicted_high_spender_prob>=0.3,"Yes","No")

#Confusion matrix:
table(high_spender, predicted_high_spender_03)
#              predicted_high_spender_03
#high_spender  No  Yes
#          No  165   5
#         Yes  28    2

#Accuracy metrics:

##Misclassification:
misclassified_03 = (5+28)/200 * 100 ; misclassified_03
#or
mean(high_spender!=predicted_high_spender_03) 
# 16.5% of prediction were not correctly predicted.

## False positive rate: wrongly predicted as yes
false_positive_rate = 5 / (165+5) *100 ; false_positive_rate
#2.94% of negative were predicted as positive

##False negative rate= % Of the true Yes's.
false_negative_rate <- 28 / (28+2) *100 ; false_negative_rate
#93.3% of positive (high spender) were predicted as negative (non high spender)




#c)	Compare the two approaches from part 4a and 4b. Suppose you are planning to 
#send separate targeted advertisements to the two groups of customers based on 
#their “predicted class”. Which prediction cutoff probability would you prefer, 
#one in 4.a or 4.b? Why? Explain your reasoning and its potential impact on the 
#company.

#Considering that the company wants to send separate advertisements to both high spenders and
#no high spenders, and that high spender is more costly than low spender,
#it would be more beneficial to choose a prediction cutoff probability of 0.3 (4.b).
#Even though the misclassification rate is higher with the 0.3 cutoff, it has a
#lower false negative rate than the 0.5 cutoff.
#That is, the company want to send the right advertisement to high spenders, so they would not
#lose those high spender customers, if they advertised only for non high spenders.

#On the other hand, if the company chose 0.5, its target customer would be mainly the non high spender customers,
#because this prediction 100% misclassifies high spenders as non high spenders.
#The costs of 0.3 misclassification would be losing a few low spender costumers, which
#would be less costly than losing all of high spender costumers.


detach(mall)
