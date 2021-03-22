# ---
# title: "MSc Bioinformatics Coursework 2020/21"
# author: "Sophie Wilson"
# date: "22/10/2020"
# output: html_document
# ---

library(ggplot2)

  #     ''' Exercise 2: Extend the code above to incorporate the effect of the snakes and ladders. Simulate the play for a single player only. [4 MARKS] ''''

num_simulations <- 200
total_turns_taken <- rep(0, num_simulations)

for (x in 1:num_simulations){
  max_turns <- 500 # First we set the maximum turns a 'player' can have to 50. So the For loop will run 50 times.
  num_turns <- 0
  position <- 0 # Next we set a starting position on the board.
  max_position <- 100 # Then set an upper bound for movement to keep the simulation on the board. 
  n_sides_die <- 6 # Make a variable setting the possible dice roll outcomes to 6, as it is a six sided dice.
  
  ladder_df <- data.frame(start=c(1,4,9,21,28,36,51,71,80), end=c(8,14,31,42,84,44,67,91,100))
  snakes_df <- data.frame(start=c(98,95,93,87,64,62,56,49,47,16), end=c(78,75,73,24,60,19,53,11,26,6))
  
  # This for loop is simulating player movement through dice rolling. It loops through max_turns and changes the variables we set above.
  
  for ( turn in 1:max_turns ){
    num_turns = num_turns + 1
    die_roll <- sample.int(n_sides_die, 1) # Making a variable to store the results of the die roll. This function takes a sample containing one vector of the variable n_sides_die, which is what die_roll is set to. This results in a random number from 1-6 to simulate die rolls. 
    position <-position + die_roll # Setting a new position, we do position + die_roll to ensure that you don't go backwards.
    print(position)
    
    
    
    check_ladder <- grep(paste("^", position, "$", sep=""), ladder_df$start)
    if (length(check_ladder) > 0){
      cat("ladder index", check_ladder, "\n")
      cat("Up the ladder! from ", position, " to")
      
      position = ladder_df$end[check_ladder[1]]
      print(position)
    }
    
    check_snakes <-  grep(paste("^", position, "$", sep=""), snakes_df$start)
    if (length(check_snakes) > 0){
      cat("snake index", check_snakes, "\n")
      cat("Down the snake! from ", position, " to")
      
      position = snakes_df$end[check_snakes[1]]
      print(position)
    }
    
    if ( position >= max_position ){
      break # Exits the for loop
      # This if statement specifies an end point if the end of the board (shown by the max_position variable) is reached before the max_turns.
      
    }
  }
  cat('it took you ', num_turns, 'turns!')
  total_turns_taken[x] = num_turns

}

cat('average turns', format(round(mean(total_turns_taken), 2), nsmall = 2))

var(total_turns_taken)
sd(total_turns_taken)

plot_data <- data.frame(total_turns_taken)

ggplot(plot_data, aes(x= total_turns_taken)) +
  geom_histogram(binwidth=3, fill='red', alpha=0.75, colour='grey') +
  labs(x='Number of turns taken', y= 'Frequency') +
  ggtitle(~underline('Variance in Snakes and Ladders turns', x='Number of turns taken', y= 'Frequency')) +
  theme(plot.title = element_text(hjust=0.5)) 



#     ''''' Expression Data Analysis '''''

getwd()
setwd('C:/Users/Mischa/Documents/Uni Masters/Module 1 Maths/Coursework')

# -------------------Part 1---------------------
library(tidyverse)

load("assess_data_20.Rdata") # Loading in the data

pca_x <- t(log(Y + 1)) # Transposing the matrix Y, you add +1 to all values to ensure there are no 0's in the dataset, it makes minimal difference as the dataset is large. You use log to scale the data so it is all on a similar scale. 
pca_res1 <- prcomp(pca_x, center = TRUE, scale = TRUE) # Performs PCA analysis on matrix pca_x, values are centred around 0 and scaled to have unit variance.
pc_df1 <- data.frame(pca_res1$x, tissue = patient_data$tissue, 
                     patient = patient_data$patient) # Converting the values of each sample in terms of principle components of pca_res1 into a data frame so that we can plot it. Also assigning labels to tissue and patient columns. 


# plotting the PCA to get a sense of the data # Plotting pc_dft1, with PC1 on the x axis and PC2 on the y axis. Setting the shape by tissue and the colour by patient. So there should be two different shapes (as there is only Tumour and Normal tissues), and 15 different colours. Plotting as a point plot with the points of size 6. 
ggplot(pc_df1, aes(x = PC1, y = PC2, shape = tissue, col = patient)) +
  geom_point(size = 6)

# plotting the pca with an ellipse to detect outliers, from this you can see patient 14, patient 1 and patient 3 are outliers although patient 3 is quite close to the group. 
ggplot(pc_df1, aes(x = PC1, y = PC2)) +
  geom_point(size = 2) + stat_ellipse() + geom_text(label = pc_df1$patient, nudge_x = 20) 

# Because patient 3 is very close to the ellipse I will run some more descriptive graphics to decide whether i should remove them. Because the dataset is so small i am hesitant to remove too many patients
biplot(pca_res1, main = 'title')
# The biplot shows that sample 14 and 16 are clear outliers, but doesn't tell us much about patient 3.

# The boxplot doesnt show labels for the outliers, but it does show there there are only two outliers. Because of this I will keep patient 3 for further analysis
boxplot(pca_res1$sdev)
Y_adjusted <- Y[,-c(1,16,14,29)]
pca_adjusted <- t(log(Y + 1)) # transposing and the new data set
pca_adjusted <- pca_adjusted[-c(1,16,14,29),]
patient_data_adjusted <- patient_data[-c(1,16,14,29),] # removing outliers
pca_res2 <- prcomp(pca_adjusted, center = TRUE, scale = TRUE) # performing pca on thew new data
pc_df2 <- data.frame(pca_res2$x, tissue = patient_data_adjusted$tissue, 
                     patient = patient_data_adjusted$patient) # making it into a data frame

# biplot(pca_res2) # using a biplot to make sure the outliers have been removed patient 1 as an outlier? 13 patients 13 patients left
# 
ggplot(pc_df2, aes(x = PC1, y = PC2, shape = tissue, col = patient)) +
   geom_point(size = 6) # checking the new plot. 

# You can see that with the two large outliers removed, patient 3 now fits nicely into the data and so doesn't need to be removed.
ggplot(pc_df2, aes(x = PC1, y = PC2)) +
  geom_point(size = 2) + stat_ellipse() + geom_text(label = pc_df2$patient, nudge_x = 20) 
# -------------------Part 2---------------------

boxplot(pca_res2$sdev)
library(MASS) # Import the MASS library
idx <- nrow(Y_adjusted) # Setting the index (i dont think this is necessary)
c_cl <- 1:nrow(patient_data_adjusted) # Setting a range to get from the dataset
x <- patient_data_adjusted$tissue[c_cl] # Getting the first 20 values in column 'tissue' in the patient dataset
z <- patient_data_adjusted$patient[c_cl] # Setting the variable z to values 1-20 in column patient in patient_data. 

#gene_labels <- rownames(Y_adjusted)
p_val <- rep(0, idx)
degreesfreedom <- rep(0, idx)
deviance <- rep(0, idx)

for (m in 1:idx) {
  #gene <- gene_labels[m]
  tmp <- data.frame(y = Y_adjusted[m, c_cl], x = x, z = z, lib_size = colSums(Y_adjusted[, c_cl])) # making a dataframe. y column = the idx row or the columns 1:20 in matrix Y. x column = x variable, z column = z variable. lib_size = the Sum of rows 1-20 in matrix Y
  out <- glm(y ~ z + lib_size, data = tmp, family = 'quasipoisson') # Fitting a generalised linear model. y is modelled by the linear predictor specified by x + z + lib_size. This uses the data in tmp data frame. It uses Poisson error distribution. possion can work for count data of single genes but its not the best difference between and variance for all genes is very large, cant use nb for each gene. quasi possion worked in a loop
  p <- summary(out)$coefficients[2,4]
  p_val[m] <- p
  df <- summary(out)$df.residual
  degreesfreedom[m] <- df
  dev <- summary(out)$deviance
  deviance[m] <- dev
  #sig_labels <- gene
}

for (m in 1:1) {
  tmp <- data.frame(y = Y_adjusted[m, c_cl], x = x, z = z, lib_size = colSums(Y_adjusted[, c_cl])) 
  out1 <- glm(y ~ x + z + lib_size, data = tmp, family = "quasipoisson") # Run models to compare
  out2 <- glm(y ~ z + lib_size, data = tmp, family = 'quasipoisson') 
  
  withx <- summary(out1)$coefficients # Extract the P values for both
  print(summary(out2))
}


overdispersion <- data.frame(deviance = deviance, degreesfreedom = degreesfreedom, overdispersion = deviance>degreesfreedom)
table(overdispersion$overdispersion)['TRUE']

sig = 0.05
psig = -log10(sig)

p_df = data.frame(p_values = p_val, idx = 1:idx)
ggplot(p_df, aes(x = idx, y= -log10(p_values)))+
  geom_point(size = 1)+
  geom_hline(yintercept = psig, col= "Red")+
  labs(y= "-log10 p-values", title = "p-values with tissue type covariate")

plot(-log10(p_val))





plot_df <- data.frame(cbind(p_val_plot, 1:idx))
out.res <- resid(out)
ggplot(plot_df, aes(x = V2, y = log10(p_val))) + geom_point()


#### TRYING TO LOOK AT A CHANGE IN DEVIANCE ##### 

pval <- c()
pval2 <- c()
pval3 <- c()
pval4 <- c()
pval5 <- c()
deviance1 <- c()
deviance2 <- c()
deviance3 <- c()
deviance4 <- c()
deviance5 <- c()
annova <- c()
for (m in 1:idx) {
  #gene <- gene_labels[m]
  tmp <- data.frame(y = Y_adjusted[m, c_cl], x = x, z = z, lib_size = colSums(Y_adjusted[, c_cl]))
  out <- glm(y ~ x + z + lib_size, data = tmp, family = "poisson") # 
  p_val[m] <- summary(out)$coefficients[2,4]
  deviance1[m] <- summary(out)$deviance
  
  out2 <- glm(y ~ x + z + lib_size, data = tmp, family ='quasipoisson')
  pval2[m] <- summary(out2)$coefficients[2,4]
  deviance2[m] <- summary(out2)$deviance
  
  out3 <- glm(y ~ x + lib_size, data = tmp, family ='quasipoisson')
  pval3[m] <- summary(out3)$coefficients[2,4]
  deviance3[m] <- summary(out3)$deviance
  
  out4 <- glm(y ~ z + lib_size, data = tmp, family ='quasipoisson')
  pval4[m] <- summary(out4)$coefficients[2,4]
  deviance4[m] <- summary(out4)$deviance
  
  out5 <- glm(y ~ lib_size, data = tmp, family ='quasipoisson')
  pval5[m] <- summary(out5)$coefficients[2,4]
  deviance5[m] <- summary(out5)$deviance
  
  modellist <- list(all = out2, x_only = out3, z_only = out4, libsize = out5)
}
av1 <- mean(deviance1)
av2 <- mean(deviance2)
av3 <- mean(deviance3)
av4 <- mean(deviance4)
av5 <- mean(deviance5)

Deviance_comparison <- data.frame(Model = c('All Covariates', 'Removing x', 'Removing z', 'Removing x and z'), Deviance = c(av2, av4, av3, av5), Deviance_change = c(0, av4-av2, av3-av2, av5-av2))

Deviance_comparison2 <- data.frame(Model = c('All Covariates', 'Removing x', 'Removing z', 'Removing x and z'), Deviance = c(mean(deviance2), mean(deviance4), mean(deviance3), mean(deviance5)), Deviance_change = c(0, mean(deviance4)-mean(deviance2), mean(deviance3)-mean(deviance2), mean(deviance5)-mean(deviance2)))
### From this you can see that z is the most important covariate in influencing the deviance of the model, x also has an impact but not as much. Removing z makes the model far less accurate

## THIS IS THE BIT WHERE I DO ANNOVA, ITS GOOD USE IT #####
annovapval <- c()
for (m in 1:idx) {
  tmp <- data.frame(y = Y_adjusted[m, c_cl], x = x, z = z, lib_size = colSums(Y_adjusted[, c_cl])) 
  
  withx <- glm(y ~ x + z + lib_size, data = tmp, family ='quasipoisson')
  withoutx <- glm(y ~ z + lib_size, data = tmp, family ='quasipoisson')

  ano <- anova(withx, withoutx, test = 'LRT')
  annovapval[m] <- ano[2]
}
annova_df <- data.frame(p_value = annovapval, gene = 1:idx)
print(sum(annova_df$p_value)/nrow(annova_df))

#### TESTING THE DATA ##### 
withxpredict <- data.frame(matrix(ncol = ncol(Y_adjusted), nrow = nrow(Y_adjusted))) 
rownames(withxpredict) <- rownames(Y_adjusted)
colnames(withxpredict) <- colnames(Y_adjusted)

noxpredict <- data.frame(matrix(ncol = ncol(Y_adjusted), nrow = nrow(Y_adjusted))) 
rownames(noxpredict) <- rownames(Y_adjusted)
colnames(noxpredict) <- colnames(Y_adjusted)

for (m in 1:idx){
  tmp <- data.frame(y = Y_adjusted[m, c_cl], x = x, z = z, lib_size = colSums(Y_adjusted[, c_cl])) 
  withx <- glm(y ~ x + z + lib_size, data = tmp, family ='quasipoisson')
  withoutx <- glm(y ~ z + lib_size, data = tmp, family ='quasipoisson')
  withxpredict[m,] <- predict(withx, test_data, type = "response")
  noxpredict[m,] <- predict(withoutx, tmp, type = "response")
}

compare_counts <- data.frame(with_x = colSums(withxpredict[,c_cl]), without_x = colSums(noxpredict[, c_cl]), actual = colSums(Y_adjusted[, c_cl]))

ggplot(compare_counts, aes(x = log(actual), y = log(without_x))) + geom_point()

predicted_data_rough <- rbind(with_x = withxpredict[2,], without_x = noxpredict[2,], actual = Y_adjusted[2,c_cl], patient_id = colnames(Y_adjusted))
predicted_data <- data.frame(t(predicted_data_rough))

errors_x <- data.frame(matrix(ncol = ncol(Y_adjusted), nrow = nrow(Y_adjusted))) 
rownames(errors_x) <- rownames(Y_adjusted)
colnames(errors_x) <- colnames(Y_adjusted)

errors_no_x <- data.frame(matrix(ncol = ncol(Y_adjusted), nrow = nrow(Y_adjusted))) 
rownames(errors_no_x) <- rownames(Y_adjusted)
colnames(errors_no_x) <- colnames(Y_adjusted)
### With X for loop
for (r in 1:idx){
  for (c in 1:26){
    prediction <- withxpredict[r,c]
    actual <- Y_adjusted[r,c]
    difference <- prediction - actual 
    errors_x[r,c] <- difference^2 # Difference squared between actual value and predicted, so that negative errors are accounted for.
  }
}
for (r in 1:idx){
  for (c in 1:26){
    prediction2 <- noxpredict[r,c]
    actual2 <- Y_adjusted[r,c]
    difference2 <- prediction2 - actual2 
    errors_no_x[r,c] <- difference2^2 # Difference squared between actual value and predicted, so that negative errors are accounted for.
  }
}
errors_x$sum <- rowSums(errors_x[,c_cl])
errors_x$mse <- errors_x$sum/nrow(errors_x)

errors_no_x$sum <- rowSums(errors_no_x[,c_cl])
errors_no_x$mse <- errors_no_x$sum/nrow(errors_no_x)

compare_errors <- data.frame(with_x = errors_x$mse, without_x = errors_no_x$mse, sum_x = errors_x$sum, sum_without_x = errors_no_x$sum)

ggplot(compare_errors, aes(x = log(sum_x), y = log(sum_without_x))) + geom_point() + labs(x= "log Mean Squared error for with x model", y = "Log mean Squared error for without x model", title = 'Log mean squared error comparisons for models with and without tissue covariate')
      






















mod_test<-anova(out,out2, test = 'Chisq')
lapply(out, summary)
devpois <- mean(deviance1)
devquasi <- mean(deviance1)
p_val_plot <- -log10(p_val)
plot(-log10(p_val))
plot(-log10(pval2))
