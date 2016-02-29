# Reading in data
check <- read.table("~/Documents/Data/TrainData.txt",sep="|", nrow=5, header=T)
#Problem with escape charachter "\n"
# Read in all data with dummy separator
check2 <- read.table("~/Documents/Data/TrainData.txt",sep="~", header=F)
train_vect <- as.character(check2[,1])
class(train_vect)

#Check that presence of \n is random not systemtatic
bad <- grep("\n", train_vect)
plot(bad) #Seems to be random

#Split strings based on line break and piping
list <- strsplit(train_vect, "\n")
tmp <- unlist(list)
list2 <- lapply(tmp, function(x) strsplit(x, "\\|"))

#Check that the lengths are all the same - can loop over random elements or the whole list (this takes a long time)
# Random samples - seems to all be the correct length, no extra delimiters
lengths <- NULL
for (i in 1:100){
	lengths <- c(lengths, length(list2[[i]][[1]]))
}
table(lengths)

#Unlist the list of vectors and convert to a matrix with 31 columns
#length(vector)/31 is an even number
vector <- unlist(list2)
df <- matrix(vector, ncol=31, byrow=T)
data <- as.data.frame(df[2:nrow(df),])
names(data) <- df[1,]

#Save to access later
saveRDS(data, "~/Documents/Data/cleaned_train.rds")


## Clean test data - could make this into a function later to avoid re-writing
check2 <- read.table("~/Documents/Data/TestData.txt",sep="~", header=F)
test_vect <- as.character(check2[,1])
class(test_vect)
bad <- grep("\n", test_vect)
plot(bad) #Seems to be "random/stratified" not systematic

list <- strsplit(test_vect, "\n")
tmp <- unlist(list)
list2 <- lapply(tmp, function(x) strsplit(x, "\\|"))
lengths <- NULL
for (i in 1:length(list2)){
	lengths <- c(lengths, length(list2[[i]][[1]]))
}
vector <- unlist(list2)
df <- matrix(vector, ncol=30, byrow=T)
data <- as.data.frame(df[2:nrow(df),])
names(data) <- names(check)
summary(data[,3])
saveRDS(data, "~/Documents/Data/cleaned_test.rds")
test <- readRDS("~/Documents/Data/cleaned_test.rds")

### Read in cleaned data

data <- readRDS("~/Documents/Data/cleaned_train.rds")

#Separate out response variables
response <- data[, c(1:7, 9,10)]

train <- data[, c(8, 11:31)]
test <- test[, c(7, 10:30)]

#Check quality of training variables

summary(train)
# Initial_Pos_Y and Plate_Y are not relevant 

train <- data[, c(8, 11:14, 16:24, 26:31)]
out <- apply(train[,4:20], 2, as.numeric)
train[,4:20] <- out

test <- test[, c(7, 10:13, 15:23, 25:30)]
out <- apply(test[,4:20], 2, as.numeric)
test[,4:20] <- out


summary(test) # Doesn't seem to be any obvious outliers

par(mfrow=c(4,4))
for ( i in 4:19){
	hist(train[,i],
	main=names(train)[i])
}

#Also check the last one
par(mfrow=c(4,4))
for ( i in 20{
	hist(train[,i],
	main=names(train)[i])
}

# Distributions all look good - note bimodal dist of Init_Pos_X and Init_Vel_X

library(ROCR)


# Create model matrix

#Model 1
x_MM_train <- model.matrix(~. -1, data=train)
Y <- response$swingingstrike
glm_out <- glm(Y ~ x_MM_train, family="binomial")

preds <- predict(glm_out, as.data.frame(x_MM_train), type="response")

#Get roc
prediction_train <- prediction(preds, Y)

auc <- performance(prediction_train, measure="auc")
plot(performance(prediction_train, measure="tpr", x.measure="fpr"))

# 
x_MM_2 <- model.matrix(~. -1 + pitchtype:Init_Vel +
	pitchtype:Init_Pos_X +
	pitchtype:Init_Pos_Z +
	pitchtype:Init_Vel_X +
	pitchtype:Init_Vel_Y +
	pitchtype:Init_Vel_Z +
	pitchtype:Init_Accel_X +
	pitchtype:Init_Accel_Y +
	pitchtype:Init_Accel_Z +
	pitchtype:Plate_Vel +
	pitchtype:Plate_X +
	pitchtype:Plate_Z +
	pitchtype:Break_X +
	pitchtype:Break_Z +
	pitchtype:SZ_Top +
	pitchtype:SZ_Bottom +
	pitchtype:SpinRate, data=train)

glm_out_2 <- glm(Y ~ x_MM_2, family="binomial")

# re-use same x_MM_2 variable name because this is required by predict()
preds_2 <- predict(glm_out_2, as.data.frame(x_MM_2), type="response")

prediction_train <- prediction(preds_2, Y)

auc <- performance(prediction_train, measure="auc") #AUC is a bit better when interactions are included, although may be overfitted
plot(performance(prediction_train, measure="tpr", x.measure="fpr"))

# Calculate predictions for test sample
x_MM_2 <- model.matrix(~. -1 + pitchtype:Init_Vel +
	pitchtype:Init_Pos_X +
	pitchtype:Init_Pos_Z +
	pitchtype:Init_Vel_X +
	pitchtype:Init_Vel_Y +
	pitchtype:Init_Vel_Z +
	pitchtype:Init_Accel_X +
	pitchtype:Init_Accel_Y +
	pitchtype:Init_Accel_Z +
	pitchtype:Plate_Vel +
	pitchtype:Plate_X +
	pitchtype:Plate_Z +
	pitchtype:Break_X +
	pitchtype:Break_Z +
	pitchtype:SZ_Top +
	pitchtype:SZ_Bottom +
	pitchtype:SpinRate, data=test)

preds_2 <- predict(glm_out_2, as.data.frame(x_MM_2), type="response")
pred_out <- data.frame(pitchid=test[,1], Swinging_Strike_Prediction=preds_2)

write.csv(pred_out, "~/Documents/Swinging_strike_test.csv")

#Look at scale/size of coefficients 
coefs <- exp(coefficients(glm_out_2))-1

ordered <- coefs[order(coefs)]
barplot(ordered[1:108]) # Big outliers for some coefficients. Note that pitchtype SC stands out but v. small sample size

# standardize coefficients
sds <- apply(x_MM_2, 2, sd) # Make sure to use the train version of the x_MM_2 matrix
stand <- coefs[2:length(coefs)]/sds
std2 <- stand[order(stand)]
par(mar=c(10,5,2,5))
barplot(std2[c(1:12, 85:(length(std2)-5))], las=2, cex.names=0.5)

#Create a quick summary table 
one <- table(swingingstrikes$pitchtype)
two <- table(notswingingstrikes$pitchtype)
one/(one+two)
prop.table(one+two)*100

# Create density plots 

library(reshape2) # For melt function

notswingingstrikes <- train[!ss,]
swingingstrikes <- train[ss,]

# Note this function takes a while to run - I deliberately hard-coded the variable names within the function
# since I wanted to go through the results manually. In future I would make the variable name an argument option
plot_function <- function(pt){
	swingingstrikes <- train[ss & pt,]		
	notswingingstrikes <- train[!ss & pt,]	
	x1 <- swingingstrikes$Init_Pos_X
	y1 <- swingingstrikes$Init_Pos_Z
	x2 <- notswingingstrikes$Init_Pos_X
	y2 <- notswingingstrikes$Init_Pos_Z
	
	
	xrng = range(train$Init_Pos_X)
	yrng = range(train$Init_Pos_Z)
	
	# Calculate the 2d density estimate over the common range
	d1 = kde2d(x1, y1, lims=c(xrng, yrng), n=100)
	d2 = kde2d(x2, y2, lims=c(xrng, yrng), n=100)
	
	# Confirm that the grid points for each density estimate are identical
	identical(d1$x, d2$x) # TRUE
	identical(d1$y, d2$y) # TRUE
	
	# Calculate the difference between the 2d density estimates
	diff12 = d1 
	diff12$z = d2$z - d1$z
	
	## Melt data into long format
	# First, add row and column names (x and y grid values) to the z-value matrix
	rownames(diff12$z) = diff12$x
	colnames(diff12$z) = diff12$y
	
	# Now melt it to long format
	diff12.m = melt(diff12$z, id.var=rownames(diff12))
	names(diff12.m) = c("Init_Pos_X","Init_Pos_Z","z")
	
	# Plot difference between geyser2 and geyser1 density
	ggplot(diff12.m, aes(Init_Pos_X, Init_Pos_Z, z=z, fill=z)) +
	  geom_tile() +
	  stat_contour(aes(colour=..level..), binwidth=0.001) +
	  scale_fill_gradient2(high="steelblue", midpoint=0) +
	  scale_colour_gradient2(high="steelblue", midpoint=0) +
	  coord_cartesian(xlim=xrng, ylim=yrng) +
	  guides(colour=FALSE) + 
	  scale_x_discrete(expand = c(0, 0)) +
	   scale_y_discrete(expand = c(0, 0)) + 
	   labs(x = "", y = "")

}


# I plotted each of the results separately, but in future I would loop through all the various options
# Computational speed is a limitation on this computer.

pt <- train$pitchtype=="SC"
pdf("~/Documents/SC_init_vel_xz.pdf")
plot_function(pt)
dev.off()
pt <- train$pitchtype=="FB"
pdf("~/Documents/FB_init_vel_xz.pdf")
plot_function(pt)
dev.off()
pt <- train$pitchtype=="CB"
pdf("~/Documents/CB_init_vel_xz.pdf")
plot_function(pt)
dev.off()
pt <- train$pitchtype=="SL"
pdf("~/Documents/SL_init_vel_xz.pdf")
plot_function(pt)
dev.off()
pt <- train$pitchtype=="CH"
pdf("~/Documents/CH_init_vel_xz.pdf")
plot_function(pt)
dev.off()
pt <- train$pitchtype=="KN"
pdf("~/Documents/KN_init_vel_xz.pdf")
plot_function(pt)
dev.off()

## Summarize pitcher data

library(plyr)
pt <- "FB"
summarize_pitches <- function(pt){
	batch <- data[data$pitchtype==pt,]
	batch$Init_Vel <- as.numeric(batch$Init_Vel)
	batch$Break_X <- as.numeric(batch$Break_X)
	batch$Break_Z <- as.numeric(batch$Break_Z)
	summary <- ddply(batch, "pitcherid", function(x) c(pitches_thrown=nrow(x), avg_init_vel=mean(x$Init_Vel),
						avg_x_break=mean(x$Break_X), avg_z_break=mean(x$Break_Z)))
	summary <- summary[summary$pitches_thrown >= 50,]
	summary <- summary[rev(order(summary$avg_init_vel)),]
	data.frame(pitchtype=pt, summary[1:5,])
}

result <- rbind(
	summarize_pitches("FB"), 
	summarize_pitches("CB"),
	summarize_pitches("SL"),
	summarize_pitches("CH")
	)

pitcher_name <- data$pitcher_name[match(result$pitcherid, data$pitcherid)]
result$pitcher_name <- pitcher_name


write.csv(result, "~/Documents/Pitcher_summary.csv")