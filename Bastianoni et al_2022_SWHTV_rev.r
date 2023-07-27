library(sampler)
library(bootstrap)
library(ggplot2)
library(smotefamily)
library(DMwR2)
library(UBL)
library(viridis)

#setting bigger plots
options(repr.plot.width=12, repr.plot.height=8)

# Setting up the Giovannelli Lab plot theme
theme_glab <- function(base_size = 11,
                    base_family = "",
                    base_line_size = base_size / 180,
                    base_rect_size = base_size / 180) {
   
    font <- "Helvetica" #assign font family up front
   
    theme_bw(base_size = base_size,
                base_family = base_family,
                base_line_size = base_line_size) %+replace%
    theme(
        legend.background =  element_blank(),
        legend.title =       element_text(color = rgb(100, 100, 100, maxColorValue = 255),
                                          size = rel(0.65),
                                         hjust = 0),
        legend.text =        element_text(color = rgb(100, 100, 100, maxColorValue = 255),
                                          size = rel(0.65)),
        legend.key.size =    unit(0.8, "lines"),
     
      plot.title = element_text(
        color = rgb(100, 100, 100, maxColorValue = 255),
        hjust = 0),
       
      axis.title = element_text(
        color = rgb(100, 100, 100, maxColorValue = 255),
        size = rel(0.65)),
      axis.text = element_text(
        color = rgb(100, 100, 100, maxColorValue = 255),
        size = rel(0.65)),
       
      plot.caption = element_text(
        color = rgb(100, 100, 100, maxColorValue = 255),
        size = rel(0.35),
        hjust = 1),
       
      panel.grid.major = element_blank(),  
      panel.grid.minor = element_blank(),  
      panel.border = element_rect(fill = NA, colour = rgb(100, 100, 100, maxColorValue = 255)),

     
      complete = TRUE
    )
}

#step 1: open the flux dataset
options(digits = 10)
sht <- read.csv("shtv3.csv")

# ## Approach 1: Sample increase by bootstrapping 1,000 times 

# ***Packages needed:*** boostrap,sampler

# ***What we did:*** We renamed our dataset as **x** and included in a for-loop code, that allowed the sampling of the dataset followed by its bootstrapping 1,000 times. 
# This dataset was then bootstrapped 1,000 times using the function *bootstrap(x,1000,theta)*. The final result was a dataset made of 50,000 points randomly selected based on the original distribution.
# ***What we used as a starting dataset:*** *sht*
# ***What we got***: A final dataset called: *"flux1_df"* saved as a .csv file that we used for downstream operations

sht

flux<-sht[,3]
flux

set.seed(12)
x <- flux
theta <- function(x){sample(flux,50,rep=TRUE)}
results_shtv1 <- bootstrap(x,1000,theta)

###### Command lines to obtain the flux1_df.csv file and description of the csv file columns ######  
# For constructing the .csv file we select the column called "thetastar" writing ***results_shtv1$thetastar*** 
# since in that column we find the fluxes created after the bootstrapping of the function created, 
# that is,the sampled flux data. We also used the functions **unlist()** and **as.data.frame()** to aid 
# the organization of the file. 
# The *.csv file created is composed of two columns:
# the first one corresponds to the sample number (1;2;3....; 50,000)
# the column under the heading "flux1_matrix" represents all the fluxes calculated. 
# With this file we can generate plots and calculate the descriptive statistics of the population (mean, standard deviation, confidence interval). 

write.csv(results_shtv1$thetastar, "flux1.csv") 
flux1<-read.csv("flux1.csv")
flux1b<-flux1[,2:1001]
#to join all columns into 1
flux1_matrix=unlist(flux1b)
flux1_df=as.data.frame(flux1_matrix)
write.csv(flux1_df,"flux1_df.csv") 

# ## Approach 2: Jack-knife resampling and bootstrap of the dataset

# ***Packages needed:*** boostrap,sampler

# ***What we did:*** Using a for-loop code, similar to the one used in "Approach 1", and thanks to the function *rsamp(x, as.data.frame(flux2), n=8, replace=TRUE)* 
# we created a new dataset randomly removing 2 samples from the original file, and bootstrapping it 1,000 times, through the code *bootstrap(x,1000,theta)*. 
# The final result was a dataset, called **flux 2** made of 8,000 points randomly selected based on the original distribution of the dataset. 

# ***What we used as a starting dataset:*** *sht2*, this is always the file "shtv3.csv" but we renamed it for an easy differentiation from the first file we used. 


# ***What we got***: A final dataset called: *"flux2_df"* saved as a .csv file that we used for downstream operations

options(digits = 15) #With options(digits = 10, we changed the number of maximum digits to 10
sht2 <- read.csv("shtv3.csv")

flux2<-sht2[,3] #I removed the site column in order to obtain a cleaner file at the end

flux2

set.seed(12)

x <- flux2
theta <- function(x){rsamp(as.data.frame(flux2),n=8,rep=TRUE)}
flux2 <- bootstrap(x,1000,theta)

##### Command lines to obtain  the flux2_df.csv file and description of the csv file columns ###### 
# #For constructing the .csv file we select the column called "thetastar" writing ***flux2$thetastar*** 
# since in that column we find the fluxes created after the bootstrapping of the function created, 
# that is,the sampled flux data. 
# We also used the functions **unlist()** and **as.data.frame()** to aid the organization of the file. 
# The *.csv file created is composed of two columns: 
# the first one corresponds to the sample number (1;2;3....; 8,000) 
# the cloumn under the heading "flux2_matrix" reprents all the fluxes calculated. 
# With this file we can generate plots and calculate the descriptive statistics of the population (mean, standard deviation, confidence interval). 

write.csv(flux2$thetastar, "flux2.csv")
flux2 <- read.csv("flux2.csv")
flux2_matrix=unlist(flux2[,2:1001])
flux2_df=as.data.frame(flux2_matrix)
write.csv(flux2_df,"flux2_df.csv")

## Approach 3: Creation of a balanced dataset from an artificial one applying SMOTE


#***Packages needed***:UBL, DMwR2,bootstrap, sampler

# ***What we did:***  First we ran the for-loop function using our original dataset ("sht") and setting the temperature as a majority class. 
# The **SmoteClassif()** function takes in the first two arguments a formula (used to know which is the target variable) and the imbalanced data. 
# The parameter "C.per" of the function controls the way the under- and over-sampling takes place. 
# In this case, temperature was the majority class and we used a balanced sampling. The output obtained was named "results_shtv1", and saved as "smote_df.csv". 
# We then obtained a new dataset, running the function rnorm (), and setting 50 as a sample size, and the mean and standard deviation of the "smote_df.csv" dataset. 
# This balanced dataset was named "shtv5" and used to run again the SmoteClassif() function using as majority class the type of value (wheter estimated, E, or reported, R), 
# and defining an over-sampling of the reported values (R=6) and that the estimated class (E=1) remained unchanged. 
# The final dataset obtained was named "results_smote" and saved as  "smotev2_df.csv"

# ***What we used as a starting dataset:*** *sht*

# ***What we got***: A final dataset named *"smotev2_df"* saved as a .csv file that we used for downstram operations


set.seed(12)

x<-sht

#command to create the balanced dataset using temperature as a majority class
theta <- function(x){SmoteClassif(Tclass ~., data, C.perc = "balance")}
results_shtv1 <- bootstrap(x,1000,theta)

flux_smote_clean=select(results_shtv1, -starts_with("Tclass")) #to remove the column Tclass
flux_smote[,3:1002] #to select just the columns of interest
flux_smote=unlist(flux_smote[,3:1002]) #to traspose all the columns and obtain a list of values
as.data.frame(flux_smote)

#to obtain the csv needed for the downstream steps
write.csv(results_shtv1$thetastar, "smote_df.csv") 

smote_df<-read.csv("/home/alessia/Desktop/Postdoc_Giovannelli_Lab/SHTV/Bootstrap_results/Results_100122/smote_df.csv")
summary(smote_df)

#average and std dev obtained after running a balanced SMOTE with Temp as a classifying variable
#negative values were replaced with the min flux value =0.198
norm_dist=rnorm(50,273863.7767,699002.7713) 
norm_df <- as.data.frame(norm_dist)
norm_df$norm_dist <- ifelse(norm_dist < 0, "0.198", norm_df$norm_dist)
norm_df

write.csv(norm_df, "normdist.csv") 
#command to write the data into a .csv file

shtv5 <- read.csv("shtv5.csv", sep="\t") 
#file with the columns and fluxes generated pasted from normdist.csv

data <- shtv5[,-1]
data <- as.data.frame(data)
data$Type<-as.factor(data$Type) 
#to transform Type into a factor

#to create the SMOTE'd data from the new dataset
newData <- SmoteClassif(Type ~., data,  C.perc = list(E = 1,R = 6))

#to log transform the dataset
logFluxsmote <- log(newData$Flux,10)
log_smote <-as.data.frame(logFluxsmote) 

set.seed(12)
x <- data
#create a function to obtain the balanced dataset 
#where E stands for Estimated and R for reported 
theta <- function(x){SmoteClassif(Type ~., data,  C.perc = list(E = 1,R = 6))} 
results_smote <- bootstrap(x,1000,theta)

results_smote

##### Command lines to obtain  the smotev2_df.csv file and description of the csv file columns ###### 
# For constructing the .csv file we select the column called "thetastar" writing ***results_smote$thetastar*** 
# since in that column we find the fluxes created after the bootstrapping of the function created, that is,the sampled flux data. 
# We also used the functions **unlist()** and **as.data.frame()** to aid the organization of the file. 
# The *.csv file created is composed of two columns:
# the first one corresponds to the sample number (1;2;3....; 109,0000) 
# the column under the heading "smotev2_matrix" represents the fluxes calculated. 
# With this file we can generate plots and calculate the descriptive statistics of the population (mean, standard deviation, confidence interval). 

smote3 <-as.data.frame(results_smote$thetastar)
smote_flux <- select(smote3, -starts_with("Temp")) #to remove the column of the temperature class
smote_flux <- select(smote_flux, -starts_with("Type")) #to remove the column of the type of value (estimated or reported)
write.csv(smote_flux2, "smotev2.csv")
smotev2_df <-as.data.frame(smotev2_matrix)
write.csv(smotev2_df, "smotev2_df.csv")


