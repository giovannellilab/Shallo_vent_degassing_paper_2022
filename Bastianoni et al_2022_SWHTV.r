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
options(digits = 10)
sht <- read.csv("shtv3.csv")

sht

flux<-sht[,3]
flux

set.seed(12)

flux1=sample(flux,50,rep=TRUE)

flux1

set.seed(12)
x <- flux
theta <- function(x){sample(flux,50,rep=TRUE)}
results_shtv1 <- bootstrap(x,1000,theta)

write.csv(results_shtv1$thetastar, "flux1.csv")
flux1<-read.csv("flux1.csv")
flux1b<-flux1[,2:1001]
#to join all columns into 1
flux1_matrix=unlist(flux1b)
flux1_df=as.data.frame(flux1_matrix)
write.csv(flux1_df,"flux1_df.csv") 


options(digits = 15) #With options(digits = 10, we changed the number of maximum digits to 10
sht2 <- read.csv("shtv3.csv")

flux2<-sht2[,3] #I removed the site column in order to obtain a cleaner file at the end

flux2

set.seed(12)

x <- flux2
theta <- function(x){rsamp(as.data.frame(flux2),n=8,rep=TRUE)}
flux2 <- bootstrap(x,1000,theta)

write.csv(flux2$thetastar, "flux2.csv")
flux2 <- read.csv("flux2.csv")
flux2_matrix=unlist(flux2[,2:1001])
flux2_df=as.data.frame(flux2_matrix)
write.csv(flux2_df,"flux2_df.csv")

set.seed(12)

norm_dist=rnorm(50,273863.7767,699002.7713) #average and std dev obtained after running a balanced SMOTE with Temp as a classifying variable
norm_df <- as.data.frame(norm_dist)
norm_df$norm_dist <- ifelse(norm_dist < 0, "0.198", norm_df$norm_dist)
norm_df
#negative values were replaced with the min flux value =0.198

write.csv(norm_df, "normdist.csv") #command to write the data into a .csv file

shtv5 <- read.csv("shtv5.csv", sep="\t") #file with the columns and fluxes generated pasted from normdist.csv

data <- shtv5[,-1]
data <- as.data.frame(data)
data$Type<-as.factor(data$Type) #to transform Type into a factor

#to create the SMOTE'd data from the new dataset
newData <- SmoteClassif(Type ~., data,  C.perc = list(E = 1,R = 6))

#to log transform the dataset
logFluxsmote <- log(newData$Flux,10)
log_smote <-as.data.frame(logFluxsmote) 

set.seed(12)
x <- data
theta <- function(x){SmoteClassif(Type ~., data,  C.perc = list(E = 1,R = 6))}
results_smote <- bootstrap(x,1000,theta)

results_smote

smote3 <-as.data.frame(results_smote$thetastar)
smote_flux <- select(smote3, -starts_with("Temp"))
smote_flux <- select(smote_flux, -starts_with("Type"))
write.csv(smote_flux2, "smote.csv")
smote<-read.csv("smote.csv")
smote_matrix <- unlist(smote[,2:1000])
smote_df <-as.data.frame(smote_matrix)
write.csv(smotev2_df, "smote_df.csv")


