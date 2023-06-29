# Yamashita et al. Multivariate Landscape Structure Analysis


#####################################################################################################################
###################################### Image Classification Accuracy Assessment #####################################
#####################################################################################################################

# NOTE: This section can be run independent of the above
rm(list = ls()); gc()

# Loading and preparing accuracy assessment data ####
acc1 <- openxlsx::read.xlsx("Data/Accuracy_Assessment_20210528.xlsx", sheet = 2)

# Assessing accuracy ####
acc2 <- table(acc1[,2], acc1[,15])

(accuracy <- sum(diag(acc2))/sum(acc2))


#####################################################################################################################
######################################### Pre-processing: Preparing the Data ########################################
#####################################################################################################################

# NOTE: This section can be run independent of the above
rm(list = ls()); gc()

# Loading and prepping bobcat data for summarizing ####
## Load the data from excel files
bob_excel <- "data/bobcats_WCS_interactions_all.xlsx"
bobcats <- lapply(openxlsx::getSheetNames(bob_excel), function(i){openxlsx::read.xlsx(bob_excel, sheet = i, detectDates = T)})
names(bobcats) <- openxlsx::getSheetNames(bob_excel)

## Load the site information data
stations <- openxlsx::read.xlsx("data/EnvData_RS.xlsx")

## Combine bobcat data with site information
bobcats2 <- lapply(1:length(bobcats), function(i){
  x1 <- merge.data.frame(bobcats[[i]], stations[stations$Road==names(bobcats)[i],], by.x = "Site", by.y = "Station", all.x = T)
  x2 <- x1[x1$Date >= "2019-12-03" & x1$Date <= "2020-11-11",]  # Earliest date that all roads active and latest date that all roads active
  x2$Month <- lubridate::month(x2$Date)
  x2$Year <- lubridate::year(x2$Date)
  x2$yearmonth <- with(x2, paste(Year, formatC(Month, width = 2, flag = "0"), sep = ""))
  return(x2)
})
names(bobcats2) <- names(bobcats)


# Calculate number of detections by site ####
bobcats3 <- do.call(dplyr::bind_rows, bobcats2)

bob_sum <- dplyr::summarise(dplyr::group_by(bobcats3, Name1, Name2, yearmonth), sum = sum(Individuals))
bob_mean <- dplyr::summarise(dplyr::group_by(bob_sum, Name1, yearmonth), det_av = mean(sum), det_total = sum(sum))
bob_mean$Label <- with(bob_mean, paste(Name1, yearmonth, sep = "_"))

stations2 <- data.frame("Site" = rep(sort(unique(stations$Name1)), each = 12), 
           "yearmonth" = rep(sort(unique(bob_mean$yearmonth)), times = 14))
stations2$Label <- with(stations2, paste(Site, yearmonth, sep = "_"))

b1 <- merge.data.frame(stations2, bob_mean[,3:5], by = "Label", all.x = T)
b1$det_av[is.na(b1$det_av)] <- 0
b1$det_total[is.na(b1$det_total)] <- 0


# Load the Metrics data ####
m1 <- openxlsx::read.xlsx("data/PERMANOVA_metrics_20230308.xlsx")


# Combine with bobcat detections ####
## Do the combining
ds1 <- merge.data.frame(m1, b1, by = "Site", all.y = T)
ds2 <- ds1[,c(17,1,18,2:16,19:20)]

## Save the data
#openxlsx::write.xlsx(ds2, file = paste("Regression_all_", format(Sys.Date(), "%Y%m%d"), ".xlsx", sep = ""))


#####################################################################################################################
########################################## Analysis 1: PERMANOVA of Metrics #########################################
#####################################################################################################################

# NOTE: This section can be run independent of the above
rm(list = ls()); gc()

# Part 1: Load the data ####
ds_perm <- openxlsx::read.xlsx("data/PERMANOVA_metrics_20230406.xlsx")

## Convert "Type" to a factor for plotting
ds_perm$Type <- factor(ds_perm$Type, levels = c("Crossing", "Random"), labels = c("Crossing" = "WCS", "Random" = "Random"))


# Part 2: PERMANOVA Analysis ####

## NOTE: This was all run in Primer v7


# Part 3: Visualizing the results ####
## Run PCA and examine results
pca_perm <- prcomp(ds_perm[,3:16], center = T, scale = T)

## Examine the PCA results
### Eigenvalues and percent of variation explained
data.frame(eigenvalue = pca_perm$sdev^2,                                    # Variance explained by each axis
           proportion = (pca_perm$sdev^2)/sum(pca_perm$sdev^2)*100,         # Percent variance explained by each axis
           cumulative = cumsum((pca_perm$sdev^2)/sum(pca_perm$sdev^2)*100)) # Cumulative percent variance
### Eigenvectors
pca_perm$rotation
### Correlation between data and PCA axes
cor(ds_perm[,3:16], pca_perm$x)[,1:3]

## Prepare to create the ordination diagram
pcaplotFun <- function(data, pca, data.cols, pca.cols, x1, x2, Split, size.text, size.lab){
  #data = ds_perm
  #pca = pca_perm
  #data.cols = 1:2
  #pca.cols = 3:16
  #x1 = "PC1"
  #x2 = "PC2"
  #Split = "Type"
  #size.text = 5
  #size.lab = 13
  
  ds <- list(sitescores = data.frame(data[,data.cols],pca$x), 
                              correlation = data.frame(metric = rownames(cor(data[,pca.cols], pca$x)),cor(data[,pca.cols], pca$x)*5)
  )
  
  require(ggplot2)
  theme.plot <- theme(text = element_text(family = "serif")) + 
    theme(plot.title = element_text(hjust = 0.5, size = size.lab*1.25, margin = margin(b = 0.5, unit = "inch")), 
          #plot.background = element_blank(), 
          plot.margin = unit(c(.1,.1,.1,.1), "inch")) +
    theme(axis.ticks = element_line(color = NA, linewidth = 1, linetype = "solid"), 
          axis.line = element_line(color = NA, linewidth = .1, linetype = "solid"), 
          axis.title=element_text(size=size.lab, margin = margin(t = 0.25, unit="inch")),  
          axis.title.x = element_text(vjust = 0), 
          axis.title.y = element_text(angle = 90, vjust = 1.5), 
          axis.text = element_text(size = size.lab*0.75), 
          axis.text.x = element_text(angle = 0, hjust = 0.5), 
          axis.text.y = element_text(angle = 0, hjust = 0)) + 
    theme(panel.border = element_rect(fill = NA, color = "black"), 
          panel.background = element_rect(fill = NA, color = NA), 
          panel.grid.major = element_line(color = NA)) + 
    theme(legend.margin=margin(c(0.15,0.15,0.15,0.15), unit = "inch"), 
          legend.background = element_rect(fill = NA, color = NA), 
          legend.text=element_text(size = size.lab*0.75), 
          legend.title=element_text(size=size.lab*0.75), 
          #legend.position = "top", 
          #legend.key = element_rect(color = "black", fill = NA), 
          legend.key.height = unit(0.25,"inch"), 
          legend.key.width = unit(0.25, "inch")) + 
    theme(strip.background = element_rect(fill = "gray85", color = "black"), 
          strip.text = element_text(size = size.lab*0.75), 
          strip.text.x = element_text(margin = margin(t = 0.05, r = 0.05, b = 0.05, l = 0.05, unit = "inch")), 
          strip.text.y = element_text(angle = -90, margin = margin(t = 0.05, r = 0.05, b = 0.05, l = 0.05, unit = "inch")))
  
  if(length(unique(ds[[1]][,Split]))==1){
    scale <- scale_color_manual(Split, values = c("red"))
  }else if(length(unique(ds[[1]][,Split]))==2){
    scale <- scale_color_manual(Split, values = c("red", "blue"))
  }else if(length(unique(ds[[1]][,Split]))==3){
    scale <- scale_color_manual(Split, values = c("red", "blue", "purple"))
  }else{
    stop("Choose a column for Split with only 2 or 3 unique values")
  }
  
  plot.out <- ggplot(mapping = aes(x = !!sym(x1), y = !!sym(x2))) + 
    geom_segment(aes(x=Inf,xend=-Inf,y=0,yend=0), color = "grey80") + 
    geom_segment(aes(x=0,xend=0,y=Inf,yend=-Inf), color = "grey80") + 
    geom_segment(data = ds[[2]], aes(x=!!sym(x1), y=!!sym(x2), xend=0, yend=0), color = "grey50") +
    geom_point(data = ds[[1]], aes(color = !!sym(Split), shape = !!sym(Split)), size = 5) + 
    geom_text(data = ds[[2]], aes(label = rownames(ds[[2]]), family = "serif"), size = size.text) + 
    scale + 
    guides(shape = "none") + 
    coord_cartesian(c(-6.5,6.5),c(-6.5,6.5)) + 
    theme.plot
  
  #plot.out
  
  return(plot.out)
  rm(ds, theme.plot,plot.out)
  #rm(data,x1,x2,type,size.lab,size.text)
}

## The ordination diagrams
pca_perm_12 <- pcaplotFun(data = ds_perm, pca = pca_perm, data.cols = 1:2, pca.cols = 3:16, x1 = "PC1", x2 = "PC2", Split = "Type", size.text = 4, size.lab = 12)
pca_perm_13 <- pcaplotFun(data = ds_perm, pca = pca_perm, data.cols = 1:2, pca.cols = 3:16, "PC1", "PC3", "Type", size.text = 4, size.lab = 12)
pca_perm_23 <- pcaplotFun(data = ds_perm, pca = pca_perm, data.cols = 1:2, pca.cols = 3:16, "PC2", "PC3", "Type", size.text = 4, size.lab = 12)
pca_perm_12
pca_perm_13
pca_perm_23

## Saving the ordinations
ggsave(filename = paste("PCA_perm_12_", format(Sys.Date(), "%Y%m%d"), ".tif", sep = ""), plot = pca_perm_12, device = "tiff", width = 5, height = 4.5, dpi = 900, compression = "lzw")
ggsave(filename = paste("PCA_perm_13_", format(Sys.Date(), "%Y%m%d"), ".tif", sep = ""), plot = pca_perm_13, device = "tiff", width = 5, height = 4.5, dpi = 900, compression = "lzw")
ggsave(filename = paste("PCA_perm_23_", format(Sys.Date(), "%Y%m%d"), ".tif", sep = ""), plot = pca_perm_23, device = "tiff", width = 5, height = 4.5, dpi = 900, compression = "lzw")

## Creating a multi-plot
pca_perm_plot <- ggpubr::ggarrange(pca_perm_12, pca_perm_13, ncol = 2, common.legend = T, labels = "auto")
pca_perm_plot
ggsave(filename = paste("PCA_perm_plot_", format(Sys.Date(), "%Y%m%d"), ".tif", sep = ""), plot = pca_perm_plot, device = "tiff", width = 6.5, height = 4.0, dpi = 900, compression = "lzw")


# Part 4: Calculation of basic metrics of landscape structure ####
ds_means <- aggregate(ds_perm[,3:16], by = list(ds_perm$Type), function(x){c(mean = mean(x), sd = sd(x))})
str(ds_means)
ds_means2 <- lapply(ds_means[,-1], function(x){
  apply(x, 1, function(y){paste(round(y, 2), collapse = "+")})
})
ds_means3 <- data.frame(do.call(rbind, ds_means2))
ds_means3
write.csv(ds_means3, file = paste("Metrics_means_", format(Sys.Date(), "%Y%m%d"), ".csv", sep = ""))


#####################################################################################################################
################################# Analysis 2: GLMM on Bobcat Detections and Metrics #################################
#####################################################################################################################

# NOTE: This section can be run independent of the above
rm(list = ls()); gc()

# Part 1: Loading the data ####
## Load the original data used for regression
ds_reg <- openxlsx::read.xlsx("Data/Regression_all_20230406.xlsx")

## Load the regression results from SAS
ds_SAS <- read.csv("SAS_data_PCA_20230406.csv")
ds_beta <- read.csv("SAS_betahats_20230406.csv")
ds_cov <- read.csv("SAS_varcov_20230406.csv")

ds_GLMM <- ds_SAS[,c(2:3,20:22,18,19)]


# Part 2: The Generalized Linear Mixed Models ####

## NOTE: All regressions were all run in SAS v9.4


# Part 3: Visualizing the PCA results ####
## Running PCA on the configuration and lidar metrics
pca_config <- prcomp(ds_reg[,6:13], center = T, scale = T)
pca_lidar <- prcomp(ds_reg[,14:19], center = T, scale = T)

## Assess percent variance explained using eigenvalues
with(pca_config, data.frame(vars = "Configuration", eigenvalue=sdev^2, proportion=(sdev^2)/sum(sdev^2)*100, cumulative=cumsum((sdev^2)/sum(sdev^2)*100)))
with(pca_lidar, data.frame(vars = "LiDAR", eigenvalue=sdev^2, proportion=(sdev^2)/sum(sdev^2)*100, cumulative=cumsum((sdev^2)/sum(sdev^2)*100)))

## Examine Eigenvectors
pca_config$rotation
pca_lidar$rotation

## Calculate correlation between PCA axes and variables
ds_SAS[1:2,]
panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...){
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y))
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste0(prefix, txt)
  if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex.cor * r)
}
### For Configuration
cor(ds_SAS[,c(4:11,20:21)])
pairs(ds_SAS[,c(4:11,20:21)], upper.panel = panel.cor)
### For LiDAR
cor(ds_SAS[,c(12:17,22)])
pairs(ds_SAS[,c(12:17,22)], upper.panel = panel.cor)

## Create the ordination diagrams
pcaplotFun <- function(data, pca, data.cols, pca.cols, x1, x2, Split, size.text, size.lab){
  #data = ds_perm
  #pca = pca_perm
  #data.cols = 1:2
  #pca.cols = 3:16
  #x1 = "PC1"
  #x2 = "PC2"
  #Split = "Type"
  #size.text = 5
  #size.lab = 13
  
  ds <- list(sitescores = data.frame(data[,data.cols],pca$x), 
             correlation = data.frame(metric = rownames(cor(data[,pca.cols], pca$x)),cor(data[,pca.cols], pca$x)*5)
  )
  
  require(ggplot2)
  theme.plot <- theme(text = element_text(family = "serif")) + 
    theme(plot.title = element_text(hjust = 0.5, size = size.lab*1.25, margin = margin(b = 0.5, unit = "inch")), 
          #plot.background = element_blank(), 
          plot.margin = unit(c(.1,.1,.1,.1), "inch")) +
    theme(axis.ticks = element_line(color = NA, linewidth = 1, linetype = "solid"), 
          axis.line = element_line(color = NA, linewidth = .1, linetype = "solid"), 
          axis.title=element_text(size=size.lab, margin = margin(t = 0.25, unit="inch")),  
          axis.title.x = element_text(vjust = 0), 
          axis.title.y = element_text(angle = 90, vjust = 1.5), 
          axis.text = element_text(size = size.lab*0.75), 
          axis.text.x = element_text(angle = 0, hjust = 0.5), 
          axis.text.y = element_text(angle = 0, hjust = 0)) + 
    theme(panel.border = element_rect(fill = NA, color = "black"), 
          panel.background = element_rect(fill = NA, color = NA), 
          panel.grid.major = element_line(color = NA)) + 
    theme(legend.margin=margin(c(0.15,0.15,0.15,0.15), unit = "inch"), 
          legend.background = element_rect(fill = NA, color = NA), 
          legend.text=element_text(size = size.lab*0.75), 
          legend.title=element_text(size=size.lab*0.75), 
          #legend.position = "top", 
          #legend.key = element_rect(color = "black", fill = NA), 
          legend.key.height = unit(0.25,"inch"), 
          legend.key.width = unit(0.25, "inch")) + 
    theme(strip.background = element_rect(fill = "gray85", color = "black"), 
          strip.text = element_text(size = size.lab*0.75), 
          strip.text.x = element_text(margin = margin(t = 0.05, r = 0.05, b = 0.05, l = 0.05, unit = "inch")), 
          strip.text.y = element_text(angle = -90, margin = margin(t = 0.05, r = 0.05, b = 0.05, l = 0.05, unit = "inch")))
  
  if(length(unique(ds[[1]][,Split]))==1){
    scale <- scale_color_manual(Split, values = c("red"))
  }else if(length(unique(ds[[1]][,Split]))==2){
    scale <- scale_color_manual(Split, values = c("red", "blue"))
  }else if(length(unique(ds[[1]][,Split]))==3){
    scale <- scale_color_manual(Split, values = c("red", "blue", "purple"))
  }else{
    stop("Choose a column for Split with only 2 or 3 unique values")
  }
  
  plot.out <- ggplot(mapping = aes(x = !!sym(x1), y = !!sym(x2))) + 
    geom_segment(aes(x=Inf,xend=-Inf,y=0,yend=0), color = "grey80") + 
    geom_segment(aes(x=0,xend=0,y=Inf,yend=-Inf), color = "grey80") + 
    geom_segment(data = ds[[2]], aes(x=!!sym(x1), y=!!sym(x2), xend=0, yend=0), color = "grey50") +
    geom_point(data = ds[[1]], aes(color = !!sym(Split), shape = !!sym(Split)), size = 5) + 
    geom_text(data = ds[[2]], aes(label = rownames(ds[[2]]), family = "serif"), size = size.text) + 
    scale + 
    guides(shape = "none") + 
    coord_cartesian(c(-6,6),c(-6,6)) + 
    theme.plot
  
  #plot.out
  
  return(plot.out)
  rm(ds, theme.plot,plot.out)
  #rm(data,x1,x2,type,size.lab,size.text)
}

### Primary plots
pca_config_12 <- pcaplotFun(data = ds_reg, pca = pca_config, data.cols = 1:5, pca.cols = 6:13, "PC1", "PC2", "Road", size.text = 4, size.lab = 12)
pca_lidar_12 <- pcaplotFun(data = ds_reg, pca = pca_lidar, data.cols = 1:5, pca.cols = 14:19, "PC1", "PC2", "Road", size.text = 4, size.lab = 12)
pca_config_12
pca_lidar_12

### Other Plots
pca_config_13 <- pcaplotFun(data = ds_reg, pca = pca_config, data.cols = 1:5, pca.cols = 6:13, "PC1", "PC3", "Road", size.text = 4, size.lab = 12)
pca_config_23 <- pcaplotFun(data = ds_reg, pca = pca_config, data.cols = 1:5, pca.cols = 6:13, "PC2", "PC3", "Road", size.text = 4, size.lab = 12)
pca_lidar_13 <- pcaplotFun(data = ds_reg, pca = pca_lidar, data.cols = 1:5, pca.cols = 14:19, "PC1", "PC3", "Road", size.text = 4, size.lab = 12)
pca_lidar_23 <- pcaplotFun(data = ds_reg, pca = pca_lidar, data.cols = 1:5, pca.cols = 14:19, "PC2", "PC3", "Road", size.text = 4, size.lab = 12)
pca_config_13
pca_config_23
pca_lidar_13
pca_lidar_23

## Saving the ordinations
ggsave(paste("PCA_config_12_", format(Sys.Date(), "%Y%m%d"), ".tif", sep = ""), plot = pca_config_12, device = "tiff", width = 5, height = 4.5, dpi = 900, compression = "lzw")
ggsave(paste("PCA_lidar_12_", format(Sys.Date(), "%Y%m%d"), ".tif", sep = ""), plot = pca_lidar_12, device = "tiff", width = 5, height = 4.5, dpi = 900, compression = "lzw")

## Creating a multi-plot
pca_metrics_plot <- ggpubr::ggarrange(pca_config_12, pca_lidar_12, ncol = 2, common.legend = T, labels = "auto")
pca_metrics_plot
ggsave(filename = paste("PCA_metrics_", format(Sys.Date(), "%Y%m%d"), ".tif", sep = ""), plot = pca_metrics_plot, device = "tiff", width = 6.5, height = 4.0, dpi = 900, compression = "lzw")


# Part 4: Visualizing the Regression results ####
## Data manipulation for creation of plots
bs <- matrix(ds_beta[,2], ncol = 1, dimnames = list(ds_beta[,1], "Estimate"))
vc <- as.matrix(ds_cov[,3:7])
dimnames(vc) <- list(ds_cov[,1], ds_cov[,1])
df_error <- unique(ds_beta[,4])

## Calculate predicted values using the appropriate transformation
predictFun <- function(model, betas, ds, cov, constant = "mean", range = c(0.1, 0.9), n = 100, alpha = 0.025, df){
  #model <- "negbin"    # What type of model was used c("negbin", "poisson", "logistic", "gaussian", "normal")? Essentially, what was the link function used for the GLM/GLMM (log link, logit link)?
  #betas <- bs          # The betahat estimates for each predictor, including the Intercept
  #ds <- ds_GLMM        # The data used to run the regression
  #cov <- vc            # The variance-covariance matrix from the regression
  #constant <- "mean"   # The constants for each beta when predicting a different beta. This can be "mean" or a set of numbers with length equal to the number of betas
  #range <- c(0.1, 0.9) # Quantiles that will be used for estimating the minimum and maximum for each beta
  #n <- 100             # How many points should be predicted
  #alpha <- 0.05        # The alpha level you want to predict at
  #df <- df_error       # The error degrees of freedom
  
  # First thing is calculating a few necessary arguments
  k <- length(betas)                                                   # The number of parameters in the model
  ftab <- qf(1-alpha, k, df)                                           # A tabular F value given the provided alpha level and degrees of freedom. Required for calculation of the confidence band
  ttab <- qt(1-alpha, df)                                              # A tabular T value given the provided alpha level and degrees of freedom. Required for calculation of confidence intervals
  ranges <- apply(ds[,rownames(betas)[-1]], 2, quantile, probs=range)  # The value of the minimum and maximum quantiles specified by range
  
  # Next, we extract only those columns from the data that we need (the predictors)
  x <- ds[,colnames(ds) %in% rownames(betas)]
  
  # Now we need to see if the argument constant was defined or not
  if(constant=="mean"){
    constant <- c(Intercept = 1, apply(x, 2, mean))
  }else if(length(constant) != k){
    stop("The length of the provided constants is not the same as the number of betas. Did you remember to include the intercept?")
  }
  
  # Now we can start calculating important stuff
  ## Start by creating a vector of constants
  X1 <- sapply(constant, rep, n)
  
  ## Now create a vector representing the range of data you want to include
  X2 <- cbind(Intercept = 1, apply(ranges, 2, function(x){seq(min(x), max(x), length.out = n)}))
  
  ## Now calculate your yhats and confidence intervals and bands at the observed and interpreted scales
  X3 <- lapply(2:length(betas), function(i){
    name <- rownames(betas)[i]
    x1 <- data.frame(X1[,-i], X2[,i])
    colnames(x1)[ncol(x1)] <- name
    x2 <- x1[,rownames(betas)]
    
    x3 <- as.matrix(x2)
    x4 <- data.frame(pred = name,                            # The name of the variable you are predicting for
                     x2,                                     # The original data
                     yhat = c(x3 %*% betas),                 # The estimate of y calculated from the data
                     ftab,                                   # The tabular F value you calculated earlier
                     ttab,                                   # The tabular T value you calculated earlier
                     se = diag(sqrt(x3 %*% cov %*% t(x3))))
    x4$Lci <- with(x4, yhat - ttab*se)
    x4$Uci <- with(x4, yhat + ttab*se)
    x4$Lcb <- with(x4, yhat - sqrt(k*ftab)*se)
    x4$Ucb <- with(x4, yhat + sqrt(k*ftab)*se)
    
    if(model %in% c("poisson", "negbin")){
      x4$yhatI <- (exp(x4$yhat)-1)
      x4$LciI <- (exp(x4$Lci)-1)
      x4$UciI <- (exp(x4$Uci)-1)
      x4$LcbI <- (exp(x4$Lcb)-1)
      x4$UcbI <- (exp(x4$Ucb)-1)
    }else if(model %in% c("logistic")){
      x4$yhatI <- 1/(1 + exp(-x4$yhat))
      x4$LciI <- 1/(1 + exp(-x4$Lci))
      x4$UciI <- 1/(1 + exp(-x4$Uci))
      x4$LcbI <- 1/(1 + exp(-x4$Lcb))
      x4$UcbI <- 1/(1 + exp(-x4$Ucb))
    }else if(model %in% c("gaussian", "normal")){
      message("No backtransformation is needed for normally distributed data")
    }else{
      stop("You must choose a valid model for back-transformation")
    }
    return(x4)
    #rm(name, x1, x2, x3, x4)
  })
  names(X3) <- rownames(betas)[-1]

  return(X3)
  rm(k, ftab, ttab, ranges, x, X1, X2, X3)
  #rm(model, betas, ds, cov, constant, range, n, alpha, df)
}

R <- predictFun(model = "negbin", betas = bs, ds = ds_GLMM, cov = vc, constant = "mean", range = c(0.1, 0.9), n = 100, alpha = 0.025, df = df_error)

## Now we can finally get ready to plot our data
R2 <- lapply(1:length(R), function(i){
  x <- R[[i]]
  name <- names(R)[i]
  x1 <- data.frame(x[,c("pred", "yhatI", "LcbI", "UcbI", name)])
  colnames(x1) <- c("var", "yhatI", "LcbI", "UcbI", "pred")
  return(x1)
})
Rplot <- do.call(rbind, R2)
Rplot$var <- factor(Rplot$var, levels = c("PCconfig1", "PCconfig2", "PClidar1", "CanopyH_MN"), labels = c("PC Configuration 1", "PC Configuration 2", "PC LiDAR", "Mn Canopy Ht."))

regPlot <- function(ds, size.text, size.lab){
  #ds <- Rplot
  #size.text = 9
  #size.lab = 45
  require(ggplot2)
  
  theme.plot <- theme(text = element_text(family = "serif")) + 
    theme(plot.title = element_text(hjust = 0.5, size = size.lab*1.25, margin = margin(b = 0.5, unit = "inch")), 
          #plot.background = element_blank(), 
          plot.margin = unit(c(.1,.1,.1,.1), "inch")) +
    theme(axis.ticks = element_line(color = "grey50", linewidth = 1, linetype = "solid"), 
          axis.line = element_line(color = "grey50", linewidth = .1, linetype = "solid"), 
          axis.title=element_text(size=size.lab, margin = margin(t = 0.25, unit="inch")),  
          axis.title.x = element_text(vjust = 0), 
          axis.title.y = element_text(angle = 90, vjust = 1.5), 
          axis.text = element_text(size = size.lab*0.75), 
          axis.text.x = element_text(angle = 0, hjust = 0.5), 
          axis.text.y = element_text(angle = 0, hjust = 0)) + 
    theme(panel.border = element_rect(fill = NA, color = "black"), 
          panel.background = element_rect(fill = NA, color = NA), 
          panel.grid.major = element_line(color = NA)) + 
    theme(legend.margin=margin(c(0.15,0.15,0.15,0.15), unit = "inch"), 
          legend.background = element_rect(fill = NA, color = NA), 
          legend.text=element_text(size = size.lab*0.75), 
          legend.title=element_text(size=size.lab*0.75), 
          #legend.position = "top", 
          #legend.key = element_rect(color = "black", fill = NA), 
          legend.key.height = unit(0.25,"inch"), 
          legend.key.width = unit(0.25, "inch")) + 
    theme(strip.background = element_rect(fill = "gray85", color = "black"), 
          strip.text = element_text(size = size.lab*0.75), 
          strip.text.x = element_text(margin = margin(t = 0.05, r = 0.05, b = 0.05, l = 0.05, unit = "inch")), 
          strip.text.y = element_text(angle = -90, margin = margin(t = 0.05, r = 0.05, b = 0.05, l = 0.05, unit = "inch")))
  p <- ggplot(ds, aes(x = pred)) + 
    geom_line(aes(y = yhatI), col = "black", linewidth = 2) + 
    geom_line(aes(y = LcbI), col = "red", linewidth = 1.0) + 
    geom_line(aes(y = UcbI), col = "red", linewidth = 1.0) + 
    ylab("Number of Events") + 
    xlab("") + 
    facet_wrap(facets = vars(var), nrow = 2, scales = "free") + 
    theme.plot
  
  return(p)
  
}

## Plot the results
plot_reg <- regPlot(Rplot, size.text = 5, size.lab = 25)
plot_reg

## Save the plots
ggsave(filename = paste("Reg_plots_", format(Sys.Date(), "%Y%m%d"), ".tif", sep = ""), plot = plot_reg, device = "tiff", width = 6.5, height = 6, dpi = 900, compression = "lzw")

## Next, we should back-transform the betahat estimates and confidence intervals
ds_CI <- data.frame(ds_beta[,1:3], ttab = qt(1 - 0.025, ds_beta[,4]), Pvalue = ds_beta[,6])
ds_CI$Lci <- with(ds_CI, Estimate - ttab*StdErr)
ds_CI$Uci <- with(ds_CI, Estimate + ttab*StdErr)
ds_CI$EstimateI <- exp(ds_CI$Estimate)-1
ds_CI$LciI <- exp(ds_CI$Lci)-1
ds_CI$UciI <- exp(ds_CI$Uci)-1
ds_CI
ds_CI_mScale <- data.frame(Effect = ds_CI[,1], ds_CI[,c(8:10)]*100, ds_CI[,5])
colnames(ds_CI_mScale) <- c("Effect", "% Change", "LCI", "UCI", "P-value")
ds_CI_mScale
write.csv(ds_CI_mScale, file = paste("Transformed_Estimates_", format(Sys.Date(), "%Y%m%d"), ".csv", sep = ""))
write.csv(ds_CI, file = paste("Model_Output_", format(Sys.Date(), "%Y%m%d"), ".csv", sep = ""))

