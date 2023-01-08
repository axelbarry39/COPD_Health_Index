#' Variables having an impact on a disease presence/absence
#'
#' Create a list of the variable having an effect on the presence/absence of a disease. Using a logistic regression for every unique variable in the dataset.
#' @param y The presence/absence value for the study disease.
#' @param x The variables values we want to analyse.
#' @param signiThresh The pvalue limit of the glm output that we conserve the variables.
#' @param slopeThresh The slope value limit of the glm output that we conserve the variables.
#' @return List of the variables having a pvalue lower thant signiThresh value and a slope value higher than the slopeThresh.
#' @examples 
#' list1 <- COPD_HEALTH_INDEX_fct(y2,x2,signiThresh = 0.05, slopeThresh = 0.1);
#' list2 <- COPD_HEALTH_INDEX_fct(y2,x2,signiThresh = 0.05, slopeThresh = 0.1);
#' @export
COPD_HEALTH_INDEX_fct <- function(y, X, signiThresh, slopeThresh) {
  
  z <- list()
  
  
  for(i in 1:ncol(X)) {
    if  (is.numeric(X[,i]) == TRUE) {
      
      model_glm<- summary(glm(y ~ scale(X[,i]) , family = binomial(link = "logit")))
      
    }
    
    else                            {
      
      model_glm <- summary(glm(y ~X[,i] , family = binomial(link = "logit")))
      
    }
    
    z[[i]] <- model_glm$coefficients
  }
  names(z) <- colnames(X)
  
  
  
  
  a <- data.frame()
  varNames <- vector(mode = "character")
  counter <- 1
  
  ##For loop inserting our glm summary ouput into a data.frame##
  
  for (i in 1:length(z)) {
    
    for (j in 2:nrow(z[[i]])) {
      
      a[counter,1] <- z[[i]][j,1] 
      
      a[counter,2] <- z[[i]][j,4] 
      
      varNames[counter] <- names(z[i])
      
      counter <- counter + 1
      
    }
  }
  
  
  colnames(a) <- c("Estimate", "Pr(>|z|)")
  matrice <- cbind(varNames, a)
  Decision <- vector(mode = "logical")
  
  
  
  for (i in 1:nrow(matrice)) {
    
    if  (matrice[i,3] < signiThresh & abs(matrice[i,2]) > slopeThresh) {
      
      Decision[length(Decision)+1] <- matrice[i,1]
      
    }
  }
  Decision <- unique(Decision)
  Decision
  
  
  print(Decision)
  
  
}

#' Procruste analysis for patient with a disease between 2 time-step.
#'
#' Does a procruste analysis on the patient between two time-step to get a distance of how much their explanatory variables changes between the them.
#' @param x Dataset with the variable we want to analyse with the individual number and the the disease presence/absence in the first two columns.
#' @return A plot of the distance between the indivuduals and the value on the distance for each of them. 
#' @examples 
#' Proc1 <- MPOC_OUI_Procrustre(x1);
#' Proc2 <- MPOC_OUI_Procrustre (x2);
#' @export
MPOC_OUI_Procruste <- function(x){
  MPOCData <- model.matrix(~.,x[,-(1:2)])
  library(vegan)
  pcaMPOCNon <- rda(MPOCData[x[,2] == 0,])
  pcaMPOCOui <- rda(MPOCData[x[,2] == 1,])
  procr <- procrustes(pcaMPOCNon, pcaMPOCOui)
  data_ind <- numeric()
  for(i in 1:nrow(procr$X)){
    data_ind[i] <- dist(rbind(procr$X[i,], procr$Yrot[i,]))
  }
  data_ind
  names(data_ind) <- x[,1][x[,2] == 0]
  plot(procrustes(pcaMPOCNon, pcaMPOCOui))
  print(data_ind)
}