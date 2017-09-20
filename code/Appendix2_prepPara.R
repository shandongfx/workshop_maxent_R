# A function that implements Maxent parameters using the general R manner
prepPara <- function(userfeatures=NULL, #NULL=autofeature, could be any combination of # c("L", "Q", "H", "H", "P")
                     responsecurves=TRUE,
                     jackknife=TRUE,      
                     outputformat="logistic",
                     outputfiletype="asc", 
                     projectionlayers=NULL,
                     randomseed=FALSE,
                     removeduplicates=TRUE,
                     betamultiplier=NULL,
                     biasfile=NULL,
                     testsamplesfile=NULL,
                     replicates=1,
                     replicatetype="crossvalidate",
                     writeplotdata=TRUE,
                     extrapolate=TRUE,
                     doclamp=TRUE,
                     beta_threshold=NULL,
                     beta_categorical=NULL,
                     beta_lqp=NULL,
                     beta_hinge=NULL,
                     applythresholdrule=NULL
                     ){
  #20 & 29-33 features, default is autofeature
  if(is.null(userfeatures)){
    args_out <- c("autofeature")
  } else {
    args_out <- c("noautofeature")
    if(grepl("L",userfeatures)) args_out <- c(args_out,"linear") else args_out <- c(args_out,"nolinear")
    if(grepl("Q",userfeatures)) args_out <- c(args_out,"quadratic") else args_out <- c(args_out,"noquadratic")
    if(grepl("H",userfeatures)) args_out <- c(args_out,"hinge") else args_out <- c(args_out,"nohinge")
    if(grepl("P",userfeatures)) args_out <- c(args_out,"product") else args_out <- c(args_out,"noproduct")
    if(grepl("T",userfeatures)) args_out <- c(args_out,"threshold") else args_out <- c(args_out,"nothreshold")
  }
  
  #1 
  if(responsecurves) args_out <- c(args_out,"responsecurves") else args_out <- c(args_out,"noresponsecurves")
  #2
  #if(picture) args_out <- c(args_out,"pictures") else args_out <- c(args_out,"nopictures")
  #3
  if(jackknife) args_out <- c(args_out,"jackknife") else args_out <- c(args_out,"nojackknife")
  #4
  args_out <- c(args_out,paste0("outputformat=",outputformat))
  #5
  args_out <- c(args_out,paste0("outputfiletype=",outputfiletype))
  #7
  if(!is.null(projectionlayers))    args_out <- c(args_out,paste0("projectionlayers=",projectionlayers))
  #10
  if(randomseed) args_out <- c(args_out,"randomseed") else args_out <- c(args_out,"norandomseed")
  #16
  if(removeduplicates) args_out <- c(args_out,"removeduplicates") else args_out <- c(args_out,"noremoveduplicates")
  #20 & 53-56
  # check if negative
  betas <- c( betamultiplier,beta_threshold,beta_categorical,beta_lqp,beta_hinge)
  if(! is.null(betas) ){
    for(i in 1:length(betas)){
      if(betas[i] <0) stop("betamultiplier has to be positive")
    }
  }
    if (  !is.null(betamultiplier)  ){
      args_out <- c(args_out,paste0("betamultiplier=",betamultiplier))
    } else {
      if(!is.null(beta_threshold)) args_out <- c(args_out,paste0("beta_threshold=",beta_threshold))
      if(!is.null(beta_categorical)) args_out <- c(args_out,paste0("beta_categorical=",beta_categorical))
      if(!is.null(beta_lqp)) args_out <- c(args_out,paste0("beta_lqp=",beta_lqp))
      if(!is.null(beta_hinge)) args_out <- c(args_out,paste0("beta_hinge=",beta_hinge))
    }
  #22
  if(!is.null(biasfile))    args_out <- c(args_out,paste0("biasfile=",biasfile))
  #23
  if(!is.null(testsamplesfile))    args_out <- c(args_out,paste0("testsamplesfile=",testsamplesfile))
  #24&25
  replicates <- as.integer(replicates)
  if(replicates>1 ){
    args_out <- c(args_out,
                  paste0("replicates=",replicates),
                  paste0("replicatetype=",replicatetype) )
  }
  #37
  if(writeplotdata) args_out <- c(args_out,"writeplotdata") else args_out <- c(args_out,"nowriteplotdata")
  #39
  if(extrapolate) args_out <- c(args_out,"extrapolate") else args_out <- c(args_out,"noextrapolate")
  #42
  if(doclamp) args_out <- c(args_out,"doclamp") else args_out <- c(args_out,"nodoclamp")
  #60
  if(!is.null(applythresholdrule))    args_out <- c(args_out,paste0("applythresholdrule=",applythresholdrule))
  
  return(args_out)
}
