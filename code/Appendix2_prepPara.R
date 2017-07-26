# a function that takes Maxent parameters following the general R manner
prepPara <- function(para_userfeatures=NULL, #NULL=autofeature, could be any combination of # c("L", "Q", "H", "H", "P")
                     para_responsecurve=TRUE,
                     para_jackknife=TRUE,      
                     para_outputformat=c("logistic"),
                     para_outputfiletype=c("asc"), 
                     para_projectionlayers=NULL,
                     para_randomseed=FALSE,
                     para_removeduplicates=TRUE,
                     para_betamultiplier=NULL,
                     para_biasfile=NULL,
                     para_testsamplesfile=NULL,
                     para_replicates=1,
                     para_replicatetype="crossvalidate",
                     para_writeplotdata=TRUE,
                     para_extrapolate=TRUE,
                     para_doclamp=TRUE,
                     beta_threshold=NULL,
                     beta_categorical=NULL,
                     beta_lqp=NULL,
                     beta_hinge=NULL,
                     para_applythresholdrule=NULL
                     ){
  #20 & 29-33 features, default is autofeature
  if(is.null(para_userfeatures)){
    args_out <- c("autofeature")
  } else {
    args_out <- c("noautofeature")
    if(grepl("L",para_userfeatures)) args_out <- c(args_out,"linear") else args_out <- c(args_out,"nolinear")
    if(grepl("Q",para_userfeatures)) args_out <- c(args_out,"quadratic") else args_out <- c(args_out,"noquadratic")
    if(grepl("H",para_userfeatures)) args_out <- c(args_out,"hinge") else args_out <- c(args_out,"nohinge")
    if(grepl("P",para_userfeatures)) args_out <- c(args_out,"product") else args_out <- c(args_out,"noproduct")
    if(grepl("T",para_userfeatures)) args_out <- c(args_out,"threshold") else args_out <- c(args_out,"nothreshold")
  }
  
  #1 
  if(para_responsecurve) args_out <- c(args_out,"responsecurves") else args_out <- c(args_out,"noresponsecurves")
  #2
  #if(para_picture) args_out <- c(args_out,"pictures") else args_out <- c(args_out,"nopictures")
  #3
  if(para_jackknife) args_out <- c(args_out,"jackknife") else args_out <- c(args_out,"nojackknife")
  #4
  args_out <- c(args_out,paste0("outputformat=",para_outputformat))
  #5
  args_out <- c(args_out,paste0("outputfiletype=",para_outputfiletype))
  #7
  if(!is.null(para_projectionlayers))    args_out <- c(args_out,paste0("projectionlayers=",para_projectionlayers))
  #10
  if(para_randomseed) args_out <- c(args_out,"randomseed") else args_out <- c(args_out,"norandomseed")
  #16
  if(para_removeduplicates) args_out <- c(args_out,"removeduplicates") else args_out <- c(args_out,"noremoveduplicates")
  #20 & 53-56
  # check if negative
  betas <- c( para_betamultiplier,beta_threshold,beta_categorical,beta_lqp,beta_hinge)
  if(! is.null(betas) ){
    for(i in 1:length(betas)){
      if(betas[i] <0) stop("betamultiplier has to be positive")
    }
  }
    if (  !is.null(para_betamultiplier)  ){
      args_out <- c(args_out,paste0("betamultiplier=",para_betamultiplier))
    } else {
      if(!is.null(beta_threshold)) args_out <- c(args_out,paste0("beta_threshold=",beta_threshold))
      if(!is.null(beta_categorical)) args_out <- c(args_out,paste0("beta_categorical=",beta_categorical))
      if(!is.null(beta_lqp)) args_out <- c(args_out,paste0("beta_lqp=",beta_lqp))
      if(!is.null(beta_hinge)) args_out <- c(args_out,paste0("beta_hinge=",beta_hinge))
    }
  #22
  if(!is.null(para_biasfile))    args_out <- c(args_out,paste0("biasfile=",para_biasfile))
  #23
  if(!is.null(para_testsamplesfile))    args_out <- c(args_out,paste0("testsamplesfile=",para_testsamplesfile))
  #24&25
  para_replicates <- as.integer(para_replicates)
  if(para_replicates>1 ){
    args_out <- c(args_out,
                  paste0("replicates=",para_replicates),
                  paste0("replicatetype=",para_replicatetype) )
  }
  #37
  if(para_writeplotdata) args_out <- c(args_out,"writeplotdata") else args_out <- c(args_out,"nowriteplotdata")
  #39
  if(para_extrapolate) args_out <- c(args_out,"extrapolate") else args_out <- c(args_out,"noextrapolate")
  #42
  if(para_doclamp) args_out <- c(args_out,"doclamp") else args_out <- c(args_out,"nodoclamp")
  #60
  if(!is.null(para_applythresholdrule))    args_out <- c(args_out,paste0("applythresholdrule=",para_applythresholdrule))
  
  return(args_out)
}
