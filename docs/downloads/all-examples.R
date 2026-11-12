## ----setup, include=FALSE----------------------------------------------------------
library(fdaSP)
library(ggplot2)
knitr::opts_chunk$set(echo=TRUE, message=FALSE, warning=TRUE, error=FALSE,
                     fig.width=8, fig.height=4.8, out.width="100%",
                     fig.align="center", dpi=160, cache=FALSE)
options(width=85)
theme_set(theme_minimal(base_size=12) + theme(legend.position="bottom"))



## ----helpers-----------------------------------------------------------------------
library(fdaSP)
library(ggplot2)
theme_set(theme_minimal(base_size=12)+theme(legend.position="bottom"))
ctl <- list(maxit=3000, abstol=1e-6, reltol=1e-6, print.out=FALSE)
path_data <- function(path, lambda) {
  path <- as.matrix(path)
  stopifnot(nrow(path)==length(lambda))
  data.frame(lambda=rep(lambda,ncol(path)), coefficient=as.vector(path),
             term=factor(rep(seq_len(ncol(path)),each=nrow(path))))
}
check_path <- function(fit) {
  stopifnot(all(is.finite(fit$sp.coef.path)), all(fit$convergence==1))
}


## ----linear-data-------------------------------------------------------------------
set.seed(101)
n <- 100
p <- 12
X <- matrix(rnorm(n*p), n, p)
beta <- c(2,-1,1,rep(0,p-3))
y <- drop(X %*% beta) + rnorm(n)
lambda <- 10^seq(0.7,-2,length.out=30)
control <- ctl


## ----linear-fit--------------------------------------------------------------------
fit <- lmSP(X=X, y=y, penalty="LASSO", lambda=lambda,
            standardize.data=FALSE, intercept=FALSE, control=control)
check_path(fit)
dim(fit$sp.coef.path)


## ----linear-path, fig.cap="Lasso coefficient paths. The horizontal axis is logarithmic; each line is one predictor."----
ggplot(path_data(fit$sp.coef.path,fit$lambda),
       aes(lambda,coefficient,colour=term)) + geom_line(linewidth=.65) +
  scale_x_log10() + labs(x=expression(lambda),y="Coefficient",colour="Predictor")


## ----linear-cv---------------------------------------------------------------------
set.seed(102)
cv <- lmSP_cv(X=X, y=y, penalty="LASSO", lambda=fit$lambda,
              cv.fold=5, standardize.data=FALSE, control=control)
i_cv <- which.min(abs(fit$lambda-cv$lambda.min))
beta_cv <- as.numeric(fit$sp.coef.path[i_cv,])
Xnew <- matrix(rnorm(40*p),40,p)
ynew <- drop(Xnew %*% beta) + rnorm(40)
pred <- drop(Xnew %*% beta_cv)
data.frame(lambda=cv$lambda.min, test_MSE=mean((ynew-pred)^2))


## ----linear-cv-plot, fig.cap="Cross-validated prediction error and the selected tuning parameter."----
ggplot(data.frame(lambda=cv$lambda,mse=as.numeric(cv$mse)),aes(lambda,mse)) +
  geom_line(colour="#9e1b32") + geom_point(size=1.5) +
  geom_vline(xintercept=cv$lambda.min,linetype=2) + scale_x_log10() +
  labs(x=expression(lambda),y="Cross-validated MSE")


## ----penalty-families--------------------------------------------------------------
groups <- rep(1:4,each=3)
overlap <- list(groups=c(1:4,4:7,7:10,10:12),Glen=c(4,4,4,3))
family_args <- list(
  LASSO=list(),
  GLASSO=list(groups=groups),
  spGLASSO=list(groups=groups,alpha=.5),
  OVGLASSO=list(groups=overlap),
  spOVGLASSO=list(groups=overlap,alpha=.5))
fits <- lapply(names(family_args),function(method) {
  do.call(lmSP,c(list(X=X,y=y,penalty=method,lambda=lambda,
                     standardize.data=FALSE,control=control),family_args[[method]]))
})
names(fits) <- names(family_args)
invisible(lapply(fits,check_path))
paths <- do.call(rbind,lapply(names(fits),function(method) {
  d <- path_data(fits[[method]]$sp.coef.path,fits[[method]]$lambda)
  d$method <- method
  d
}))


## ----penalty-paths, fig.height=7, fig.cap="Coefficient paths under five penalties, using the same response and tuning grid. Equal numerical tuning parameters do not imply equal penalty strength across methods."----
ggplot(paths,aes(lambda,coefficient,group=term,colour=term)) +
  geom_line(linewidth=.55) + scale_x_log10() + facet_wrap(~method,ncol=2) +
  labs(x=expression(lambda),y="Coefficient") + theme(legend.position="none")


## ----adaptive-fits-----------------------------------------------------------------
adaptive_lasso <- lmSP(X=X,y=y,penalty="LASSO",adaptive=TRUE,
                       lambda=lambda,standardize.data=FALSE,control=control)
pilot <- drop(qr.solve(X,y))
pilot_norms <- vapply(split(pilot,groups),function(b) sqrt(sum(b^2)),numeric(1))
group_weights <- 1/pmax(pilot_norms,1e-4)
adaptive_group <- lmSP(X=X,y=y,penalty="GLASSO",groups=groups,
                       group_weights=group_weights,lambda=lambda,
                       standardize.data=FALSE,control=control)
check_path(adaptive_lasso)
check_path(adaptive_group)


## ----linear-dof, fig.cap="Degrees-of-freedom estimates returned by fdaSP along the available fitted paths."----
dof_methods <- names(fits)[vapply(fits,function(z) !is.null(z$dof),logical(1))]
df_paths <- do.call(rbind,lapply(dof_methods,function(method) {
  z <- fits[[method]]
  stopifnot(length(z$dof)==length(z$lambda))
  data.frame(lambda=z$lambda,dof=as.numeric(z$dof),method=method)
}))
ggplot(df_paths,aes(lambda,dof,colour=method)) + geom_line(linewidth=.7) +
  scale_x_log10() + labs(x=expression(lambda),y="Estimated degrees of freedom",colour=NULL)


## ----scalar-data-------------------------------------------------------------------
set.seed(201)
n_fun <- 80
t <- seq(0,1,length.out=41)
H <- splines::bs(t,df=8,intercept=TRUE)
curve_scores <- matrix(rnorm(n_fun*8),n_fun,8)
X_curves <- curve_scores %*% t(H)
qweight <- c(.5,rep(1,length(t)-2),.5)*diff(t)[1]
X_quad <- sweep(X_curves,2,qweight,"*")
psi_true <- drop(H %*% c(0,0,2,2,-1,0,0,0))
y_fun <- drop(X_quad %*% psi_true) + rnorm(n_fun,sd=.08)


## ----predictor-curves, fig.cap="Six predictor curves before quadrature weighting."----
curves <- data.frame(t=rep(t,6),x=as.vector(t(X_curves[1:6,])),
                     subject=factor(rep(1:6,each=length(t))))
ggplot(curves,aes(t,x,colour=subject)) + geom_line() +
  labs(x="Domain",y="Predictor value",colour="Subject")


## ----scalar-fit--------------------------------------------------------------------
scalar_fit <- f2sSP(vY=y_fun,mX=X_quad,M=8,splOrd=4,
                    nlambda1=25,lambda1.min.ratio=.001,
                    standardize.data=FALSE,control=control)
check_path(scalar_fit)
set.seed(202)
scalar_cv <- f2sSP_cv(vY=y_fun,mX=X_quad,M=8,splOrd=4,
                      lambda1=scalar_fit$lambda1,cv.fold=5,
                      standardize.data=FALSE,control=control)
i_scalar <- which.min(abs(scalar_fit$lambda1-scalar_cv$lambda1.min.mse))
psi_est <- drop(H %*% scalar_fit$sp.coef.path[i_scalar,])
stopifnot(max(abs(psi_est-scalar_fit$sp.fun.path[i_scalar,]))<1e-8)


## ----coefficient-curves, fig.cap="True coefficient curve and the estimate selected by subject-level cross-validation."----
curve_comparison <- data.frame(t=rep(t,2),value=c(psi_true,psi_est),
                               curve=rep(c("Truth","CV estimate"),each=length(t)))
ggplot(curve_comparison,aes(t,value,colour=curve)) + geom_line(linewidth=.85) +
  scale_colour_manual(values=c("CV estimate"="#9e1b32","Truth"="#334e68")) +
  labs(x="Domain",y=expression(psi(t)),colour=NULL)


## ----scalar-path, fig.cap="Basis-coefficient paths for the scalar-on-function model."----
ggplot(path_data(scalar_fit$sp.coef.path,scalar_fit$lambda1),
       aes(lambda,coefficient,colour=term)) + geom_line() + scale_x_log10() +
  labs(x=expression(lambda[1]),y="Basis coefficient",colour="Basis")


## ----scalar-prediction-------------------------------------------------------------
set.seed(203)
Xnew_curves <- matrix(rnorm(30*8),30,8) %*% t(H)
Xnew_quad <- sweep(Xnew_curves,2,qweight,"*")
ynew_fun <- drop(Xnew_quad %*% psi_true)+rnorm(30,sd=.08)
pred_fun <- drop(Xnew_quad %*% psi_est)
data.frame(test_MSE=mean((ynew_fun-pred_fun)^2),
           curve_integrated_squared_error=sum(qweight*(psi_est-psi_true)^2))


## ----scalar-diagnostics, fig.height=6, fig.cap="Training error, BIC and the returned degrees-of-freedom estimate across the scalar-on-function path."----
scalar_diagnostics <- data.frame(lambda=rep(scalar_fit$lambda1,3),
 value=c(scalar_fit$mse,scalar_fit$bic,scalar_fit$dof),
 measure=rep(c("Training MSE","BIC","Degrees of freedom"),each=length(scalar_fit$lambda1)))
ggplot(scalar_diagnostics,aes(lambda,value)) + geom_line(colour="#9e1b32") +
 scale_x_log10() + facet_wrap(~measure,scales="free_y",ncol=1) +
 labs(x=expression(lambda[1]),y=NULL)


## ----scalar-smoothing--------------------------------------------------------------
smooth_fit <- f2sSP(vY=y_fun,mX=X_quad,M=8,lambda1=scalar_fit$lambda1,
                    lambda2=c(.001,.01),diff_order=2,
                    standardize.data=FALSE,control=control)
check_path(smooth_fit)
dim(smooth_fit$sp.coef.path)


## ----surface-data------------------------------------------------------------------
set.seed(301)
Theta <- splines::bs(t,df=5,intercept=TRUE)
Psi_true <- matrix(0,8,5)
Psi_true[3:4,2:3] <- 2
surface_true <- H %*% Psi_true %*% t(Theta)
Y_curves <- X_quad %*% surface_true +
  matrix(rnorm(n_fun*length(t),sd=.04),n_fun,length(t))


## ----surface-fit-------------------------------------------------------------------
surface_fit <- f2fSP(mY=Y_curves,mX=X_quad,M=8,L=5,
                     nlambda=20,lambda.min.ratio=.001,
                     standardize.data=FALSE,control=control)
check_path(surface_fit)
set.seed(302)
surface_cv <- f2fSP_cv(mY=Y_curves,mX=X_quad,M=8,L=5,
                       lambda=surface_fit$lambda,cv.fold=5,
                       standardize.data=FALSE,control=control)
i_surface <- which.min(abs(surface_fit$lambda-surface_cv$lambda.min))
Psi_est <- surface_fit$sp.coef.path[i_surface,,]
surface_est <- H %*% Psi_est %*% t(Theta)
stopifnot(max(abs(surface_est-surface_fit$sp.fun.path[i_surface,,]))<1e-8)


## ----surface-plot, fig.height=4.6, fig.cap="True and estimated coefficient surfaces on a common colour scale. Horizontal coordinates index the predictor domain; vertical coordinates index the response domain."----
surface_plot <- rbind(
  transform(expand.grid(t=t,s=t),value=as.vector(surface_true),panel="Truth"),
  transform(expand.grid(t=t,s=t),value=as.vector(surface_est),panel="CV estimate"))
ggplot(surface_plot,aes(t,s,fill=value)) + geom_raster() + coord_equal() +
  facet_wrap(~panel) + scale_fill_gradient2(low="#2166ac",mid="white",high="#b2182b") +
  labs(x="Predictor domain t",y="Response domain s",fill=expression(psi(t,s)))


## ----surface-path, fig.cap="Tensor-product coefficient paths; the first array dimension indexes the regularization parameter."----
surface_path_matrix <- matrix(surface_fit$sp.coef.path,
                               nrow=length(surface_fit$lambda))
ggplot(path_data(surface_path_matrix,surface_fit$lambda),
       aes(lambda,coefficient,group=term)) + geom_line(colour="#9e1b32",alpha=.6) +
  scale_x_log10() + labs(x=expression(lambda),y="Tensor-product coefficient")


## ----surface-prediction------------------------------------------------------------
set.seed(303)
Ynew <- Xnew_quad %*% surface_true +
  matrix(rnorm(nrow(Xnew_quad)*length(t),sd=.04),nrow(Xnew_quad),length(t))
Ypred <- Xnew_quad %*% surface_est
subject <- 1
curve_prediction <- data.frame(s=rep(t,2),value=c(Ynew[subject,],Ypred[subject,]),
                                curve=rep(c("Observed","Predicted"),each=length(t)))
mean((Ynew-Ypred)^2)


## ----surface-prediction-plot, fig.cap="Observed and predicted response for a held-out subject."----
ggplot(curve_prediction,aes(s,value,colour=curve)) + geom_line(linewidth=.8) +
  labs(x="Response domain",y="Response",colour=NULL)


## ----original-inflation-setup------------------------------------------------------
library(fdaSP)
library(ggplot2)
stopifnot(all(vapply(c("glmnet","leaps","BMS","corrplot"),
                    requireNamespace,logical(1),quietly=TRUE)))
original_data <- new.env()
load("../data/inflation-Q.RData",envir=original_data)
inflation <- original_data$infl.data
y_original <- as.numeric(inflation$CPIAUCSL)
lag1_names <- grep("_l1$",names(inflation),value=TRUE)
all_lag_names <- grep("_l[1-4]$",names(inflation),value=TRUE)
X1 <- as.matrix(inflation[,lag1_names])
X4 <- as.matrix(inflation[,all_lag_names])
stopifnot(ncol(X1)==18L,ncol(X4)==72L,
          all(is.finite(X4)),all(is.finite(y_original)))
data.frame(observations=nrow(inflation),first=min(inflation$DATE),
           last=max(inflation$DATE),one_lag=ncol(X1),four_lags=ncol(X4))


## ----original-correlations, fig.width=8, fig.height=7, fig.cap="Inflation and the 18 first-lag macroeconomic predictors. Variables are ordered by the angular order of eigenvectors (AOE), as in the original analysis."----
X1_scaled <- scale(X1)
y_original_scaled <- drop(scale(y_original))
correlation_data <- cbind(INFL=y_original_scaled,X1_scaled)
correlation_matrix <- cor(correlation_data)
corrplot::corrplot(correlation_matrix,order="AOE",method="ellipse",
                   type="upper",tl.cex=.65,tl.col="#333333",mar=c(0,0,1,0))


## ----original-ols-one-lag----------------------------------------------------------
formula1 <- reformulate(lag1_names,response="CPIAUCSL")
ols1 <- lm(formula1,data=inflation)
summary(ols1)


## ----original-glmnet-one-lag-------------------------------------------------------
set.seed(42)
folds1 <- sample(rep(seq_len(10),length.out=length(y_original)))
lasso1 <- glmnet::glmnet(X1,y_original,alpha=1,standardize=TRUE,nlambda=100)
cv_lasso1 <- glmnet::cv.glmnet(X1,y_original,alpha=1,standardize=TRUE,
                              nlambda=100,foldid=folds1)
cv_enet1 <- glmnet::cv.glmnet(X1,y_original,alpha=.5,standardize=TRUE,
                             nlambda=100,foldid=folds1)
enet1_selected <- glmnet::glmnet(X1,y_original,alpha=.5,standardize=TRUE,
                                 lambda=cv_enet1$lambda.min)


## ----original-plot-helpers---------------------------------------------------------
glmnet_path_data <- function(object,label) {
  b <- as.matrix(object$beta)
  data.frame(lambda=rep(object$lambda,each=nrow(b)),
             coefficient=as.vector(b),
             term=rep(rownames(b),times=ncol(b)),method=label)
}
glmnet_cv_data <- function(object,label) {
  data.frame(lambda=object$lambda,mse=object$cvm,se=object$cvsd,method=label)
}
selected_coefficients <- function(object,rule,label) {
  b <- as.matrix(coef(object,s=rule))
  keep <- b[,1]!=0
  data.frame(method=label,rule=rule,term=rownames(b)[keep],
             coefficient=as.numeric(b[keep,1]),row.names=NULL)
}


## ----original-lasso-one-path, fig.cap="One-lag Lasso coefficient paths from glmnet; each line represents one macroeconomic regressor."----
ggplot(glmnet_path_data(lasso1,"Lasso"),aes(lambda,coefficient,group=term)) +
  geom_line(colour="#9e1b32",alpha=.65) + scale_x_log10() +
  labs(x=expression(lambda),y="Coefficient")


## ----original-one-cv, fig.height=5, fig.cap="Ten-fold CV for one-lag Lasso and Elastic Net. Ribbons show the CV mean plus or minus the standard error returned by glmnet; dashed lines mark lambda.min."----
one_cv <- rbind(glmnet_cv_data(cv_lasso1,"Lasso"),
                 glmnet_cv_data(cv_enet1,"Elastic Net"))
one_cv_choices <- data.frame(method=c("Lasso","Elastic Net"),
                              lambda=c(cv_lasso1$lambda.min,cv_enet1$lambda.min))
ggplot(one_cv,aes(lambda,mse)) +
  geom_ribbon(aes(ymin=mse-se,ymax=mse+se),fill="#9e1b32",alpha=.15) +
  geom_line(colour="#9e1b32") +
  geom_vline(data=one_cv_choices,aes(xintercept=lambda),linetype=2) +
  facet_wrap(~method,scales="free_x") + scale_x_log10() +
  labs(x=expression(lambda),y="Cross-validated MSE")


## ----original-one-selected---------------------------------------------------------
one_selected <- rbind(
  selected_coefficients(cv_lasso1,"lambda.min","Lasso"),
  selected_coefficients(cv_lasso1,"lambda.1se","Lasso"),
  selected_coefficients(cv_enet1,"lambda.min","Elastic Net"),
  selected_coefficients(cv_enet1,"lambda.1se","Elastic Net"))
knitr::kable(one_selected,digits=5)
stopifnot(max(abs(as.matrix(coef(enet1_selected))-
                 as.matrix(coef(cv_enet1,s="lambda.min"))))<1e-3)


## ----original-best-subset----------------------------------------------------------
subset_fit <- leaps::regsubsets(x=X1_scaled,y=y_original_scaled,
                                method="exhaustive",nvmax=8)
subset_summary <- summary(subset_fit)
subset_summary
subset_criteria <- data.frame(size=seq_along(subset_summary$bic),
                              BIC=subset_summary$bic,
                              Cp=subset_summary$cp,RSS=subset_summary$rss)
best_subset_size <- which.min(subset_summary$bic)
subset_coefficients <- coef(subset_fit,id=best_subset_size)
subset_terms <- setdiff(names(subset_coefficients),"(Intercept)")
subset_data <- data.frame(response=y_original_scaled,X1_scaled,check.names=FALSE)
subset_refit <- lm(reformulate(subset_terms,response="response"),data=subset_data)
coef(subset_refit)
stopifnot(max(abs(coef(subset_refit)[names(subset_coefficients)]-
                   subset_coefficients))<1e-7)


## ----original-subset-criteria, fig.height=5, fig.cap="BIC, Mallows' Cp and residual sum of squares for the best subset at each size. Separate vertical scales avoid comparing unlike criteria numerically."----
subset_plot <- data.frame(size=rep(subset_criteria$size,3),
 value=c(subset_criteria$BIC,subset_criteria$Cp,subset_criteria$RSS),
 criterion=rep(c("BIC","Mallows' Cp","RSS"),each=nrow(subset_criteria)))
ggplot(subset_plot,aes(size,value)) + geom_line(colour="#9e1b32") +
 geom_point() + facet_wrap(~criterion,scales="free_y") +
 scale_x_continuous(breaks=1:8) + labs(x="Number of regressors",y=NULL)


## ----original-adaptive-pilots------------------------------------------------------
set.seed(1234)
folds_adaptive <- sample(rep(seq_len(10),length.out=length(y_original)))
cv_ridge1 <- glmnet::cv.glmnet(X1,y_original,alpha=0,standardize=TRUE,
                              foldid=folds_adaptive,type.measure="mse")
cv_ridge1
ridge_pilot <- as.matrix(coef(cv_ridge1,s="lambda.min"))[-1,1]
ols_pilot <- coef(ols1)[colnames(X1)]
weight_floor <- 1e-8
ridge_weights <- 1/pmax(abs(ridge_pilot),weight_floor)
ols_weights <- 1/pmax(abs(ols_pilot),weight_floor)
pilot_table <- data.frame(term=colnames(X1),ridge=ridge_pilot,OLS=ols_pilot,
                          ridge_weight=ridge_weights,OLS_weight=ols_weights)
knitr::kable(pilot_table,digits=5,row.names=FALSE)


## ----original-adaptive-fits--------------------------------------------------------
cv_adaptive_ols <- glmnet::cv.glmnet(X1,y_original,alpha=1,
  standardize=TRUE,foldid=folds_adaptive,type.measure="mse",penalty.factor=ols_weights)
cv_adaptive_ridge <- glmnet::cv.glmnet(X1,y_original,alpha=1,
  standardize=TRUE,foldid=folds_adaptive,type.measure="mse",penalty.factor=ridge_weights)


## ----original-adaptive-cv, fig.cap="CV curves for adaptive Lasso using OLS and ridge pilots, with full-sample weights held fixed throughout CV."----
adaptive_cv_plot <- rbind(glmnet_cv_data(cv_adaptive_ols,"OLS pilot"),
                          glmnet_cv_data(cv_adaptive_ridge,"Ridge pilot"))
ggplot(adaptive_cv_plot,aes(lambda,mse)) +
 geom_ribbon(aes(ymin=mse-se,ymax=mse+se),fill="#9e1b32",alpha=.15) +
 geom_line(colour="#9e1b32") + facet_wrap(~method,scales="free_x") +
 scale_x_log10() + labs(x=expression(lambda),y="Cross-validated MSE")


## ----original-adaptive-path, fig.cap="Adaptive coefficient paths for the two pilots. Dashed and dotted lines mark lambda.min and lambda.1se, respectively."----
adaptive_paths <- rbind(glmnet_path_data(cv_adaptive_ols$glmnet.fit,"OLS pilot"),
                         glmnet_path_data(cv_adaptive_ridge$glmnet.fit,"Ridge pilot"))
adaptive_choices <- data.frame(method=rep(c("OLS pilot","Ridge pilot"),each=2),
 rule=rep(c("lambda.min","lambda.1se"),2),
 lambda=c(cv_adaptive_ols$lambda.min,cv_adaptive_ols$lambda.1se,
          cv_adaptive_ridge$lambda.min,cv_adaptive_ridge$lambda.1se))
ggplot(adaptive_paths,aes(lambda,coefficient,group=term)) +
 geom_line(colour="#9e1b32",alpha=.6) +
 geom_vline(data=adaptive_choices,aes(xintercept=lambda,linetype=rule),inherit.aes=FALSE) +
 scale_linetype_manual(values=c("lambda.min"="dashed","lambda.1se"="dotted")) +
 facet_wrap(~method,scales="free_x") + scale_x_log10() +
 labs(x=expression(lambda),y="Coefficient",linetype="CV rule")


## ----original-adaptive-selected----------------------------------------------------
adaptive_selected <- rbind(
 selected_coefficients(cv_adaptive_ols,"lambda.1se","OLS pilot"),
 selected_coefficients(cv_adaptive_ridge,"lambda.1se","Ridge pilot"))
knitr::kable(adaptive_selected,digits=5)


## ----original-bms------------------------------------------------------------------
BMS_data <- data.frame(response=y_original,X1,check.names=FALSE)
set.seed(417)
bma_sample <- BMS::bms(BMS_data,burn=10000,iter=50000,
  mprior="uniform",g="UIP",mcmc="rev.jump",nmodel=100,
  user.int=FALSE,logfile=FALSE)
bma_enumeration <- BMS::bms(BMS_data,mprior="uniform",g="UIP",
  mcmc="enumerate",nmodel=100,user.int=FALSE,logfile=FALSE)
summary(bma_sample)
summary(bma_enumeration)


## ----original-bma-coefficients-----------------------------------------------------
bma_sample_coef <- coef(bma_sample,exact=FALSE,order.by.pip=FALSE,
                         include.constant=FALSE,std.coefs=FALSE)
bma_full_coef <- coef(bma_enumeration,exact=FALSE,order.by.pip=FALSE,
                       include.constant=FALSE,std.coefs=FALSE)
bma_top100_coef <- coef(bma_sample,exact=TRUE,order.by.pip=FALSE,
                         include.constant=TRUE,std.coefs=FALSE)
knitr::kable(bma_top100_coef,digits=5,
 caption="Posterior coefficient summaries restricted to the 100 best retained models, including the intercept.")
bma_comparison <- data.frame(term=rownames(bma_full_coef),
 PIP_sample=bma_sample_coef[,"PIP"],PIP_enumeration=bma_full_coef[,"PIP"],
 mean_sample=bma_sample_coef[,"Post Mean"],mean_enumeration=bma_full_coef[,"Post Mean"])
knitr::kable(bma_comparison,digits=5,row.names=FALSE)


## ----original-bma-pip, fig.height=6, fig.cap="Posterior inclusion probabilities from the sampled run and full enumeration of the one-lag model space. A single sampled run is a comparison, not a convergence guarantee."----
pip_plot <- data.frame(term=rep(bma_comparison$term,2),
 PIP=c(bma_comparison$PIP_sample,bma_comparison$PIP_enumeration),
 method=rep(c("Model sampling","Full enumeration"),each=nrow(bma_comparison)))
ggplot(pip_plot,aes(PIP,reorder(term,PIP),colour=method)) +
 geom_point(position=position_dodge(width=.45),size=2) +
 scale_x_continuous(limits=c(0,1)) + labs(x="Posterior inclusion probability",y=NULL,colour=NULL)


## ----original-bma-top-models-------------------------------------------------------
retained_model_table <- function(fit, n=8L) {
  tab <- BMS::topmodels.bma(fit)
  weights <- tab["PMP (Exact)", ]
  stopifnot(all(is.finite(weights)), all(weights >= 0), sum(weights) > 0)
  weights <- weights / sum(weights)
  out <- rbind(tab[fit$reg.names, , drop=FALSE],
               "Posterior weight conditional on retained models"=weights)
  out[, seq_len(min(n,ncol(out))), drop=FALSE]
}
top_sample <- retained_model_table(bma_sample)
top_enumeration <- retained_model_table(bma_enumeration)
knitr::kable(top_sample,digits=4,caption="Eight best retained models from the sampled run; weights are conditional on the 100 retained models.")
knitr::kable(top_enumeration,digits=4,caption="Eight best models from enumeration; weights are conditional on the 100 retained models.")


## ----original-four-lag-ols---------------------------------------------------------
formula4 <- reformulate(all_lag_names,response="CPIAUCSL")
ols4 <- lm(formula4,data=inflation)
summary(ols4)
ols_comparison <- data.frame(model=c("One lag","Four lags"),
 regressors=c(ncol(X1),ncol(X4)),residual_df=c(df.residual(ols1),df.residual(ols4)),
 training_MSE=c(mean(residuals(ols1)^2),mean(residuals(ols4)^2)),
 adjusted_R2=c(summary(ols1)$adj.r.squared,summary(ols4)$adj.r.squared))
knitr::kable(ols_comparison,digits=5)


## ----original-four-lag-glmnet------------------------------------------------------
lasso4 <- glmnet::glmnet(X4,y_original,alpha=1,standardize=TRUE,nlambda=100)
cv_enet4 <- glmnet::cv.glmnet(X4,y_original,alpha=.5,standardize=TRUE,
                             nlambda=100,foldid=folds1)
enet4_selected <- glmnet::glmnet(X4,y_original,alpha=.5,standardize=TRUE,
                                 lambda=cv_enet4$lambda.min)
coef(enet4_selected)
knitr::kable(selected_coefficients(cv_enet4,"lambda.1se","Four-lag Elastic Net"),digits=5)


## ----original-four-lag-path, fig.cap="Four-lag Lasso path on the 72-predictor design."----
ggplot(glmnet_path_data(lasso4,"Four lags"),aes(lambda,coefficient,group=term)) +
 geom_line(colour="#9e1b32",alpha=.5) + scale_x_log10() +
 labs(x=expression(lambda),y="Coefficient")


## ----original-four-lag-cv, fig.cap="Ten-fold CV for the four-lag Elastic Net. The ribbon is one glmnet standard error; the dashed line marks lambda.min."----
ggplot(glmnet_cv_data(cv_enet4,"Four lags"),aes(lambda,mse)) +
 geom_ribbon(aes(ymin=mse-se,ymax=mse+se),fill="#9e1b32",alpha=.15) +
 geom_line(colour="#9e1b32") + geom_vline(xintercept=cv_enet4$lambda.min,linetype=2) +
 scale_x_log10() + labs(x=expression(lambda),y="Cross-validated MSE")


## ----original-fdasp-cv-------------------------------------------------------------
original_control <- list(adaptation=TRUE,rho=1,maxit=3000,
                          reltol=1e-4,abstol=1e-4,mu.ada=10,tau.ada=2,print.out=FALSE)
set.seed(418)
original_fdasp_cv <- fdaSP::lmSP_cv(X=X4,y=y_original,penalty="LASSO",
  standardize.data=TRUE,intercept=FALSE,cv.fold=5,nlambda=30,
  control=original_control)
original_fdasp_path <- fdaSP::lmSP(X=X4,y=y_original,penalty="LASSO",
  standardize.data=TRUE,intercept=FALSE,lambda=original_fdasp_cv$lambda,
  control=original_control)
stopifnot(all(original_fdasp_path$convergence==1),
          all(original_fdasp_cv$convergence==1),
          all(is.finite(original_fdasp_path$sp.coef.path)))


## ----original-fdasp-cv-plot, fig.cap="Original fdaSP workflow: five-fold CV error plus or minus one fold-error standard deviation, with the selected lambda marked."----
fdasp_cv_plot <- data.frame(lambda=original_fdasp_cv$lambda,
 mse=as.numeric(original_fdasp_cv$mse),sd=as.numeric(original_fdasp_cv$mse.sd))
ggplot(fdasp_cv_plot,aes(lambda,mse)) +
 geom_ribbon(aes(ymin=mse-sd,ymax=mse+sd),fill="#9e1b32",alpha=.15) +
 geom_line(colour="#9e1b32") + geom_vline(xintercept=original_fdasp_cv$lambda.min,linetype=2) +
 scale_x_log10() + labs(x=expression(lambda),y="Cross-validated MSE (package scale)")


## ----original-fdasp-path, fig.cap="The fdaSP coefficient path from the original four-lag example, evaluated on the same lambda grid as its CV fit."----
fdasp_coefficients <- as.matrix(original_fdasp_path$sp.coef.path)
fdasp_original_path_plot <- data.frame(
 lambda=rep(original_fdasp_path$lambda,ncol(fdasp_coefficients)),
 coefficient=as.vector(fdasp_coefficients),
 term=rep(colnames(X4),each=nrow(fdasp_coefficients)))
ggplot(fdasp_original_path_plot,aes(lambda,coefficient,group=term)) +
 geom_line(colour="#9e1b32",alpha=.55) +
 geom_vline(xintercept=original_fdasp_cv$lambda.min,linetype=2) +
 scale_x_log10() + labs(x=expression(lambda),y="Coefficient")


## ----inflation-data----------------------------------------------------------------
data_env <- new.env()
load("../data/inflation-Q.RData",envir=data_env)
infl <- data_env$infl.data
y_infl <- as.numeric(infl$CPIAUCSL)
X_infl <- as.matrix(infl[,3:74])
stopifnot(all(is.finite(X_infl)),all(is.finite(y_infl)))
N <- nrow(X_infl)
train <- seq_len(floor(.6*N))
valid <- seq.int(max(train)+1,floor(.8*N))
test <- seq.int(max(valid)+1,N)
data.frame(period=c("Training","Validation","Test"),
           observations=c(length(train),length(valid),length(test)))


## ----inflation-fit-----------------------------------------------------------------
xmean <- colMeans(X_infl[train,,drop=FALSE])
xsd <- apply(X_infl[train,,drop=FALSE],2,sd)
keep <- is.finite(xsd) & xsd>0
X_infl <- X_infl[,keep,drop=FALSE]
xmean <- xmean[keep]; xsd <- xsd[keep]
Xs <- sweep(sweep(X_infl,2,xmean,"-"),2,xsd,"/")
ymean <- mean(y_infl[train])
infl_fit <- lmSP(X=Xs[train,,drop=FALSE],y=y_infl[train]-ymean,
                 penalty="LASSO",nlambda=30,lambda.min.ratio=.001,
                 standardize.data=FALSE,control=control)
check_path(infl_fit)
valid_predictions <- Xs[valid,,drop=FALSE] %*% t(infl_fit$sp.coef.path)+ymean
validation_mse <- colMeans((valid_predictions-y_infl[valid])^2)
i_infl <- which.min(validation_mse)
test_predictions <- drop(Xs[test,,drop=FALSE] %*% infl_fit$sp.coef.path[i_infl,])+ymean
inflation_results <- data.frame(lambda=infl_fit$lambda[i_infl],
 validation_MSE=min(validation_mse),test_MSE=mean((y_infl[test]-test_predictions)^2),
 training_mean_baseline_MSE=mean((y_infl[test]-ymean)^2))
inflation_results


## ----inflation-path, fig.cap="Validation error over the inflation regression path. The test period is not used to choose lambda."----
ggplot(data.frame(lambda=infl_fit$lambda,mse=validation_mse),aes(lambda,mse)) +
 geom_line(colour="#9e1b32") + geom_vline(xintercept=infl_fit$lambda[i_infl],linetype=2) +
 scale_x_log10() + labs(x=expression(lambda),y="Chronological validation MSE")


## ----inflation-predictions, fig.cap="Observed inflation and predictions over the held-out final period; the horizontal axis retains the data's row order."----
infl_plot <- data.frame(observation=rep(test,2),value=c(y_infl[test],test_predictions),
                        series=rep(c("Observed","Predicted"),each=length(test)))
ggplot(infl_plot,aes(observation,value,colour=series)) + geom_line() + geom_point(size=1.2) +
 labs(x="Observation index",y="Inflation",colour=NULL)


## ----convergence-table-------------------------------------------------------------
convergence_table <- data.frame(
 model=c("Lasso","Scalar-on-function","Function-on-function","Inflation"),
 path_points=c(length(fit$lambda),length(scalar_fit$lambda1),
               length(surface_fit$lambda),length(infl_fit$lambda)),
 converged=c(all(fit$convergence==1),all(scalar_fit$convergence==1),
             all(surface_fit$convergence==1),all(infl_fit$convergence==1)))
knitr::kable(convergence_table)


## ----session-version---------------------------------------------------------------
packageVersion("fdaSP")

