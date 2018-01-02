classify <- function(y, X, method = c("logistic", "lda", "naiveBayes"), 
                     intercept = TRUE, indCont = NULL) {
    method <- match.arg(method)
    if (length(y) != nrow(X)) stop("The dimensions of y and X do not match!")
    if (nlevels(as.factor(y)) != 2) stop("y dosen't have exactly 2 levels!")
    
    #Number of predictors
    p <- ncol(X)
    
    #Convert y to 0/1
    y <- as.factor(y)
    y.lev <- levels(y)
    if (!identical(y.lev , c("0","1"))) {
        y <- ifelse(y == min(y.lev) , 0, 1)
    } else{
        y <- as.numeric(levels(y))[y]
    }
    
    if (method == "logistic") {
        
        #Check whether intercept is included
        if (intercept == TRUE) {
            X <- model.matrix(~., data = data.frame(X))
        } else {
            X <- model.matrix(~. - 1, data = data.frame(X))
        }
        #Sigmoid function
        sigmoid <- function(t)
        {
            g <- 1/(1+exp(-t))
            g
        }
        
        #Cost Function
        cost <- function(theta)
        {
            n <- nrow(X)
            h <- sigmoid(X%*%theta)
            J <- (1/n)*sum((-y*log(h)) - ((1-y)*log(1-h)))
            J
        }
        
        #Calculate beta using gradient descent
        beta.optim <- optim(par=rep(0, ncol(X)), fn=cost)
        beta <- beta.optim$par
        names(beta) <- colnames(X)
        list(method = "logistic", intercept = intercept, coef = beta, p = p)
        
    } else if (method == "lda") {
        X <- model.matrix(~. - 1, data = data.frame(X))
        
        X0 <- X[y==0, ]
        X1 <- X[y==1, ]
        
        n0 <- sum(y==0)
        n1 <- sum(y==1)
        n <- n0 + n1
        
        mu0 <- colMeans(X0)
        mu1 <- colMeans(X1)
        #V0 <- cov(X0)
        #V1 <- cov(X1)
        #V <- ((n0-1)*V0 + (n1-1)*V1)/(n-2)
        
        V <- cov(X)
        
        beta <- solve(V, mu1 - mu0)
        names(beta) <- colnames(X)
        
        z <- log(n0/n1) + 0.5 * t(mu0 + mu1) %*% beta
        list(method = "lda", coef = beta, threshold = z, p = p)
    
    } else if (method == "naiveBayes"){
        
        #Check priors
        prior <- c(1-mean(y), mean(y))
        X <- as.data.frame(X)
        
        if (! missing(indCont)){
            #X.cont <- X[, indCont]
            #X0.cont <- X[y==0, indCont]
            #X1.cont <- X[y==1, indCont]
            
            parm <- function(x) { 
                mu <- c(mean(x[y==0]), mean(x[y==1])) 
                sigma <- c(sd(x[y==0]), sd(x[y==1]))
                m <- cbind(mu, sigma)
                row.names(m) <- c(0 ,1)
                m
            }
            if (length(indCont)>1) {
                cont.par <- lapply(X[, indCont], parm)
            } else {
                cont.par <- list(parm(X[, indCont]))
                names(cont.par) <- colnames(X)[indCont]
            }
            
            X.disc <- X[, -indCont]
        } else {
            cont.par <- NA
        }
        
        if (length(indCont) == p) {
            list(method = "naiveBayes", prior = prior, discretProb = NA, 
                 contParamter = cont.par, indCont = indCont, p = p)
        } else {
            
            if (p - length(indCont) > 1){
                tbl.list <- sapply(X.disc, table, y)
                tbl.list <- lapply(tbl.list, t)
                disc.probs <- sapply(tbl.list, function(x) { 
                    apply(x, 1, function(x) { 
                        x / sum(x) }) })
                disc.probs <- lapply(disc.probs, t)
            } else {
                tbl <- t(table(X.disc, y))
                disc.probs <- list(tbl/rowSums(tbl))
                names(disc.probs) <- colnames(X)[-indCont]
            }
            list(method = "naiveBayes", prior = prior, discretProb = disc.probs, 
                 contParamter = cont.par, indCont = indCont, p = p)
        }
        
    }
    
}

classify.predict <- function(model, X) {
    method <- match.arg(model$method, c("logistic", "lda", "naiveBayes"))
    N <- nrow(X)
    p <- ncol(X)
    
    if (p != model$p) stop("The column number of X does not match predictors!")
    
    if (method == "logistic") {
        #Sigmoid function
        sigmoid <- function(t)
        {
            g <- 1/(1+exp(-t))
            g
        }
        
        #Check whether intercept is included
        if (model$intercept == TRUE) {
            X <- model.matrix(~., data = data.frame(X))
        } else {
            X <- model.matrix(~. - 1, data = data.frame(X))
        }
        
        prob <- sigmoid(X%*%model$coef)
        class <- as.factor(ifelse(prob < 0.5, 0, 1))
        list(class = class, prob= prob, N = N)
        
        
    } else if (method == "lda") {
        X <- model.matrix(~. - 1, data = data.frame(X))
        
        dis <- X %*% model$coef
        class <- as.factor(ifelse(dis < as.vector(model$threshold), 0, 1))
        list(class = class, linearDiscriminant = dis, N = N)
        
        
    
    } else if (method == "naiveBayes") {
        if (!is.null(model$indCont)) {
            X.cont <- as.matrix(X[, model$indCont])
            
            #Compute the prod of densities for each observation given 0 and 1
            densCompt <- function(i) {
                prob <- do.call(rbind, 
                                lapply(c(1:length(model$indCont)), 
                                       function(j) {
                                           dnorm(X.cont[i, j], 
                                                 model$contParamter[[j]][ ,1], 
                                                 model$contParamter[[j]][ ,2])
                                              }))
                apply(prob, 2, prod)
            }
            
            
            prob.cont <- do.call(rbind, lapply(c(1:N), densCompt))
            X.disc <- as.matrix(X[, -model$indCont])
        }
        else {
            prob.cont <- 1
            X.disc <- as.matrix(X)
        }
        
        if (length(model$indCont) == p) {
            prob.disc <- 1
        } else {
            
            #Compute the prod of pmfs for each observation given 0 and 1
            pmfsCompt <- function(i) {
                prob <- do.call(rbind, 
                                lapply(c(1:ncol(X.disc)), 
                                       function(j) {
                                           model$discretProb[[j]][ 
                                               ,as.character(X.disc[i,j])]
                                       }))
                apply(prob, 2, prod)
            }
            prob.disc <- do.call(rbind, lapply(c(1:N), pmfsCompt))
        }
        
        prior <- model$prior
        post <- prob.cont * prob.disc * matrix(prior, N, 2, byrow = TRUE)
        class <- as.factor(ifelse(post[, 1] > post[, 2], 0, 1))
        list(class = class, posterior = post, prior = prior, N = N)
    }
}


