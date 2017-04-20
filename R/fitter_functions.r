#
fit_glm = function(mm, response, train){
    library(glmnet)
    #error checks
    if(nrow(mm) != length(response)) stop('mm not equal to response length')
    if(nrow(mm) != length(train)) stop('mm not equal to train length')
    
    if(!is.logical(train)) stop('train is no logical')
    if(!is.logical(response)) stop('response is not logical')
       
    glmnet_fit = glmnet::cv.glmnet(x = mm[train,], y = response [train], family= "binomial")
    glmnet_test_prob = c(predict(glmnet_fit  , newx = mm[!train,]))
    
    glmnet_fit$out_of_bag = data.frame(predicted = inverse.logit(glmnet_test_prob), actual = response [!train])

    glmnet_fit$auc = auc(glmnet_fit$out_of_bag$predicted,  glmnet_fit$out_of_bag$actual )
    cat('auc =',     glmnet_fit$auc, '\n')
    

    # glmnet_fit$non_zero = en_coefs( glmnet_fit  , mm = mm, i_train = train, response =  response, min_size = 0)
    # glmnet_fit$non_zero$exp_coef = exp(glmnet_fi$non_zero$coef )
    glmnet_fit 
    }
    
 en_coefs =  function(mod, mm, i_train, response, min_size = 0, plot = FALSE){
    library(glmnet)
    cc = as.matrix(coef(mod,  mod $lambda.min))  

    mm_plus = mm[response,]
    mm_minus = mm[!response,]
    
    coefs = data.frame(name = rownames(cc),
        coefs = c(cc))#, 
        #count_training = c(NA, colSums( mm[i_train,])), 
       # count_test = c(NA, colSums( mm[!i_train,])),
       # positive_count = c(NA, colSums( mm_plus)) ,
         #negative_count = c(NA, colSums(  mm_minus) ))
         
    coefs = arrange(coefs, coefs)
    coefs_big = coefs[abs(coefs$coefs) > min_size,]

    coefs_big$name = gsub('\\n', ' ', coefs_big$name)
    if(plot)    barplot(coefs_big$coefs, names.arg = coefs_big$name, horiz = TRUE)  
    coefs_big
    }
    
    
 fit_xgboost = function(mm, response, train,plot_it = FALSE,  ...){
    library(xgboost)
    #error checks
    if(nrow(mm) != length(response)) stop('mm not equal to response length')
    if(nrow(mm) != length(train)) stop('mm not equal to train length')
    
    if(!is.logical(train)) stop('train is no logical')
    if(!is.logical(response)) stop('response is not logical')
   
    dtrain <- xgb.DMatrix(data =  mm[train,], label=response [train])
   
   cv <- xgb.cv(data = dtrain , nrounds = 2000,...,
                nthread = 4, nfold = 5, metrics = list("auc"), objective = "binary:logistic", early_stopping_rounds = 5)

   bst = xgboost(data = dtrain , ...,
                            nrounds = cv$best_iteration, objective = "binary:logistic")
   
    boost_test_pred <- predict(bst, mm[!train,])
    out = list(bst = bst)
    out$out_of_bag = data.frame(predicted = boost_test_pred , actual = response [!train])
    
    out$auc = auc(boost_test_pred, response [!train] )
    cat('auc =',    out$auc, '\n')
     
   if(plot_it) plot_boost_vars(bst, mm, 40)
    out
    }
    
boost_variables = function(model, mm, n = 40){
    ri = xgb.importance(colnames(mm), model = model)
    ri = data.frame(arrange(ri, -abs(Gain)))
    barplot(ri$Gain[1:n], horiz = TRUE, names.arg =  ri$Feature[1:n])
    ri
    }
