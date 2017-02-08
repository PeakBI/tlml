
roc <- function(predictions, labels, na.label = FALSE, na.action = na.omit){
    library(binom)
	roc.data <- data.frame(predictions = predictions, labels = labels)
	roc.data <- roc.data[ order(-predictions) ,  ]
	roc.data <-na.action(roc.data)
	n <- nrow(roc.data)
	
	TPR <- double(n)
	FPR <- double(n)
	test.res <- logical(n)
	i <- 1
	for(i in 1:n){
			test.res[1:i] <- TRUE
			test.res[-(1:i)] <- FALSE
			test.res[is.na(roc.data$predictions)] <- na.label
			
			nPos <- sum(roc.data$labels)
			nNeg <- sum(!roc.data$labels)
			
			nTruePos <- sum(test.res & roc.data$labels)
			nFalseNeg <- sum(!test.res & roc.data$labels)
			TPR[i] <- nTruePos / nPos
			
			nTrueNeg <- sum(!test.res & !roc.data$labels)
			nFalsePos <- sum(test.res & !roc.data$labels)
			FPR[i] <- nFalsePos/ nNeg

		}
	return(data.frame(predictions = roc.data$predictions ,
		labels = roc.data$labels, TPR, FPR))
}

plot_roc <- function(roc,CI = FALSE, add = FALSE,...){
	#mp(mfrow = c(2,1))
	#box.dot.plot(predictions ~ labels - 1,roc, ...)
    if(!add){
        plot(y = roc$TPR, x = 1 - roc$FPR, type = 'l', 
            ylab = 'Sensitivity', 
            xlab = 'Specificty',	
            xlim = c(0,1), ylim = c(0,1), xaxp = c(0,1,10), yaxp = c(0,1,10), xaxs = 'i', yaxs = 'i', ...)
        abline(a = 1, b = -1, lty = 2, h = 1, v = 1)
        }
     else {
        lines(y = roc$TPR, x = 1 - roc$FPR, ...)
     }
	}

	
roc.log <- function(logmodel, ...){
	predictions <- predict(logmodel, type = 'response')
	labels <- logmodel$model[[1]]
	roc(predictions , -as.numeric(labels) + 2,... )
	}
	
