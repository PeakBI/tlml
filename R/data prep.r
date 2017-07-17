is_date_ish = function(x, orders = c("ymd", "ymd HMS"), ...){
  oldw <- getOption("warn")
  options(warn = -1)
  
  try_date = parse_date_time(x, orders = orders , ...)
  options(warn = oldw)
  all_na = all(is.na(try_date))
  !all_na
}

make_diff_dates = function(df, ...){
  library(lubridate)
  

  
  to_date = function(x, orders = c("ymd", "ymd HMS"), ... ){
    oldw <- getOption("warn")
    options(warn = -1)
    d = parse_date_time(x, orders = orders, ... )
    options(warn = oldw)
    as.Date(d)
  }
  i_date = map_lgl(df, is_date_ish,  ...)
  date_df = map_df(df[i_date ] , to_date, ...)
  date_df = map_df( date_df, as.numeric) 
  dates_matrix = as.matrix(date_df)
  
  number_of_cols = ncol(dates_matrix)
  number_of_rows = nrow(dates_matrix)
  
  diff_dates = matrix(ncol = number_of_cols^2, nrow = number_of_rows)
  dimnames(diff_dates) <- list(rownames(diff_dates, do.NULL = FALSE, prefix = "row"),
                               colnames(diff_dates, do.NULL = FALSE, prefix = "col"))
  new_names =  matrix(data = '', nrow = number_of_cols^2, ncol = 2, byrow = FALSE, dimnames = NULL)
  
  
  index = 0
  for (i in 1:number_of_cols){
    for(j in 1:number_of_cols){
      index = index + 1
      diff_dates [,index] = as.integer(dates_matrix[,i] - dates_matrix[,j])
      new_names[index,1] =  colnames(dates_matrix)[i] 
      new_names[index,2] =  colnames(dates_matrix)[j] 
    }
  }
  
  colnames(diff_dates) = paste(new_names[,1] , new_names[,2] , sep = '_diff_')
  
  diff_dates = as.data.frame(diff_dates)
  diff_dates
  
}

poly_df = function(x, degree  = 3){

    poly_fitter = function(x, degree){

        if(is.numeric(x)) return( poly(x, degree = degree, raw = TRUE))
        else return(x)
    }

    out = llply(x,   poly_fitter, degree = degree)
    out = as.data.frame(out)
    names(out) = clean_names(out)
    out

    }

set_ref = function(df){

     out = llply(df, function(.x){
            if(is.factor(.x)) .x = set_ref_most_common (.x)
            return(.x)
            })
     as.data.frame(out)
    }

like_logical = function(x){
    is.logical(x) | length(unique(x)) == 2
    }

to_logical = function(x){
    if(!like_logical(x)) stop('More than 2 levels')
    x = factor(x)
    levels(x) = c('FALSE', 'TRUE')
    x = as.logical(x)
    x
    }

left_roll_join = function(x, y, x_index, y_index = x_index, x_date, y_date, roll = Inf){
    library(data.table)

    x = mutate_(x, roll_date = x_date)
    y = mutate_(y, roll_date = y_date)

    x_dt = data.table(x, key = c(x_index,'roll_date'))
    y_dt = data.table(y, key = c(y_index,'roll_date'))

    out = y_dt[x_dt , roll = roll]
    data.frame(out)
    }

df_to_mm  = function(df, na.action = na.omit){
	library(Matrix)
	#df = na.action(df)
	factors = laply(df, function(.x) (is.factor(.x) | is.character(.x) )  & !like_logical(.x))
    i_logical = laply(df,  like_logical)

    #TODO convert 2 level factor to logical
    df[i_logical] = llply(   df[ i_logical] , to_logical)


	numeric_or_logical = laply(df, function(.x) is.numeric(.x) | like_logical(.x) )


	if(any(factors)){
		df_factors = df[,factors, drop = FALSE]
        df_factors[] = llply(df_factors, as.factor)
		n <- nrow(df_factors)
		nlevels <- sapply(df_factors, nlevels)
		i <- rep(seq_len(n), ncol(df_factors))
		j <- unlist(lapply(df_factors, as.integer)) +
			 rep(cumsum(c(0, head(nlevels, -1))), each = n)
		x <- 1
		col_names = c(unlist(lapply(df_factors, levels)))
		col_names = paste(names(col_names ), col_names, sep = ':')

		mm_factors = sparseMatrix(i = i, j = j, x = x, dimnames = list(rownames(df_factors), col_names))

		if(any(numeric_or_logical )){
		mm_numeric = Matrix(data.matrix(df[,numeric_or_logical ]), sparse = TRUE)
		mm = cbind2(mm_factors, mm_numeric )
		}
	}

	else mm = Matrix(data.matrix(df[,numeric_or_logical]), sparse = TRUE)

    if(any(is.na(mm))) warning('mm contains missing values')
    return(mm)
	}

remove_correlated = function(mm, cor_max = 0.98){
    #remove any colums from mm that are correlated (almost) perfectly with another one)
    cor_matrix = lower_tri_cor(mm)
    i_cor = which(aaply(cor_matrix, 1, function(.x) any(.x > cor_max)))
    if(length(i_cor) == 0) {
        cat('Removed 0 correlated variables\n')
        return(mm)
        }
    cat('Removed ', length(i_cor), ' correlated variables\n')
    mm = mm = mm[, -i_cor]
    mm
   }

remove_sparse_terms <- function(mm, n = 1){
    keep = colSums(mm) > n
    cat('Removed ', sum(!keep), ' sparse variables\n')
    mm[,keep]
	}

prop_na = function(x){
     sum(is.na(x)) / length(x)
     }

all_same = function(x){
    length(unique(x)) == 1
    }

clean_data = function(df, column_missing = 0.25, row_missing = 0.5, min_max_level = 10, verbose = 0, trim = 30, parallel = FALSE,
                      numeric_clean_type = c('all', 'impute_only', 'none', 'skew_params'), remove_all_same = TRUE, skew_params = NULL){

    #remove columns column_missing % missing
    i_column_missing =  laply(df,  prop_na) > column_missing


    if(verbose == 1) {
      cat('Removed ', sum(i_column_missing), ' colums with more than ', row_missing, ' % missing data\n')
      cat('removed', paste(names( df[ , i_column_missing]  ), collapse = '\n'))
    }

    data_clean =  df[ , !i_column_missing]


    #Remove rows with row_missing % missing

    # Replace mising categorical variables with most common
    #these methods could be imptoved...
    i_char = laply(data_clean, function(.x) is.factor(.x) | is.character(.x) | is.logical(.x))
    
    #remove variables with less than min_max_level for max level
    i_no_variation = laply(data_clean[i_char], function(.x){
      max_level(.x) < min_max_level
    })
    
    data_clean[i_char] = llply( data_clean[i_char], mode_impute)
    
    categorical_data = data_clean[,i_char] [,!i_no_variation]
    cat('Removed ', sum(i_no_variation), ' catergorical colums with no variation\n')
    


    

    # clean numeric var5iables
    i_num = laply( data_clean, is.numeric)

    if(numeric_clean_type == 'all'){
      clean_numeric_list = llply( data_clean[ i_num], clean_numeric, trim = trim, .parallel = parallel,
                                  .paropts=list(.packages=.packages(all.available=T)))


      skew_params = llply(clean_numeric_list, function(.x) attributes(.x)$skew_params)

      data_clean[ i_num] = clean_numeric_list


    }
    else if (numeric_clean_type == 'impute_only'){
      data_clean[i_num] = llply( data_clean[ i_num], median_impute)

    }
    else if (numeric_clean_type == 'none'){
      data_clean[ i_num] = data_clean[ i_num]
    }
    else if (numeric_clean_type == 'skew_params'){

      numeric_names = names(data_clean[ i_num])
      common_names = intersect(names(skew_params),  numeric_names)

      for(.var in common_names){
        data_clean[i_num][[.var]] = clean_numeric_to (data_clean[i_num][[.var]], skew_params[[.var]])
      }

      new_vars = !numeric_names %in% names(skew_params)

      clean_numeric_list = llply( data_clean[ i_num][new_vars], clean_numeric, trim = trim, .parallel = parallel,
                                  .paropts=list(.packages=.packages(all.available=T)))

      skew_params = c(skew_params, llply(clean_numeric_list, function(.x) attributes(.x)$skew_params))

      data_clean[i_num][new_vars] = clean_numeric_list

    }
    else stop('no valid numeric type selected')



    data_clean = cbind(categorical_data, data_clean[ i_num])

    if(remove_all_same){
    #remove colums with no variation
    i_all_same = laply(data_clean,  all_same)
    cat('Removed ', sum(i_all_same), ' colums with no variation\n')
    data_clean =  data_clean[ , !i_all_same]
    }

    if(exists ('skew_params')) attr(data_clean, 'skew_params') <- skew_params
    data_clean
    }

lower_tri_cor <- function(x, ...){
	#Returns the lower triangular correlation matrix of x
    if(!is.matrix(x)) CorMat <- sparse_cor(x = x)
    else CorMat <- cor(x, ...)
	diag(CorMat) <- 0
	CorMat[upper.tri(CorMat) ] <- 0
	CorMat
}

sparse_cor <- function(x){

    n <- nrow(x)

    cMeans <- Matrix::colMeans(x)
    cSums <- Matrix::colSums(x)

    # Calculate the population covariance matrix.
    # There's no need to divide by (n-1) as the std. dev is also calculated the same way.
    # The code is optimized to minize use of memory and expensive operations
    covmat <- tcrossprod(cMeans, (-2*cSums+n*cMeans))
    crossp <- as.matrix(Matrix::crossprod(x))
    covmat <- covmat+crossp

    sdvec <- sqrt(diag(covmat)) # standard deviations of columns
    covmat/crossprod(t(sdvec)) # correlation matrix
}

median_impute <- function(x){
	m <- median(x, na.rm = TRUE)
	x[is.na(x)] <- m
	x
	}

mode_factor = function(x){
    i_na = is.na(x)
    if(require("tomr")){
        table_of_levels = t_dt(x[!i_na ])
        }
    else   table_of_levels = table(x[!i_na ])
    mode_level = names(which.max(table_of_levels ))
    mode_level
    }

set_ref_most_common = function(x){
    most_common_level = mode_factor (x)
    relevel(x, ref =  most_common_level)
    }

mode_impute = function(x){
    i_na = is.na(x)
    mode_level = mode_factor(x)
    x[i_na ] <-  mode_level
    x
    }

max_level = function(x){
    table_of_levels = table(x)
    max(table_of_levels )
    }

clean_numeric = function(x, trim = Inf){
	out = x %>%
        median_impute()%>%
        minimise_skew ()
	
	skew_params = out[[2]]
	skew_params$mad = mad(out[[1]])
	skew_params$median = median(out[[1]])
	skew_params$sd = sd(out[[1]])

	out = robust_scale(out[[1]])



    #trim extreme outliers
    if(trim < Inf){
        i_trim_high = out > trim
        out[i_trim_high] = max(out[!i_trim_high])

        i_trim_low = out < -trim
        out[i_trim_low] = min(out[!i_trim_low])
	}
    attr(out, 'skew_params') <- skew_params
    out
}


clean_numeric_to = function(x, params){
  out = median_impute(x)
  
  #minimise skew
  alpha = params$par[1]
  beta = params$par[2]

  out = (out + alpha)^beta
  
  
  
  if(beta < 0) out = -out
  
  if(abs(beta) < 1e-10 ) {
    shifted <- out + alpha
    
    if (min(shifted) > 0){
      out = log(shifted)
    }
    else
      out = log(shifted - min(shifted)*2 )
  }

  #scale
  if(params$mad != 0){
    out <- (out - params$median)/params$mad
  }
  else if (!is.null(params$sd)){
    out <- (out - params$median)/params$sd
  }
  else out = out - params$median

  out

}


minimise_skew <- function(x){
	# x is a numeric vector
	# This function transfomrs x -> (x + alpha)^beta
	# Choosing alpha and beta so as to minismie skew measured using mc() from the robustbase package
    # this is fairly slow
	require(robustbase)

	min.skew <- function(param = c(0,1), x){
		alpha = param[1]
		beta = param[2]
		shifted <- x + alpha
		if(abs(beta) < 1e-10 ) {
			if (min(shifted) > 0){
				return( abs(mc(log(shifted) )))
			}
			else return (abs(mc(log(shifted - min(shifted)*2 ))))
		}
		abs(mc( (shifted)^beta ))
	}

	opt <- optim(par = c(0,1), fn = min.skew, x = na.omit(x))
	alpha = opt$par[1]
	beta = opt$par[2]

	shifted <- x + alpha
	x.trans = shifted ^ beta

	#If beta is negative  need to reverse var
	if(beta < 0) x.trans = -x.trans

	#if abs(beta) is ~ 0 use log transform
	if(abs(beta) < 1e-10 ) {
			if (min(shifted) > 0){
				x.trans = log(shifted)
			}
			else
			x.trans = log(shifted - min(shifted)*2 )
		}


	list(x.trans = x.trans, opt = opt)
}

robust_scale <- function (x){
	#Scales x to median  = 0 and mad = 1 (unless mad = 0 then used sd = 1)
	m = mad(x)
	if(m == 0) m = sd(x)
	x <- (x - median(x))/m
	x
}

scale_to = function(x, to) {
  ##scales x to the mean and sd of to
  x <- as.matrix(x)
  to <- as.matrix(to)
  if(dim(x)[2] != dim(to)[2]) stop("X and to do not have the same number of columns")
  x <- robust_scale(x)
  x[is.nan(x)] <- 0  #replaces any unchanging colums with zeros
  out <-  sweep(x, 2, sd(to), "*")
  out <- sweep(out, 2, colMeans(to), "+")
  out
  }





