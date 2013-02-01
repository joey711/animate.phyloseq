################################################################################
# The core animation wrapper function. Assumes one wants to connect points
# of the same type with a line to designate their "path" during the time
# series.
################################################################################
#' General function for creating animated graphics from a \code{\link{data.frame}}
#'
#' Proper function depends entirely on the \code{\link[animation]{animation-package}}, and
#' details regarding customization options can be found in its documentation.
#' In particular, the \code{\link[animation]{ani.options}} function can be called
#' beforehand to make animnation-related adjustments, analogous to the way
#' \code{\link{par}} can be called for base graphics. Similarly, most 
#' arguments to \code{ani.options} can be provided as additional named arguments
#' and will be passed on to \code{ani.options} via \code{...}. 
#'
#' Since this is an animation, one of the most important arguments is \code{t},
#' which specifies the variable within the \code{data.frame} (\code{df}) that
#' represents time. This will be coerced to a \code{\link{numeric}}, and each
#' unique value will be treated as a separate time-step within the resulting
#' animation. Note that in most cases it should not be necessary to translate
#' your time measurement into time step integers, so long as the values of t
#' are coerceable
#' to a numeric that represents the correct order. If your time variable is 
#' a date represented by the \code{\link{Date}} class, it will probably work
#' fine. If in doubt, however, check that the following code provides the correct
#' \emph{order} of values.
#'
#' \code{as(df[, t], "numeric")}
#'
#' Similarly, the number of frames in the animation will be equal to
#'
#' \code{ length(unique(as(df[, t], "numeric"))) }
#'
#' so check that this value is what you need/expect.
#'
#' @usage animate(df, t=colnames(df)[1], x=colnames(df)[2], y=colnames(df)[3], 
#'		color=NULL, shape=NULL, ggplot2_expr=NULL, movie.name="animation.gif", ...)
#'
#' @param df (Required). A \code{\link{data.frame}} that contains covariates that
#'  can be mapped to time, as well as two additional axes.
#'
#' @param t (Recommended). A \code{\link{character-class}}. 
#'  The name of the time variable in \code{df}. Default
#'  is the name of the first column, \code{colnames(df)[1]}.
#'
#' @param x (Recommended). A \code{\link{character-class}}. 
#'  The name of the variable in \code{df} that will be mapped
#'  to the horizontal axis. Default
#'  is the name of the second column, \code{colnames(df)[2]}.
#'
#' @param y (Recommended). A \code{\link{character-class}}. 
#'  The name of the variable in \code{df} that will be mapped
#'  to the vertical axis. Default
#'  is the name of the third column, \code{colnames(df)[3]}.
#'
#' @param color (Optional). A \code{\link{character-class}}. 
#'  The name of the variable in \code{df} that will be mapped
#'  to color. Default is \code{NULL} (no color mapping).
#'
#' @param shape (Optional). A \code{\link{character-class}}. 
#'  The name of the variable in \code{df} that will be mapped
#'  to shape. Default is \code{NULL} (no shape mapping).
#' 
#' @param ggplot2_expr (Optional). A \code{\link{character-class}}. 
#'  A single (potentially long) character string that is a ggplot2 expression
#'  for additional graphical layers/options. For example, the following ggplot2
#'  expression, if provided, would cause each frame to be faceted in a wrap by the 
#'  \emph{Subject} factor with 3 rows, and also use the black and white theme:
#'
#' \code{"facet_wrap(~Subject, nrow=3) + theme_bw()"}
#'
#' @param movie.name (Optional). A character string of the desired name for 
#'  the output movie file. Default is \code{"animation.gif"}
#'
#' @param ... (Optional). Additional arguments passed to \code{\link[animation]{ani.options}}.
#'
#' @return A \code{.gif} animation file. Support is planned for other output
#'  file types supported by \code{\link{animation}}.
#' 
#' @seealso \code{\link{animation}}
#'
#' @references \url{http://cran.r-project.org/web/packages/animation/animation.pdf}
#' 
#' @export
#' @import animation
#' @import phyloseq
#' @examples
#' ##### Define arbitrary, simple "data"
#' # x=1:10; y=x; t=x; sdf <- data.frame(x, y, t)
#' ##### Make animation using animate() function:
#' # animate(sdf, "t", "x", "y")
#' ##### Try an alternative "by hand" using base graphics:
#' # saveMovie(	for(i in t){
#' #	plot(subset(sdf, t<=i)[, c("x", "y")], 
#' #			 xlim=c(min(sdf$x), max(sdf$x)),
#' #			 ylim=c(min(sdf$y), max(sdf$y))
#' #			 )
#' # })
#' #########
animate2 <- function(df, t=colnames(df)[1], x=colnames(df)[2], y=colnames(df)[3], 
		color=NULL, shape=NULL, ggplot2_expr=NULL, movie.name="animation.gif", ...){
	
	# require("animation"); require("ggplot2")		
	
	#######################################
	# Convert a numeric vector/factor into 
	# an integer vector of time steps, istep,
	# equal to number of rows in df.
	#######################################
	stepn <- as(df[, t], "numeric")
	istep <- (1:length(sort(unique(stepn))))[factor(stepn)]
	
	# call to saveMovie()
	saveMovie(
		# Loop on each integer in istep
		for (i in 1:max(istep) ) {
			# Initialize ggplot with data, no layer yet
			p <- ggplot2::ggplot(df) + 		
				# Create the core plot based on the subset of data up to the
				# current step, drawing path-only.
                # ggplot2::geom_path(
                #   ggplot2::aes_string(x=x, y=y, color=color, shape=shape), 
                #   data  = df[istep<=i, ],
                #   alpha = I(1)
                # ) + 
				# Set the ranges to be same for all plots
				ggplot2::xlim(range(df[, x])) + ggplot2::ylim(range(df[, y])) + 
				# Add the layer for the points at this time-step
				ggplot2::geom_point(
					aes_string(x=x, y=y, color=color, shape=shape),
					data = df[istep==i, ], 
					size = 3
				)
			# If there are additional ggplot2 options/layers to add, add them
			if( !is.null(ggplot2_expr) ){
				# # # # # cat(ggplot2_expr, fill=TRUE)
				p <- eval(parse( text = ggplot2::ps("p + ", ggplot2_expr) ))				
			}
			print(p)
		},
	movie.name, ...) # Close saveMovie
} # Close the animate() function definition
################################################################################
# wrapper/pipeline for making an animation on ordination data
# (basically formalize items from the animation example)
################################################################################
#' Create animations using ordination results
#'
#' An interesting subset of analyses of large phylogenetic sequencing projects
#' is to produce an ordination based on all of the samples in the project, and
#' then plot the relative ``movement'' of the samples (often microbial communities)
#' over time. This function is intended to produce just that. It expects that 
#' you will provide a \code{\link[phyloseq]{phyloseq-class}} object, an ordination
#' result based on that object, and the variable name of the ``time'' covariate
#' within the \code{\link[phyloseq]{sampleData-class}} component of the phyloseq
#' object. A great many parameters are passed on to \code{\link{animate}}, or
#' even further to \code{\link[animation]{ani.options}}.
#'
#' @usage animate_ordination(physeq, t, ord.result, axes = c(1, 2), color=NULL, shape=NULL, ggplot2_expr=NULL, movie.name="animation.gif", ...)
#'
#' @param physeq (Required). A \code{\link[phyloseq]{phyloseq-class}} that 
#'  contains a \code{\link[phyloseq]{sampleData-class}} component with at 
#'  least three covariates, one of which that can be mapped to ``time''
#'  (really sampling-order).
#'
#' @param t (Required). A \code{\link{character-class}}. 
#'  The name of the time variable in \code{physeq}. 
#'
#' @param ord.result (Required). An ordination result, as defined by the 
#'  \code{\link[vegan]{vegan-package}}.
#'  
#' @param axes (Optional). A two-element integer vector that defines the
#'  axes (aka coordinate, component, dimension) 
#'  within the ordination result that will be used. Default is to take the 
#'  first two axes (a good place to start). \code{c(1, 2)}
#'
#' @param color (Optional). A \code{\link{character-class}}. 
#'  The name of the variable in \code{physeq} that will be mapped
#'  to color. Default is \code{NULL} (no color mapping).
#'
#' @param shape (Optional). A \code{\link{character-class}}. 
#'  The name of the variable in \code{physeq} that will be mapped
#'  to shape. Default is \code{NULL} (no shape mapping).
#' 
#' @param ggplot2_expr (Optional). A \code{\link{character-class}}. 
#'  A single (potentially long) character string that is a ggplot2 expression
#'  for additional graphical layers/options. For example, the following ggplot2
#'  expression, if provided, would cause each frame to be faceted in a wrap by the 
#'  \emph{Subject} factor with 3 rows, and also use the black and white theme:
#'
#' \code{"facet_wrap(~Subject, nrow=3) + theme_bw()"}
#'
#' @param movie.name (Optional). A character string of the desired name for 
#'  the output movie file. Default is \code{"animation.gif"}
#'
#' @param ... (Optional). Additional arguments passed to \code{\link[animation]{ani.options}}.
#'
#' @return A \code{.gif} animation file. Support is planned for other output
#'  file types supported by \code{\link{animation}}.
#' 
#' @seealso \code{\link[animation]{animation}}, 
#'  \code{\link[phyloseq]{phyloseq-package}}
#'
#' @references \url{http://cran.r-project.org/web/packages/animation/animation.pdf}
#' 
#' @export
#' @import phyloseq
#' @importFrom vegan scores
#' @examples
#' ##### 
animate_ordination2 <- function(physeq, t, ord.result, axes = c(1, 2), color=NULL, shape=NULL, ggplot2_expr=NULL, movie.name="animation.gif", ...){
	# Now grab the scores from the first two axes of that result
	ord.co  <- scores(ord.result, choices=axes, "sites")
	x <- colnames(ord.co)[1]
	y <- colnames(ord.co)[2]
	
	### Now combine the coordinates with the rest of the sampleData
	sdf <- as(sampleData(physeq), "data.frame")
	# check indices match between ord.co and sampleData
	if( !all(rownames(sdf)==rownames(ord.co)) ){
		stop("rownames of sampleData(physeq) do not match ordination result sites (ord.res)")
	}
	sdf <- data.frame(ord.co, sdf)	

	# Call the more general animate() wrapper with sampleData provided as df,
	#  and including the ordination axes as x, y
	animate(sdf, t, x, y, color, shape, ggplot2_expr, movie.name, ...)
}
################################################################################
################################################################################
