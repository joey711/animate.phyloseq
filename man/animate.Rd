\name{animate}
\alias{animate}
\title{General function for creating animated graphics from a \code{\link{data.frame}}}
\usage{
  animate(df, t=colnames(df)[1], x=colnames(df)[2],
    y=colnames(df)[3], color=NULL, shape=NULL,
    ggplot2_expr=NULL, movie.name="animation.gif", ...)
}
\arguments{
  \item{df}{(Required). A \code{\link{data.frame}} that
  contains covariates that can be mapped to time, as well
  as two additional axes.}

  \item{t}{(Recommended). A \code{\link{character-class}}.
  The name of the time variable in \code{df}. Default is
  the name of the first column, \code{colnames(df)[1]}.}

  \item{x}{(Recommended). A \code{\link{character-class}}.
  The name of the variable in \code{df} that will be mapped
  to the horizontal axis. Default is the name of the second
  column, \code{colnames(df)[2]}.}

  \item{y}{(Recommended). A \code{\link{character-class}}.
  The name of the variable in \code{df} that will be mapped
  to the vertical axis. Default is the name of the third
  column, \code{colnames(df)[3]}.}

  \item{color}{(Optional). A \code{\link{character-class}}.
  The name of the variable in \code{df} that will be mapped
  to color. Default is \code{NULL} (no color mapping).}

  \item{shape}{(Optional). A \code{\link{character-class}}.
  The name of the variable in \code{df} that will be mapped
  to shape. Default is \code{NULL} (no shape mapping).}

  \item{ggplot2_expr}{(Optional). A
  \code{\link{character-class}}.  A single (potentially
  long) character string that is a ggplot2 expression for
  additional graphical layers/options. For example, the
  following ggplot2 expression, if provided, would cause
  each frame to be faceted in a wrap by the \emph{Subject}
  factor with 3 rows, and also use the black and white
  theme:

  \code{"facet_wrap(~Subject, nrow=3) + theme_bw()"}}

  \item{movie.name}{(Optional). A character string of the
  desired name for the output movie file. Default is
  \code{"animation.gif"}}

  \item{...}{(Optional). Additional arguments passed to
  \code{\link[animation]{ani.options}}.}
}
\value{
  A \code{.gif} animation file. Support is planned for
  other output file types supported by
  \code{\link{animation}}.
}
\description{
  Proper function depends entirely on the
  \code{\link[animation]{animation-package}}, and details
  regarding customization options can be found in its
  documentation. In particular, the
  \code{\link[animation]{ani.options}} function can be
  called beforehand to make animnation-related adjustments,
  analogous to the way \code{\link{par}} can be called for
  base graphics. Similarly, most arguments to
  \code{ani.options} can be provided as additional named
  arguments and will be passed on to \code{ani.options} via
  \code{...}.
}
\details{
  Since this is an animation, one of the most important
  arguments is \code{t}, which specifies the variable
  within the \code{data.frame} (\code{df}) that represents
  time. This will be coerced to a \code{\link{numeric}},
  and each unique value will be treated as a separate
  time-step within the resulting animation. Note that in
  most cases it should not be necessary to translate your
  time measurement into time step integers, so long as the
  values of t are coerceable to a numeric that represents
  the correct order. If your time variable is a date
  represented by the \code{\link{Date}} class, it will
  probably work fine. If in doubt, however, check that the
  following code provides the correct \emph{order} of
  values.

  \code{as(df[, t], "numeric")}

  Similarly, the number of frames in the animation will be
  equal to

  \code{ length(unique(as(df[, t], "numeric"))) }

  so check that this value is what you need/expect.
}
\examples{
##### Define arbitrary, simple "data"
# x=1:10; y=x; t=x; sdf <- data.frame(x, y, t)
##### Make animation using animate() function:
# animate(sdf, "t", "x", "y")
##### Try an alternative "by hand" using base graphics:
# saveMovie(	for(i in t){
#	plot(subset(sdf, t<=i)[, c("x", "y")],
#			 xlim=c(min(sdf$x), max(sdf$x)),
#			 ylim=c(min(sdf$y), max(sdf$y))
#			 )
# })
#########
}
\references{
  \url{http://cran.r-project.org/web/packages/animation/animation.pdf}
}
\seealso{
  \code{\link{animation}}
}

