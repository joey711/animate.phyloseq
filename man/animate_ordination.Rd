\name{animate_ordination}
\alias{animate_ordination}
\title{Create animations using ordination results}
\usage{
  animate_ordination(physeq, t, ord.result, axes = c(1, 2),
    color=NULL, shape=NULL, ggplot2_expr=NULL,
    movie.name="animation.gif", ...)
}
\arguments{
  \item{physeq}{(Required). A
  \code{\link[phyloseq]{phyloseq-class}} that contains a
  \code{\link[phyloseq]{sampleData-class}} component with
  at least three covariates, one of which that can be
  mapped to ``time'' (really sampling-order).}

  \item{t}{(Required). A \code{\link{character-class}}.
  The name of the time variable in \code{physeq}.}

  \item{ord.result}{(Required). An ordination result, as
  defined by the \code{\link[vegan]{vegan-package}}.}

  \item{axes}{(Optional). A two-element integer vector that
  defines the axes (aka coordinate, component, dimension)
  within the ordination result that will be used. Default
  is to take the first two axes (a good place to start).
  \code{c(1, 2)}}

  \item{color}{(Optional). A \code{\link{character-class}}.
  The name of the variable in \code{physeq} that will be
  mapped to color. Default is \code{NULL} (no color
  mapping).}

  \item{shape}{(Optional). A \code{\link{character-class}}.
  The name of the variable in \code{physeq} that will be
  mapped to shape. Default is \code{NULL} (no shape
  mapping).}

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
  An interesting subset of analyses of large phylogenetic
  sequencing projects is to produce an ordination based on
  all of the samples in the project, and then plot the
  relative ``movement'' of the samples (often microbial
  communities) over time. This function is intended to
  produce just that. It expects that you will provide a
  \code{\link[phyloseq]{phyloseq-class}} object, an
  ordination result based on that object, and the variable
  name of the ``time'' covariate within the
  \code{\link[phyloseq]{sampleData-class}} component of the
  phyloseq object. A great many parameters are passed on to
  \code{\link{animate}}, or even further to
  \code{\link[animation]{ani.options}}.
}
\examples{
#####
}
\references{
  \url{http://cran.r-project.org/web/packages/animation/animation.pdf}
}
\seealso{
  \code{\link[animation]{animation}},
  \code{\link[phyloseq]{phyloseq-package}}
}

