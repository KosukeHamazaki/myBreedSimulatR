% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/myStoSOO.R
\name{stoSOO}
\alias{stoSOO}
\title{R6 Class Representing a function optimizer by StoSOO (Stochastic Simultaneous Optimistic Optimization)}
\description{
stoSOO object store specific information on global optimization for stochastic functions given a finite number of budget (StoSOO).
}
\references{
R. Munos (2011), Optimistic optimization of deterministic functions without the knowledge of its smoothness,
\emph{NIPS}, 783-791. \cr \cr
M. Valko, A. Carpentier and R. Munos (2013), Stochastic Simultaneous Optimistic Optimization,
\emph{ICML}, 19-27 \url{http://hal.inria.fr/hal-00789606}. Matlab code: \url{https://team.inria.fr/sequel/software/StoSOO}. \cr \cr
P. Preux, R. Munos, M. Valko (2014), Bandits attack function optimization, \emph{IEEE Congress on Evolutionary Computation (CEC)}, 2245-2252.
}
\section{Public fields}{
\if{html}{\out{<div class="r6-fields">}}
\describe{
\item{\code{parameter}}{[vector] Vector with length defining the dimensionality of the optimization problem. Providing actual values of par is not necessary (NAs are just fine).}

\item{\code{paramLen}}{[numeric] Number of parameters}

\item{\code{optimizeFunc}}{[function] Scalar function to be optimized, with first argument to be optimized over}

\item{\code{lowerBound}}{[numeric] Vectors of lower bounds on the variables}

\item{\code{upperBound}}{[numeric] Vectors of upper bounds on the variables}

\item{\code{nIterOptimization}}{[numeric] Number of function evaluations allocated to optimization}

\item{\code{nMaxEvalPerNode}}{[numeric] Maximum number of evaluations per leaf}

\item{\code{maxDepth}}{[numeric] Maximum depth of the tree}

\item{\code{nChildrenPerExpansion}}{[numeric] Number of children per expansion}

\item{\code{confidenceParam}}{[numeric] Confidence parameter (see Valko et al., 2013)}

\item{\code{maximize}}{[logical] If TRUE, performs maximization}

\item{\code{optimizeType}}{[character] Either 'deterministic' for optimizing a deterministic function or 'stochastic' for a stochastic one}

\item{\code{returnOptimalNodes}}{[numeric] When (how many iterations) to return (or save) the optimal nodes when optimizing hyper parameter by StoSOO}

\item{\code{saveTreeNameBase}}{[character] Base name of the tree to be saved}

\item{\code{whenToSaveTrees}}{[numeric] When (how many iterations) to save the tree in StoSOO}

\item{\code{withCheck}}{[logical] Check arguments for `node`, `layer`, and `tree` class or not}

\item{\code{verbose}}{[logical] Display information}

\item{\code{widthBase}}{[numeric] Base of width of the estimates of rewards}

\item{\code{funcScale}}{[numeric] Scale for function to be optimized. If `maximize = TRUE`, `funcScale = 1`, and else `funcScale = -1`.}

\item{\code{maximizeFunc}}{[function] Function to be maximized given parameters scaled from 0 to 1.}

\item{\code{currentTree}}{[tree class] `tree` class object for the current status}

\item{\code{optimalNodes}}{[list] List of optimal nodes (`node` class object) corresponding to `returnOptimalNodes`}

\item{\code{optimalNodeFinal}}{[node class] Optimal node (`node` class object) for the final tree}

\item{\code{optimalParameter}}{[numeric] Optimal parameter estimated by StooSOO given a finite number of evaluations}

\item{\code{optimalValue}}{[numeric] Optimal value estimated by StooSOO given a finite number of evaluations}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-new}{\code{stoSOO$new()}}
\item \href{#method-startOptimization}{\code{stoSOO$startOptimization()}}
\item \href{#method-print}{\code{stoSOO$print()}}
\item \href{#method-clone}{\code{stoSOO$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-new"></a>}}
\if{latex}{\out{\hypertarget{method-new}{}}}
\subsection{Method \code{new()}}{
Create a new stoSOO object.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{stoSOO$new(
  parameter,
  optimizeFunc,
  ...,
  lowerBound = NULL,
  upperBound = NULL,
  nIterOptimization = NULL,
  nMaxEvalPerNode = NULL,
  maxDepth = NULL,
  nChildrenPerExpansion = NULL,
  confidenceParam = NULL,
  maximize = NULL,
  optimizeType = NULL,
  returnOptimalNodes = NULL,
  saveTreeNameBase = NULL,
  whenToSaveTrees = NA,
  withCheck = FALSE,
  verbose = TRUE
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{parameter}}{[vector] Vector with length defining the dimensionality of the optimization problem. Providing actual values of par is not necessary (NAs are just fine).}

\item{\code{optimizeFunc}}{[function] Scalar function to be optimized, with first argument to be optimized over}

\item{\code{...}}{[logical/numeric/character/etc...] Optional additional arguments to `optimizeFunc`}

\item{\code{lowerBound}}{[numeric] Vectors of lower bounds on the variables}

\item{\code{upperBound}}{[numeric] Vectors of upper bounds on the variables}

\item{\code{nIterOptimization}}{[numeric] Number of function evaluations allocated to optimization}

\item{\code{nMaxEvalPerNode}}{[numeric] Maximum number of evaluations per leaf}

\item{\code{maxDepth}}{[numeric] Maximum depth of the tree}

\item{\code{nChildrenPerExpansion}}{[numeric] Number of children per expansion}

\item{\code{confidenceParam}}{[numeric] Confidence parameter (see Valko et al., 2013)}

\item{\code{maximize}}{[logical] If TRUE, performs maximization}

\item{\code{optimizeType}}{[character] Either 'deterministic' for optimizing a deterministic function or 'stochastic' for a stochastic one}

\item{\code{returnOptimalNodes}}{[numeric] When (how many iterations) to return (or save) the optimal nodes when optimizing hyper parameter by StoSOO}

\item{\code{saveTreeNameBase}}{[character] Base name of the tree to be saved}

\item{\code{whenToSaveTrees}}{[numeric] When (how many iterations) to save the tree in StoSOO}

\item{\code{withCheck}}{[logical] Check arguments for `node`, `layer`, and `tree` class or not}

\item{\code{verbose}}{[logical] Display information}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-startOptimization"></a>}}
\if{latex}{\out{\hypertarget{method-startOptimization}{}}}
\subsection{Method \code{startOptimization()}}{
start global optimization of stochastic function by StoSOO
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{stoSOO$startOptimization()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-print"></a>}}
\if{latex}{\out{\hypertarget{method-print}{}}}
\subsection{Method \code{print()}}{
Display information about the object
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{stoSOO$print()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-clone"></a>}}
\if{latex}{\out{\hypertarget{method-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{stoSOO$clone(deep = FALSE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{deep}}{Whether to make a deep clone.}
}
\if{html}{\out{</div>}}
}
}
}
