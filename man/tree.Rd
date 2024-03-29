% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/myStoSOO.R
\name{tree}
\alias{tree}
\title{R6 Class Representing a tree for StoSOO}
\description{
tree object store specific information on tree for StoSOO
}
\section{Public fields}{
\if{html}{\out{<div class="r6-fields">}}
\describe{
\item{\code{xMinRoot}}{[numeric] Minimum value of the root node (cell) in the tree}

\item{\code{xMaxRoot}}{[numeric] Maximum value of the root node (cell) in the tree}

\item{\code{xRepresentativeRoot}}{[numeric] Representative value of the root node (cell) in the tree}

\item{\code{paramLen}}{[numeric] Number of parameters}

\item{\code{nMaxEvalPerNode}}{[numeric] Maximum number of evaluations per leaf}

\item{\code{widthBase}}{[numeric] Base of width of the estimates of rewards}

\item{\code{maximizeFunc}}{[function] Function to be maximized given parameters scaled from 0 to 1.}

\item{\code{funcScale}}{[numeric] Scale for function to be optimized. If `maximize = TRUE`, `funcScale = 1`, and else `funcScale = -1`.}

\item{\code{optimizeType}}{[character] Either 'deterministic' for optimizing a deterministic function or 'stochastic' for a stochastic one}

\item{\code{nChildrenPerExpansion}}{[numeric] Number of children per expansion}

\item{\code{maxDepth}}{[numeric] Maximum depth of the tree}

\item{\code{withCheck}}{[logical] Check arguments for `node`, `layer`, and `tree` class or not}

\item{\code{verbose}}{[logical] Display information}

\item{\code{iterationCounter}}{[numeric] Number of evaluations for the target function done in this tree object}

\item{\code{rootNode}}{[numeric] Root node information (`node` class object)}

\item{\code{rootLayer}}{[numeric] Root layer information (`layer` class object)}

\item{\code{layersList}}{[numeric] List of layers (`layer` class object) in this tree}

\item{\code{depths}}{[numeric] Depths of the layers in this tree}

\item{\code{maxUcbValuesSoFar}}{[numeric] Maximum UCB value for the layers shallower than this layer}
}
\if{html}{\out{</div>}}
}
\section{Active bindings}{
\if{html}{\out{<div class="r6-active-bindings">}}
\describe{
\item{\code{evaluateCurrentOptimalNode}}{[node] Evaluate the optimal node in the current tree}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-new}{\code{tree$new()}}
\item \href{#method-performOneUpdate}{\code{tree$performOneUpdate()}}
\item \href{#method-print}{\code{tree$print()}}
\item \href{#method-clone}{\code{tree$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-new"></a>}}
\if{latex}{\out{\hypertarget{method-new}{}}}
\subsection{Method \code{new()}}{
Create a new tree object
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{tree$new(
  xMinRoot = NULL,
  xMaxRoot = NULL,
  xRepresentativeRoot = NULL,
  paramLen = NULL,
  nMaxEvalPerNode,
  widthBase,
  maximizeFunc,
  funcScale,
  optimizeType,
  nChildrenPerExpansion,
  maxDepth,
  withCheck = FALSE,
  verbose = TRUE
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{xMinRoot}}{[numeric] Minimum value of the root node (cell) in the tree}

\item{\code{xMaxRoot}}{[numeric] Maximum value of the root node (cell) in the tree}

\item{\code{xRepresentativeRoot}}{[numeric] Representative value of the root node (cell) in the tree}

\item{\code{paramLen}}{[numeric] Number of parameters}

\item{\code{nMaxEvalPerNode}}{[numeric] Maximum number of evaluations per leaf}

\item{\code{widthBase}}{[numeric] Base of width of the estimates of rewards}

\item{\code{maximizeFunc}}{[function] Function to be maximized given parameters scaled from 0 to 1.}

\item{\code{funcScale}}{[numeric] Scale for function to be optimized. If `maximize = TRUE`, `funcScale = 1`, and else `funcScale = -1`.}

\item{\code{optimizeType}}{[character] Either 'deterministic' for optimizing a deterministic function or 'stochastic' for a stochastic one}

\item{\code{nChildrenPerExpansion}}{[numeric] Number of children per expansion}

\item{\code{maxDepth}}{[numeric] Maximum depth of the tree}

\item{\code{withCheck}}{[logical] Check arguments for `node`, `layer`, and `tree` class or not}

\item{\code{verbose}}{[logical] Display information}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-performOneUpdate"></a>}}
\if{latex}{\out{\hypertarget{method-performOneUpdate}{}}}
\subsection{Method \code{performOneUpdate()}}{
Update the tree information once
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{tree$performOneUpdate()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-print"></a>}}
\if{latex}{\out{\hypertarget{method-print}{}}}
\subsection{Method \code{print()}}{
Display information about the object
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{tree$print()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-clone"></a>}}
\if{latex}{\out{\hypertarget{method-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{tree$clone(deep = FALSE)}\if{html}{\out{</div>}}
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
