
#' @export
#' @import SummarizedExperiment
#' @import SummarizedExperiment
#' @title  Method to get the circuit
#' @description The circuit file should contain three columns with headers,
#' "Source" "Target" "Type"
#' Here "Source" and "Target" are the names of the genes and "Type" refers to
#' the regulation, "1" if source activates target and "2" if source inhibits
#' target.
#' @param .object RacipeSE object
#'
#' @return A dataframe
#' @examples
#' rs <- RacipeSE()
#' data("demoCircuit")
#' sracipeCircuit(rs) <- demoCircuit
#' circuitDataFrame <- sracipeCircuit(rs)
#' rm(rs, demoCircuit,circuitDataFrame)
#'

setGeneric(name="sracipeCircuit",
           def=function(.object)
           {
             standardGeneric("sracipeCircuit")
           }
)


#' @export
#' @title Initialize the circuit
#' @description Initialize the circuit from a topology file
#' or a \code{data.frame}
#' A typical topology file looks like
#' \tabular{lcr}{
#'   Source \tab Target \tab Type \cr
#'   geneA \tab geneB \tab 2 \cr
#'   geneB \tab geneC \tab 1 \cr
#'   geneB \tab geneA \tab 2
#' }
#' Here the regulation type is specified by number - activation: \code{1},
#'  inhibition: \code{2}
#' @param .object RacipeSE object
#' @param value data.frame containing the circuit information
#' @return data.frame
#' @examples
#' RacipeSet <- RacipeSE()
#' data("demoCircuit")
#' sracipeCircuit(RacipeSet) <- demoCircuit
#' sracipeCircuit(RacipeSet)
#' rm(RacipeSet, demoCircuit)
#' @section Related Functions:
#'
#' \code{\link{sracipeSimulate}},  \code{\link{sracipeKnockDown}},
#' \code{\link{sracipeOverExp}},  \code{\link{sracipePlotData}}
#'

setGeneric("sracipeCircuit<-",
           def = function(.object, value)
           {
             standardGeneric("sracipeCircuit<-")
           }
)

#' @export
#' @import SummarizedExperiment
#' @title  A method to access the simulation hyperparameters
#' @description The hyperparameters like number of models, range from which
#' parameters are to be sampled, simulation time etc.
#' @param .object RacipeSE object
#' @examples
#' RacipeSet <- RacipeSE()
#' data("demoCircuit")
#' sracipeCircuit(RacipeSet) <- demoCircuit
#' sracipeConfig(RacipeSet)
#' rm(RacipeSet)
#' @return list
#'

setGeneric(name="sracipeConfig",
           def=function(.object)
           {
             standardGeneric("sracipeConfig")
           }
)

#' @export
#' @import SummarizedExperiment
#' @title  A method to access the simulation hyperparameters
#' @description The hyperparameters like number of models, range from which
#' parameters are to be sampled, simulation time etc.
#' @param .object RacipeSE object
#' @param value list. Configuration as a list
#' @examples
#' rSet <- RacipeSE()
#' tmpConfig <- sracipeConfig(rSet)
#' sracipeConfig(rSet) <- tmpConfig
#' rm(rSet, tmpConfig)
#' @return \code{RacipeSE} object
#'

setGeneric("sracipeConfig<-",
           def = function(.object, value)
           {
             standardGeneric("sracipeConfig<-")
           }
)


#' @export
#' @import SummarizedExperiment
#' @importFrom S4Vectors metadata
#' @title A method to access the simulation parameters
#' @description The parameters for each model.
#' @param .object RacipeSE object
#' @examples
#' data("demoCircuit")
#' RacipeSet <- sracipeSimulate(demoCircuit, integrate = FALSE, numModels=20)
#' parameters <- sracipeParams(RacipeSet)
#' sracipeParams(RacipeSet) <- parameters
#' rm(parameters,RacipeSet)
#' @return A  data.frame
#'
setGeneric("sracipeParams",
           def = function(.object)
           {
             standardGeneric("sracipeParams")
           }
)


#' @export
#' @title  A method to set the simulation parameters
#' @description Set the parameters
#' @param .object RacipeSE object
#' @param value DataFrame containing the parameteres
#' @examples
#' data("demoCircuit")
#' rSet <- sRACIPE::sracipeSimulate(circuit = demoCircuit, numModels = 20, 
#' integrate = FALSE)
#' parameters <- sracipeParams(rSet)
#' sracipeParams(rSet) <- parameters
#' rm(parameters, rSet)
#' @return A RacipeSE object

setGeneric("sracipeParams<-",
           def = function(.object, value)
           {
             standardGeneric("sracipeParams<-")
           }
)



#' @export
#' @import SummarizedExperiment
#' @title  A method to get the initial conditions used for simulations
#' @description The initial conditions of each of the models.
#' @param .object RacipeSE object
#' @examples
#' data("demoCircuit")
#' rSet <- sRACIPE::sracipeSimulate(circuit = demoCircuit, numModels = 20, 
#' integrate=FALSE)
#' ics <- sracipeIC(rSet)
#' rm(rSet,ics)
#' @return DataFrame

setGeneric("sracipeIC",
           def = function(.object)
           {
             standardGeneric("sracipeIC")
           }
)


#' @export
#' @import SummarizedExperiment
#' @title  A method to set the initial conditions
#' @description Set the initial conditions
#' @param .object RacipeSE object
#' @param value DataFrame containing the initial conditions
#' @examples
#' data("demoCircuit")
#' rSet <- sRACIPE::sracipeSimulate(circuit = demoCircuit, numModels = 10, 
#' integrate=FALSE)
#' ics <- sracipeIC(rSet)
#' sracipeIC(rSet) <- ics
#' rm(rSet, ics)
#' @return A RacipeSE object

setGeneric("sracipeIC<-",
           def = function(.object, value)
           {
             standardGeneric("sracipeIC<-")
           }
)



#' @export
#' @import SummarizedExperiment
#' @importFrom S4Vectors SimpleList
#' @title  Normalize the simulated gene expression
#' @description Normalize the simulated gene expression including gene
#' expressions for stochastic and knockout simulations
#' @param .object RacipeSE object
#' @return A RacipeSE object
#' @examples
#' data("demoCircuit")
#' rSet <- sRACIPE::sracipeSimulate(circuit = demoCircuit, numModels = 20,
#' integrateStepSize = 0.1, simulationTime = 30)
#' rSet <- sracipeNormalize(rSet)
#' @section Related Functions:
#'
#' \code{\link{sracipeSimulate}},  \code{\link{sracipeKnockDown}},
#' \code{\link{sracipeOverExp}},  \code{\link{sracipePlotData}},
#'
setGeneric("sracipeNormalize",
           def = function(.object)
           {
             standardGeneric("sracipeNormalize")
           }
)


#' @export
#' @import grDevices
#' @title Plot Gene Regulatory Circuit
#' @description  Plot Gene Regulatory Circuit to a file or output device.
#' @param .object RacipeSE object
#' A list returned by \code{\link{sracipeSimulate}} function
#' @param plotToFile (optional) logical. Default \code{FALSE}. Whether to save
#' plots to a file.
#' @examples
#' data("demoCircuit")
#' \dontrun{
#' rSet <- sRACIPE::sracipeSimulate(circuit = demoCircuit, numModels = 20,
#' integrateStepSize = 0.1, simulationTime = 30)
#' sracipePlotCircuit(rSet, plotToFile = FALSE)
#' rm(rSet)
#' }
#' @return circuit plot
#' @section Related Functions:
#'
#' \code{\link{sracipeSimulate}},  \code{\link{sracipeKnockDown}},
#' \code{\link{sracipeOverExp}},  \code{\link{sracipePlotData}}
setGeneric("sracipePlotCircuit",
           def = function(.object, plotToFile = FALSE)
           {
             standardGeneric("sracipePlotCircuit")
           }
)


#' @export
#' @import SummarizedExperiment
#' @importFrom gplots heatmap.2
#' @import ggplot2
#' @import gridExtra
#' @import umap
#' @import grDevices
#' @title Plot sRACIPE data
#' @description Plots heatmap, pca, umap of the data simulated using sRACIPE
#' @param  .object List A list returned by \code{\link{sracipeSimulate}} function
#' @param plotToFile (optional) logical. Default \code{FALSE}. Whether to save
#' plots to a file.
#' @param nClusters (optional) Integer. Default 2. Expected number of clusters
#' in the simulated data. Hierarchical clustering will be used to cluster the
#' data and the the models will be colored in UMAP and PCA plots according to
#' these clustering results. The clusters can be also supplied using
#' \code{assignedClusters}.
#' @param pcaPlot (optional) logical. Default \code{TRUE}. Whether to plot PCA
#' embedding.
#' @param umapPlot (optional) logical. Default \code{TRUE}. Whether to plot
#' UMAP embedding
#' @param networkPlot (optional) logical. Default \code{TRUE}. Whether to plot
#' the network.
#' @param clustMethod (optional) character. Default \code{"ward.D2"}. Clustering
#' method for heatmap. See \code{\link[gplots]{heatmap.2}}
#' @param col (optional) Color palette
#' @param distType (optional) Distance type.  Used only if specified
#' explicitly. Otherwise, 1-cor is used. See \code{\link[stats]{dist}},
#' \code{\link[stats]{hclust}}
#' @param assignedClusters vector integer or character. Default NULL.
#' Cluster assignment of models.
#' @param corMethod (optional) character. Default \code{"spearman"}. Correlation
#' method for distance function.
#' @param ... Other arguments
#' @examples
#' data("demoCircuit")
#' \dontrun{
#' rSet <- sRACIPE::sracipeSimulate(circuit = demoCircuit, numModels = 20,
#' integrateStepSize = 0.1, simulationTime = 30)
#' rSet <- sracipePlotData(rSet)
#' }
#' @return \code{RacipeSE} object
#' @section Related Functions:
#'
#' \code{\link{sracipeSimulate}},  \code{\link{sracipeKnockDown}},
#' \code{\link{sracipeOverExp}},  \code{\link{sracipePlotData}},
setGeneric("sracipePlotData",
           def = function(.object, plotToFile = FALSE, nClusters = 2,
                          pcaPlot = TRUE, umapPlot = TRUE,networkPlot = TRUE,
                          clustMethod = "ward.D2", col = col,
                          distType = "euclidean",
                          assignedClusters = NULL, corMethod = "spearman", ...)
           {
             standardGeneric("sracipePlotData")
           }
)

#' @export
#' @import SummarizedExperiment
#' @import ggplot2
#' @import grDevices
#' @title Parameter bifurcation plots
#' @description Plot the expression of the genes against parameter values
#' to understand the effect of parameters on the gene expressions.
#' @param .object RacipeSE object generated by
#' \code{\link{sracipeSimulate}} function.
#' @param paramName character. The name of the parameter to be plotted.
#' @param plotToFile (optional) logical. Default \code{FALSE}. Whether to save
#' plots to a file.
#' @param data (optional) dataframe. Default rSet geneExpression. The data
#' to be plotted. For example, use rSet$stochasticSimulations$[noise] to plot
#' the stochastic data.
#' @param paramValue (optional) Dataframe. The parameter values if rSet$params
#' is not to be used.
#' @param assignedClusters (optional) Dataframe. The cluster assignment of data.
#' @return none
#' @examples
#' data("demoCircuit")
#' \dontrun{
#' rSet <- sRACIPE::sracipeSimulate(circuit = demoCircuit, numModels = 100,
#' plots=FALSE, plotToFile = FALSE)
#' rSet <- sRACIPE::sracipeNormalize(rSet)
#' sracipePlotParamBifur(rSet, "G_A")
#' }
#'
setGeneric("sracipePlotParamBifur",
           def = function(.object, paramName, data = NULL,
                          paramValue = NULL, assignedClusters = NULL,
                          plotToFile = FALSE)
           {
             standardGeneric("sracipePlotParamBifur")
           }
)


#' @export
#' @import SummarizedExperiment
#' @import ggplot2
#' @import grDevices
#' @title Perform in-silico over expression analysis
#' @description Calculates the fraction of models in different clusters
#' with full parameter
#' range and on a subset of models with high production rate of a specific gene
#'  representing the over expression of the specific gene.
#' @param .object RacipeSE object generated by
#' \code{\link{sracipeSimulate}} function.
#' @param clusterOfInterest (optional) cluster number (integer)
#' to be used for arranging
#' the transcription factors
#' @param overProduction (optional) Percentage to which production rate
#' decreases on knockdown. Uses a default value of 10 percent.
#' @param nClusters (optional) Number of clusters in the data. Uses a default
#' value of 2.
#' @param plotFilename (optional) Name of the output file.
#' @param plotHeatmap logical. Default TRUE. Whether to plot the heatmap or not.
#' @param plotBarPlot logical. Default TRUE. Whether to plot the barplot.
#' @param clusterCut integer or character. The cluster assignments.
#' @param plotToFile logical. Default FALSE.
#' @return List containing fraction of models in different clusters
#'  in the original simulations and after knowcking down different genes.
#' Additionaly, it generates two pdf files in the results folder.
#' First is barplot
#' showing the percentage of different clusters in the original simulations
#' and after knocking down each gene. The second pdf contains the heatmap of
#' clusters after marking the models with cluster assignments.
#' @examples
#' data("demoCircuit")
#' \dontrun{
#' rSet <- sRACIPE::sracipeSimulate(circuit = demoCircuit, numModels = 100,
#' plots=FALSE, plotToFile = FALSE)
#' rSet <- sRACIPE::sracipeNormalize(rSet)
#' }
#' @section Related Functions:
#'
#' \code{\link{sracipeSimulate}},  \code{\link{sracipeKnockDown}},
#' \code{\link{sracipeOverExp}},  \code{\link{sracipePlotData}},
setGeneric("sracipeOverExp",
           def = function(.object, overProduction = 10,
                          nClusters = 2,
                          clusterOfInterest = 2,
                          plotFilename = NULL,
                          plotHeatmap = TRUE,
                          plotBarPlot = TRUE,
                          clusterCut = NULL,
                          plotToFile = FALSE)
           {
             standardGeneric("sracipeOverExp")
           }
)


#' @export
#' @import SummarizedExperiment
#' @importFrom stats cor as.dist hclust cutree as.dendrogram prcomp sd
#' @import ggplot2
#' @import grDevices
#' @title Perform in-silico knockdown analysis
#' @description  Calculate the fraction of models in different clusters
#' with full parameter
#' range and on a subset of models with low production rate of a specific gene
#'  representing the knockdown of the specific gene.
#' @param .object RacipeSE object generated by \code{\link{sracipeSimulate}}
#'  function.
#' @param clusterOfInterest (optional) cluster number (integer)
#' to be used for arranging
#' the transcription factors
#' @param reduceProduction (optional) Percentage to which production rate
#' decreases on knockdown. Uses a default value of 10 percent.
#' @param nClusters (optional) Number of clusters in the data. Uses a default
#' value of 2.
#' @param plotFilename (optional) Name of the output file.
#' @param plotHeatmap logical. Default TRUE. Whether to plot the heatmap or not.
#' @param plotBarPlot logical. Default TRUE. Whether to plot the barplot.
#' @param clusterCut integer or character. The cluster assignments.
#' @param plotToFile logical. Default FALSE.
#' @return List containing fraction of models in different clusters
#'  in the original simulations and after knowcking down different genes.
#' Additionaly, it generates two pdf files in the results folder.
#' First is barplot
#' showing the percentage of different clusters in the original simulations
#' and after knocking down each gene. The second pdf contains the heatmap of
#' clusters after marking the models with cluster assignments.
#' @examples
#' data("demoCircuit")
#' \dontrun{
#' rSet <- sRACIPE::sracipeSimulate(circuit = demoCircuit, numModels = 100,
#' plots=FALSE, plotToFile = FALSE)
#' rSet <- sRACIPE::sracipeNormalize(rSet)
#' rSet <- sRACIPE::sracipeKnockDown(rSet, plotToFile = FALSE,
#' plotBarPlot = TRUE, plotHeatmap = FALSE, reduceProduction = 50)
#' }
#'@section Related Functions:
#'
#' \code{\link{sracipeSimulate}},  \code{\link{sracipeKnockDown}},
#' \code{\link{sracipeOverExp}},  \code{\link{sracipePlotData}}
#'
setGeneric("sracipeKnockDown",
           def = function(.object, reduceProduction = 10,
                          nClusters = 2,
                          clusterOfInterest = 2,
                          plotFilename = NULL,
                          plotHeatmap = TRUE,
                          plotBarPlot = TRUE,
                          clusterCut = NULL,
                          plotToFile = FALSE)
           {
             standardGeneric("sracipeKnockDown")
           }
)

