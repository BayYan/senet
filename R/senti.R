#' sentiment analysis
#' @param edgelist edgelist with edges and counts
#' @param target the target keyword string
#' @param positiveLexicon path to the positive sentiment words in txt format, if not specified, the built-in lexicon is used
#' @param negativeLexicon path to the negtive sentiment words in txt format, if not specified, the built-in lexicon is used
#' @importFrom igraph graph.data.frame E V distances
#' @export

senti <- function(edgelist, target, positiveLexicon=system.file("extdata", "SeedsPos.txt", package = "senet"), negativeLexicon=system.file("extdata", "SeedsNeg.txt", package = "senet")) {

    graph <- igraph::graph.data.frame(edgelist)
    igraph::E(graph)$weight <- igraph::E(graph)$count
    
    targetV <- igraph::V(graph)[name == target]
     if (length(targetV) == 0){
         stop("Target does not exist in document")}

    positiveSenti <- list()
    positiveDistances <- list()

    positiveWords <- read.delim(positiveLexicon, stringsAsFactor = FALSE,header=F)$V1

    for (positiveWord in positiveWords) {
        posV <- igraph::V(graph)[name == positiveWord]
        if (length(posV) == 0){next}
        targetToPosMat <- igraph::distances(graph, v=targetV, to=posV, mode="out", algorithm = "dijkstra")
        posToTargetMat <- igraph::distances(graph, v=posV, to=targetV, mode="out", algorithm = "dijkstra")
       
        if (targetToPosMat[1] != Inf && posToTargetMat[1] != Inf) {
            dist <- targetToPosMat[1] + posToTargetMat[1]
            positiveSenti <- append(positiveSenti, positiveWord)
            positiveDistances <- append(positiveDistances, dist)
        }
    }

    negativeSenti <- list()
    negativeDistances <- list()

    negativeWords <- read.delim(negativeLexicon, stringsAsFactor = FALSE,header=F)$V1

    for (negativeWord in negativeWords) {
        negV <- igraph::V(graph)[name == negativeWord]
        if (length(negV) == 0){next}

        targetToNegMat <- igraph::distances(graph, v=targetV, to=negV, mode="out", algorithm = "dijkstra")
        negToTargetMat <- igraph::distances(graph, v=negV, to=targetV, mode="out", algorithm = "dijkstra")

        if (targetToNegMat[1] != Inf && negToTargetMat[1] != Inf) {
            dist <- targetToNegMat[1] + negToTargetMat[1]
            negativeSenti <- append(negativeSenti, negativeWord)
            negativeDistances <- append(negativeDistances, dist)
        }
    }

    # print("positiveDistances")
    # print(positiveDistances)

    # print("negativeDistances")
    # print(negativeDistances)

    actualNumPositive = length(positiveDistances)
    actualNumNegative = length(negativeDistances)

    numPositive = length(positiveWords)
    numNegative = length(negativeWords)

    sumPositive = Reduce('+', positiveDistances)
    sumNegative = Reduce('+', negativeDistances)

    s1 <- 0
    if (numPositive != 0) {
        s1 <- sumPositive / numPositive
    }

    s2 <- 0
    if (numNegative != 0) {
        s2 <- sumNegative / numNegative
    }

    s3 <- 0
    if (numNegative != 0) {
        s3 <- s1 * numPositive / numNegative
    }

    s4 <- 0
    if (s2 != 0) {
        s4 <- s3 / s2
    }

    s5 <- 0
    if (actualNumPositive != 0) {
        s5 <- sumPositive / actualNumPositive
    }

    s6 <- 0
    if (actualNumNegative != 0) {
        s6 <- sumNegative / actualNumNegative
    }

    s7 <- 0
    if (numNegative != 0) {
        s7 <- s5 * numPositive / numNegative
    }

    s8 <- 0
    if (s6 != 0) {
        s8 <- s7 / s6
    }

    s9 <- s3 -s2

    out <- as.data.frame(cbind(s1, s2, s3, s4, s5, s6, s7, s8, s9))
    return(out)
}
