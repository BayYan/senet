#' sentiment analysis
#' @param edgelist edgelist with edges and counts
#' @param target the target keyword string
#' @param positiveLexicon path to the positive sentiment words in txt format, if not specified, the built-in lexicon is used
#' @param negativeLexicon path to the negtive sentiment words in txt format, if not specified, the built-in lexicon is used
#' @importFrom igraph graph.data.frame E V all_shortest_paths
#' @export

senti <- function(edgelist, target, positiveLexicon=system.file("extdata", "SeedsPos.txt", package = "senet"), negativeLexicon=system.file("extdata", "SeedsNeg.txt", package = "senet")) {

    options(warn=-1)
    colnames(edgelist)[3] <- "weight"
    graph <- igraph::graph.data.frame(edgelist)

    targetV <- igraph::V(graph)[name == target]
    if (length(targetV) == 0) {
        stop("Target does not exist in document")
    }

    nActualPositive <- 0
    sumPositive <- 0

    positiveWords <- read.delim(positiveLexicon, stringsAsFactor = FALSE,header=F)$V1
    negativeWords <- read.delim(negativeLexicon, stringsAsFactor = FALSE,header=F)$V1

    PurePosCount=0
    PurePosCountRev=0
    PureNegCount=0
    PureNegCountRev=0
    nonPurePosCount=0
    nonPurePosCountRev=0
    nonPureNegCount=0
    nonPureNegCountRev=0


    for (positiveWord in positiveWords) {
        posV <- igraph::V(graph)[name == positiveWord]

        if (length(posV) == 0) {next }
        nActualPositive <- nActualPositive + 1

        result <- calculateDistance(graph, targetV, posV, negativeWords)
        targetToPosMat_weight <- result[1]
        targetToPosMat_length <- result[2]
        nonPurePosCount <- nonPurePosCount + result[3]

        # reverse
        result <- calculateDistance(graph, posV, targetV, negativeWords)
        posToTargetMat_weight <- result[1]
        posToTargetMat_length <- result[2]
        nonPurePosCountRev <- nonPurePosCountRev + result[3]

        if (targetToPosMat_length != 0) {
            dist_to <- targetToPosMat_weight / (targetToPosMat_length * targetToPosMat_length)
            sumPositive <- sumPositive + dist_to
            PurePosCount <- PurePosCount + 1
        }

        if (posToTargetMat_length != 0) {
            dist_from <- posToTargetMat_weight / (posToTargetMat_length * posToTargetMat_length)
            sumPositive <- sumPositive + dist_from
            PurePosCountRev <- PurePosCountRev + 1
        }
    }

    nActualNegative <- 0
    sumNegative <- 0

    for (negativeWord in negativeWords) {
        negV <- igraph::V(graph)[name == negativeWord]
        if (length(negV) == 0) { next }
        nActualNegative <- nActualNegative + 1

        result <- calculateDistance(graph, targetV, negV, positiveWords)
        targetToNegMat_weight <- result[1]
        targetToNegMat_length <- result[2]
        nonPureNegCount <- nonPureNegCount + result[3]

        # reverse
        result <- calculateDistance(graph, negV, targetV, positiveWords)
        negToTargetMat_weight <- result[1]
        negToTargetMat_length <- result[2]
        nonPureNegCountRev <- nonPureNegCountRev + result[3]

        if (targetToNegMat_length != 0) {
            dist_to <- targetToNegMat_weight / (targetToNegMat_length * targetToNegMat_length)
            sumNegative <- sumNegative + dist_to
            PureNegCount <- PureNegCount + 1
        }

        if (negToTargetMat_length != 0) {
            dist_from <- negToTargetMat_weight / (negToTargetMat_length * negToTargetMat_length)
            sumNegative <- sumNegative + dist_from
            PureNegCountRev <- PureNegCountRev + 1
        }
    }

    numPositive <- length(positiveWords)
    numNegative <- length(negativeWords)

    s1 <- nActualPositive
    s2 <- nActualNegative

    s3 <- 0
    if (numPositive != 0) {
        s3 <- sumPositive / numPositive
    }

    s4 <- 0
    if (numNegative != 0) {
        s4 <- sumNegative / numNegative
    }

    s5 <- 0
    if (numNegative != 0) {
        s5 <- s3 * numPositive / numNegative
    }

    s6 <- 0
    if (s4 != 0) {
        s6 <- s5 / s4
    } else if (s5 == 0) {
        s6 <- 0
    } else {
        s6 <- 1
    }

    s7 <- 0
    if (s1 != 0) {
        s7 <- sumPositive / s1
    }

    s8 <- 0
    if (s2 != 0) {
        s8 <- sumNegative / s2
    }

    s9 <- 0
    if (numNegative != 0) {
        s9 <- s7 * numPositive / numNegative
    }

    s10 <- 0
    if (s8 != 0) {
        s10 <- s9 / s8
    } else if (s9 == 0) {
        s10 <- 0
    } else {
        s10 <- 1
    }

    s11 <- s5 -s4

    out <- as.data.frame(cbind(s1, s2, s3, s4, s5, s6, s7, s8, s9,s10,s11))
    colnames(out) <- c("S1.Number of positive words","S2.Number of negative words","S3.Positivity",
    "S4.Negativity","S5.Normalized positivity","S6.Positivity/negativity ratio","S7.Positivity intensity",
    "S8.Negativity intensity","S9.Normalized positivity intensity","S10.Normalized ratio of positivity intensity to negativity intensity",
    "S11.Sentiment bias")
    return(out)
}

calculateDistance <- function(graph, from, to, exclude) {
    allPaths <- igraph::all_shortest_paths(graph, from, to=to, mode="out")

    if (length(allPaths$res) == 0) {
        return(c(0, 0, FALSE))
    }
    for (path in allPaths$res) {
        pure <- TRUE
        lastIndex <- -1
        d <- 0
        length <- 0
        for (vertexIndex in path) {
            vertex <- V(graph)[vertexIndex]
            if (vertex$name %in% exclude) {
                pure <- FALSE
                break
            }
            if (lastIndex != -1) {
                weight <- graph[lastIndex][vertexIndex][[1]]
                d <- d + weight
                length <- length + 1
            }
            lastIndex <- vertexIndex
        }
        if (pure) {
            return(c(d, length, FALSE))
        }
    }

    return(c(0, 0, TRUE))
}
