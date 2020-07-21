#' sentiment analysis
#' @param edgelist edgelist with edges and counts
#' @param target the target keyword string
#' @param positiveLexicon path to the positive sentiment words in txt format, if not specified, the built-in lexicon is used
#' @param negativeLexicon path to the negtive sentiment words in txt format, if not specified, the built-in lexicon is used
#' @param verbose print messages
#' @importFrom igraph graph.data.frame E V all_shortest_paths
#' @export

senti <- function(
    edgelist,
    target,
    positiveLexicon=system.file("extdata", "SeedsPos.txt", package = "senet"),
    negativeLexicon=system.file("extdata", "SeedsNeg.txt", package = "senet"),
    verbose = FALSE) {

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

    purePosCount = 0
    purePosCountRev = 0
    pureNegCount = 0
    pureNegCountRev = 0
    mixedPosCount = 0
    mixedPosCountRev = 0
    mixedNegCount = 0
    mixedNegCountRev = 0


    for (positiveWord in positiveWords) {
        posV <- igraph::V(graph)[name == positiveWord]

        if (length(posV) == 0) { next }
        nActualPositive <- nActualPositive + 1

        result <- calculateDistance(graph, targetV, posV, negativeWords, verbose)
        targetToPosPathLength <- as.numeric(result[1])
        targetToPosEdgeCount <- as.numeric(result[2])
        targetToPosIsMixed <- as.logical(result[3])
        targetToPosPathString <- result[4]

        if (targetToPosIsMixed) {
            mixedPosCount <- mixedPosCount + 1
        }

        # reverse
        result <- calculateDistance(graph, posV, targetV, negativeWords, verbose)
        posToTargetPathLength <- as.numeric(result[1])
        posToTargetEdgeCount <- as.numeric(result[2])
        posToTargetIsMixed <- as.logical(result[3])
        posToTargetPathString <- result[4]

        if (posToTargetIsMixed) {
            mixedPosCountRev <- mixedPosCountRev + 1
        }

        if (targetToPosEdgeCount != 0) {
            strengthTo <- targetToPosPathLength / (targetToPosEdgeCount * targetToPosEdgeCount)
            sumPositive <- sumPositive + strengthTo
            purePosCount <- purePosCount + 1

            if (verbose) {
                label <- if (targetToPosIsMixed) "Mixed_Positive_From_Target_to_Seed" else "Pure_Positive_From_Target_to_Seed"
                message(paste(
                    label,
                    paste("seed word:", positiveWord),
                    paste("target word:", target),
                    paste("path length:", targetToPosPathLength),
                    paste("edge count:", targetToPosEdgeCount),
                    paste("strength ", strengthTo),
                    paste("path string: [", targetToPosPathString, "]"),
                    sep = ", "),
                    appendLF = TRUE)
            }
        }

        if (posToTargetEdgeCount != 0) {
            strengthFrom <- posToTargetPathLength / (posToTargetEdgeCount * posToTargetEdgeCount)
            sumPositive <- sumPositive + strengthFrom
            purePosCountRev <- purePosCountRev + 1

            if (verbose) {
                label <- if (targetToPosIsMixed) "Mixed_Positive_From_Seed_to_Target" else "Pure_Positive_From_Seed_to_Target"
                message(paste(
                    label,
                    paste("seed word:", positiveWord),
                    paste("target word:", target),
                    paste("path length:", posToTargetPathLength),
                    paste("edge count:", posToTargetEdgeCount),
                    paste("strength", strengthFrom),
                    paste("path string: [", posToTargetPathString, "]"),
                    sep = ", "),
                    appendLF = TRUE)
            }
        }
    }

    nActualNegative <- 0
    sumNegative <- 0

    for (negativeWord in negativeWords) {
        negV <- igraph::V(graph)[name == negativeWord]
        if (length(negV) == 0) { next }
        nActualNegative <- nActualNegative + 1

        result <- calculateDistance(graph, targetV, negV, positiveWords, verbose)
        targetToNegPathLength <- as.numeric(result[1])
        targetToNegEdgeCount <- as.numeric(result[2])
        targetToNegIsMixed <- as.logical(result[3])
        targetToNegPathString <- result[4]

        if (targetToNegIsMixed) {
            mixedNegCount <- mixedNegCount + 1
        }

        # reverse
        result <- calculateDistance(graph, negV, targetV, positiveWords, verbose)
        negToTargetPathLength <- as.numeric(result[1])
        negToTargetEdgeCount <- as.numeric(result[2])
        negToTargetIsMixed <- as.logical(result[3])
        negToTargetPathString <- result[4]

        if (negToTargetIsMixed) {
            mixedNegCountRev <- mixedNegCountRev + 1
        }

        if (targetToNegEdgeCount != 0) {
            strengthTo <- targetToNegPathLength / (targetToNegEdgeCount * targetToNegEdgeCount)
            sumNegative <- sumNegative + strengthTo
            pureNegCount <- pureNegCount + 1

            if (verbose) {
                label <- if (targetToPosIsMixed) "Mixed_Negative_From Target_to_Seed" else "Pure_Negative_From_Target_to_Seed"
                message(paste(
                    label,
                    paste("seed word:", negativeWords),
                    paste("target word:", target),
                    paste("path length:", targetToNegPathLength),
                    paste("edge count:", targetToNegEdgeCount),
                    paste("strength", strengthTo),
                    paste("path string: [", targetToNegPathString, "]"),
                    sep = ", "),
                    appendLF = TRUE)
            }
        }

        if (negToTargetEdgeCount != 0) {
            strengthFrom <- negToTargetPathLength / (negToTargetEdgeCount * negToTargetEdgeCount)
            sumNegative <- sumNegative + strengthFrom
            pureNegCountRev <- pureNegCountRev + 1

            if (verbose) {
                label <- if (targetToPosIsMixed) "Mixed_Negative_From Seed_to_Target" else "Pure_Negative_From_Seed_to_Target"
                message(paste(
                    label,
                    paste("seed word:", negativeWord),
                    paste("target word:", target),
                    paste("path length:", negToTargetPathLength),
                    paste("edge count:", negToTargetEdgeCount),
                    paste("strength", strengthFrom),
                    paste("path string: [", negToTargetPathString, "]"),
                    sep = ", "),
                    appendLF = TRUE)
            }
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

    result <- data.frame(
        "senti_name" = c("S1.Number of positive words",
                         "S2.Number of negative words",
                         "S3.Positivity",
                         "S4.Negativity",
                         "S5.Normalized positivity",
                         "S6.Positivity/negativity ratio",
                         "S7.Positivity intensity",
                         "S8.Negativity intensity",
                         "S9.Normalized positivity intensity",
                         "S10.Normalized ratio of positivity intensity to negativity intensity",
                         "S11.Sentiment bias"),
        "senti_value" = c(s1, s2, s3, s4, s5, s6, s7, s8, s9,s10,s11)
    )

    return(result)
}

calculateDistance <- function(graph, from, to, exclude, verbose) {
    pathString <- ""
    allPaths <- igraph::all_shortest_paths(graph, from, to=to, mode="out")

    if (length(allPaths$res) == 0) {
        return(c(0, 0, FALSE, pathString))
    }
    for (path in allPaths$res) {
        pure <- TRUE
        lastIndex <- -1
        lastWord <- ""
        d <- 0
        count <- 0
        pathString <- ""
        for (vertexIndex in path) {
            vertex <- V(graph)[vertexIndex]
            if (vertex$name %in% exclude) {
                pure <- FALSE
            }
            if (lastIndex != -1) {
                weight <- graph[lastIndex][vertexIndex][[1]]
                d <- d + weight
                count <- count + 1
                currentPathString = paste(lastWord, vertex$name, weight)
                if (pathString == "") {
                    pathString <- paste(currentPathString, sep=", ")
                } else {
                    pathString <- paste(pathString, currentPathString, sep=", ")
                }
            }
            lastIndex <- vertexIndex
            lastWord <- vertex$name
        }
        if (pure) {
            return(c(d, count, FALSE, pathString))
        }
    }

    return(c(0, 0, TRUE, pathString))
}