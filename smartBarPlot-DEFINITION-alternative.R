smartBarPlot <-
  function (X, groups, groupNames, groupOrder = groupNames, groupColors, 
            ylim., pointcol = "blue", pointpch=1,  round = TRUE, squeeze = 1, 
            na.rm = TRUE, ...) 
  {
    the.split = split(X, groups)
    if (na.rm) 
      the.split = lapply(the.split, function(X) X[!is.na(X)])
    if (missing(groupNames)) 
      groupNames = names(the.split)
    if (missing(ylim.)) 
      ylim. = range(unlist(the.split))
    boxplotdata = lapply(the.split, boxplotdataJitter, round = round)
    for (element in 1:length(boxplotdata)) rownames(boxplotdata[[element]]) = NULL
    names(boxplotdata) = groupNames
    maxcircles = max(sapply(boxplotdata, function(group) max(rle(group[, 
                                                                       2])$lengths)))
    for (group in groupNames) {
      boxplotdata[[group]][, "x"] = boxplotdata[[group]][, 
                                                         "x"]/squeeze
      boxplotdata[[group]][, "x"] = boxplotdata[[group]][, 
                                                         "x"] + (which(group == groupNames))
    }
    # boxplotdataMatrix = boxplotdata[[1]]
    # for (group in 2:length(boxplotdata)) 
    #   boxplotdataMatrix = rbind(boxplotdataMatrix,
    #                             boxplotdata[[group]])
    # boxplotdataMatrix = cbind(groups, X)
    if (missing(groupColors)) 
      boxcol = "black"
    else {
      print(groupColors)
      if (length(groupColors) == length(groupNames)) {
        boxcol = groupColors
        pointsPerBox = unlist(sapply(boxplotdata, nrow))
        catn("pointsPerBox = ", pointsPerBox)
        pointcol = rep(groupColors, times = pointsPerBox)
      }
      else if (length(groupColors) == 1) {
        pointcol = groupColors
        boxcol = groupColors
      }
      else warning("groupColors ignored;  length = " %&% length(groupColors) %&% 
                     " with " %&% length(groupNames) %&% "groups")
    }
    #print(pointcol)
    plot(boxplotdataMatrix, axes = F, xlab = "",
         col=pointcol,
         ylab = "", xlim = c(1/2, length(boxplotdata) + 1/2), 
         ylim = ylim.)
    boxplot(the.split, boxwex = 0.5, 
            xlim = c(1 - 0.4, 
                     length(the.split) + 0.4), 
            ylim = ylim., border = boxcol, names = groupNames, 
            cex.lab = 1.5, cex.axis = 1.5, staplewex = 1, add = TRUE 
    )
    print(dim(boxplotdataMatrix))
    points(boxplotdataMatrix, col=pointcol)
    return(invisible(boxplotdata))
  }
