lines.loess <-
function (x, y, lattice = F, ...) 
{
    theCall <- sys.call()
    theCall <- as.list(theCall)
    theCall <- theCall[names(theCall) != ""]
    print(theCall)
    argNames = names(theCall)
    argNamesForLoess = intersect(names(as.list(args(loess))), 
        argNames)
    theArgsForLoess = theCall[argNamesForLoess]
    loess.out = do.call("loess", c(formula = y ~ x, theArgsForLoess))
    ord = order(loess.out$x)
    otherArgNames = setdiff(argNames, argNamesForLoess)
    otherArgs = theCall[otherArgNames]
    argListForLines = c(quote(loess.out$x[ord]), quote(loess.out$fitted[ord]), 
        otherArgs)
    print(argListForLines)
    do.call(ifelse(lattice, "panel.lines", "lines"), argListForLines)
}
