renameVars <- function(data, old, new) {
    if (length(old) != length(new)) {
        stop("Old and new name vectors must be of equal length")
    }
    k <- names(data)

    for (i in 1:length(old)) {
        v <- which(k == old[i])
        if (length(v) == 1)
            k[v] <- new[i]
    }
    names(data) <- k
    return(data)
}

rename <- function(x, old, new) {
    if (length(old) != length(new)) {
        stop("Old and new name vectors must be of equal length")
    }

    for (i in 1:length(old)) {
        v <- which(x == old[i])
        if (length(v) == 1)
            x[v] <- new[i]
    }
    return(x)
}


decap <- function(x, firstupper = TRUE) {
    s <- strsplit(x, " ")[[1]]
    s <- paste(tolower(substring(s, 1, 1)), substring(s, 2), sep = "", collapse = " ")
    s <- paste(toupper(substring(s, 1, 1)), substring(s, 2), sep = "", collapse = "")
    return(s)
}


textReplace <- function(filename, vals, tagstring = "_") {
    if (length(filename) == 1) {
        tml <- readLines(filename)
    } else {
        tml <- filename
    }
    for (i in 1:length(vals)) {
        ln <- grep(paste(tagstring, i, tagstring, sep = ""), tml)
        tml[ln] <- gsub(paste(tagstring, i, tagstring, sep = ""), vals[i], tml[ln],
            fixed = TRUE)
    }
    return(paste(tml, collapse = "\n"))
}


directory <- function() ifelse(grepl("unix", .Platform$OS.type), "/", "\\")


splitFilePath <- function(filename) {
    dirchar <- directory()
    filename <- unlist(strsplit(filename, dirchar, fixed = TRUE))
    pathfile <- c(ifelse(length(filename) > 1, paste(filename[-length(filename)],
        collapse = dirchar), "."), last(filename))
    return(pathfile)
}

pdflatex <- function(texfile, open = TRUE, verbose = FALSE, cleanup = TRUE, options = "-halt-on-error",
    path.to.pdflatex = Sys.which("pdflatex"), ...) {
    filepath <- splitFilePath(texfile)
    texfile <- filepath[2]
    filepath <- filepath[1]
    pdffile <- sub("\\.tex$", "\\.pdf", texfile, ignore.case = TRUE)
    if (grepl(" ", texfile)) {
        texfile <- paste("\"", texfile, "\"", sep = "")
        pdffile <- paste("\"", pdffile, "\"", sep = "")
    }
    if (grepl(" ", filepath)) {
        filepath <- paste("\"", filepath, "\"", sep = "")
    }

    if (grepl("unix", .Platform$OS.type)) {
        if (nchar(path.to.pdflatex) == 0) {
            path.to.pdflatex <- "/usr/texbin/pdflatex"
        }
        nullout <- ">/dev/null"
    } else {
        if (nchar(path.to.pdflatex) == 0) {
            path.to.pdflatex <- "pdflatex"
        }
        texfile <- paste(texfile, pdffile)
        nullout <- ">NUL"
        open <- FALSE
    }
    if (verbose)
        nullout <- ""

    texcommand <- paste(path.to.pdflatex, options, "-output-directory", filepath,
        texfile, nullout)
    system(texcommand)
    system(texcommand)

    pdffile <- sub("^\"", "", sub("\"$", "", pdffile))
    filepath <- sub("^\"", "", sub("\"$", "", filepath))
    returnfile <- paste(filepath, pdffile, sep = directory())

    if (cleanup) {
        files <- dir(path = filepath, pattern = sub("\\.pdf$", "", pdffile))
        files <- sgrep(c("out$", "log$", "aux$"), files, value = TRUE)
        if (length(files) > 0) {
            file.remove(paste(filepath, files, sep = directory()))
        }
    }

    if (!file.exists(returnfile)) {
        stop("PDF file does not exist. Check that there are no errors ", "in the LaTeX file.")
    } else if (open) {
        file.open(returnfile)
    }

    return(returnfile)
}

pspp <- function(stdin, verbose = TRUE, options = "", path.to.pspp = Sys.which("pspp"),
    ...) {
    if (nchar(path.to.pspp) == 0) {
        path.to.pspp <- "/opt/local/bin/pspp"
    }
    nullout <- ifelse(verbose, "", ">/dev/null")
    command <- paste(path.to.pspp, options, "<", stdin, nullout)
    system(command)
    invisible()
}

unix.alike <- function() .Platform$OS.type == "unix"

is.osx <- function() as.vector(Sys.info()["sysname"] == "Darwin")

file.open <- function(x) {
    if (grepl("mac", .Platform$pkgType)) {
        for (i in x) system(paste("open", shQuote(i)))
    }
}


lrbind <- function(l, rownames = FALSE, rownamevar = "name") {
    if (is.list(l) & length(l) > 1) {
        if (rownames)
            for (k in 1:length(l)) l[[k]] <- data.frame(asdf = names(l)[k], l[[k]],
                stringsAsFactors = FALSE)
        l <- do.call("rbind", l)
        if (rownames)
            l <- renameVars(l, "asdf", rownamevar)
    }
    return(l)
}

lor <- function(l) {
    t <- rep(FALSE, length(l[[1]]))
    l <- l[unlist(lapply(l, is.logical))]
    if (length(l) > 0)
        for (i in 1:length(l)) t <- t | l[[i]]
    return(t)
}


and <- function(l) {
    t <- rep(TRUE, length(l[[1]]))
    l <- l[unlist(lapply(l, is.logical))]
    if (length(l) > 0)
        for (i in 1:length(l)) t <- t & l[[i]]
    return(t)
}


last <- function(x, last = 1, margin = 1) {
    if (is.matrix(x) | is.data.frame(x) | class(x) == "df.styled") {
        if (margin == 1) {
            if (last > nrow(x))
                stop("'last' exceeds nrow(x)")
            return(x[(1 + nrow(x) - last):nrow(x), ])
        } else {
            if (last > ncol(x))
                stop("'last' exceeds ncol(x)")
            return(x[, (1 + ncol(x) - last):ncol(x), ])
        }
    } else {
        len <- try(length(x))
        if (is.null(len))
            stop("Invalid input x")
        if (last > len)
            stop("'last' exceeds length(x)")
        return(x[(1 + len - last):len])
    }
}


capFirst <- function(s) paste(toupper(substring(s, 1, 1)), substring(s, 2), sep = "")


ltranspose <- function(l) {
    if (length(unique(unlist(lapply(l, length)))) > 1)
        stop("All nested lists must be of equal length")
    if (!is.null(names(l[[1]]))) {
        return(sapply(names(l[[1]]), function(x) lapply(l, function(y) y[[x]]), simplify = FALSE))
    } else {
        return(sapply(1:length(l[[1]]), function(x) lapply(l, function(y) y[[x]]),
            simplify = FALSE))
    }
}


unlistwithnames <- function(x) {
    lists <- unlist(lapply(x, is.list))
    if (any(lists)) {
        subnames <- unlist(lapply(x[lists], function(y) !is.null(names(y))))
        x <- unlist(x)
        if (any(subnames)) {
            dropstrings <- names(which(lists))[subnames]
            if (length(dropstrings) > 0)
                for (s in dropstrings) names(x) <- sub(paste("^", s, "\\.", sep = ""),
                  "", names(x))
        }
    }
    return(x)
}



spsstime <- function(z) as.Date(as.POSIXct(z, origin = "1582-10-14", tz = "GMT"))



errorCheck <- function(logfile) {
    logfile <- readLines(logfile)
    if (grepl("^Execution halted", last(logfile)))
        stop("Error in batch execution")
}


increaseSpace <- function(x, gap, min, max) {
    ord <- order(x)
    if (gap > (max - min))
        stop("Gap too large for [min, max] range provided")
    diffs <- sapply(1:(length(x) - 1), function(i) x[ord][i + 1] - x[ord][i])
    while (any(diffs < gap)) {
        for (i in which(diffs < gap)) {
            a <- x[ord][i]
            b <- x[ord][i + 1]
            d <- 1.25 * gap - diffs[i]
            if (a == min) {
                b <- b + d
            } else if (b == max) {
                a <- a - d
            } else {
                a <- a - d/2
                b <- b + d/2
            }
            if (a < min)
                a <- min
            if (b > max)
                b <- max
            x[ord][i] <- a
            x[ord][i + 1] <- b
        }
        diffs <- sapply(1:(length(x) - 1), function(i) x[ord][i + 1] - x[ord][i])
    }
    return(x)
}


tapply.ttest <- function(X, INDEX, alpha = 0.05) {
    ## X and INDEX work like tapply. If alpha is not null, the function will evaluate
    ## whether the p values are less than alpha. If alpha=NULL, returns the p values.

    out <- lapply(sapply(sort(unique(INDEX)), function(x) t.test(X[INDEX == x], X[INDEX !=
        x]), simplify = FALSE), function(y) list(mean = y$estimate[[1]], p = y$p.value))
    out <- lapply(sapply(names(out[[1]]), function(x) lapply(out, function(y) y[[x]]),
        simplify = FALSE), unlist)
    for (i in 1:length(out)) names(out[[i]]) <- sort(unique(INDEX))
    if (!is.null(alpha))
        out$p <- out$p < alpha
    return(out)
}


sgrep <- function(strs, ..., simplify = TRUE) {
    out <- sapply(strs, function(x) grep(x, ...), simplify = FALSE)
    if (simplify)
        out <- unique(unlist(out))
    return(out)
}

idlookup <- function(x, table, key = NULL, ...) {
    if (!is.null(key)) {
        matchingcol <- key
    } else {
        mc <- unlist(lapply(table, .valInCol, x = x))
        ## minimal error handling but hey, you should pass lookup tables that have things
        if (sum(mc) > 1)
            stop("Value in more than one column")
        if (sum(mc) == 0) {
            warning("Value not in lookuptable")
        }
        matchingcol <- names(mc)[mc]
    }
    othercol <- which(!names(table) == matchingcol)

    return(table[, othercol][match(x, table[, matchingcol])])
}
.valInCol <- function(col, x) {
    any(x %in% col)
}

slots <- function(x) {
    x <- sapply(slotNames(x), function(i) try(slot(x, i), silent = TRUE), simplify = FALSE)
    return(x[vapply(x, Negate(is.error), logical(1))])
}


refresh <- function(x) {
    stripped <- slots(x)
    if (is.stub(x)) {
        cla <- stubTypologize(stripped)
        return(.newstub(stripped[names(stripped) %in% slotNames(cla)]))
    } else {
        cla <- class(x)
        return(do.call("new", c(stripped[names(stripped) %in% slotNames(cla)], Class = cla)))
    }
}


padNA <- function(x, before = 0, after = 0) {
    if (is.factor(x)) {
        if (any(is.na(x))) {
            y <- addNA(factor(x))
            lev <- levels(x)
            if (length(lev) == 0)
                lev <- NA
        } else {
            y <- factor(x)
            lev <- levels(y)
        }
        out <- factor(c(rep(NA, before), y, rep(NA, after)))
        out <- factor(out, levels = 1:length(lev), labels = lev, exclude = NA)
    } else {
        out <- c(rep(NA, before), x, rep(NA, after))
    }
    return(out)
}

is.error <- function(x) is(x, "try-error")


is.valid <- function(x) !is.na(x)



splat <- function(object, rename = NULL, assignnames = rename, where = parent.frame()) {
    if (!is.null(rename)) {
        matchnames <- intersect(names(object), names(rename))
        names(object)[match(matchnames, names(object))] <- unlist(rename[matchnames])
    }
    for (i in 1:length(object)) assign(names(object)[i], object[[i]], envir = where)
    invisible()
}



loadobj <- function(file, obj = "df") {
    env <- new.env()
    with(env, load(file))
    e <- try(get(obj, envir = env))
    if (is.error(e))
        stop(e)
    return(e)
}



getLETTERS <- function(len) {
    if (len > 26) {
        lett <- c(LETTERS, apply(expand.grid(LETTERS, LETTERS)[2:1], 1, paste, collapse = "")[1:(len -
            26)])
    } else {
        lett <- LETTERS[1:len]
    }
    return(lett)
}


validLevels <- function(x) {
    stopifnot(is.factor(x))
    return(levels(x)[sort(unique(x))])
}


as.ordinal <- function(x) {
    suffixcodes <- x%%10
    teens <- (x%/%10) == 1
    suffixes <- ifelse(suffixcodes == 1 & !teens, "st", ifelse(suffixcodes == 2 &
        !teens, "nd", ifelse(suffixcodes == 3 & !teens, "rd", "th")))
    return(paste(x, suffixes, sep = ""))
}


cascade <- function(x, y) {
    stopifnot(is.list(x), is.list(y))
    for (i in names(y)) {
        if (is.list(y[[i]])) {
            if (is.null(x[[i]]))
                x[[i]] <- list()
            x[[i]] <- cascade(x[[i]], y[[i]])
        } else {
            x[[i]] <- y[[i]]
        }
    }
    return(x)
}


path.escape <- function(x) {
    gsub("\\\\? ", "\\\\ ", x)
}
