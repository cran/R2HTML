#----------------------------------------------------------------------------------------------------#
#
#     R2HTML - Library of exportation to HTML for R
#
#     Copyright (C) 2002  Eric Lecoutre 
# 
#     This program is free software; you can redistribute it and/or modify
#     it under the terms of the GNU General Public License as published by
#     the Free Software Foundation; either version 2 of the License, or
#     (at your option) any later version.
# 
#     This program is distributed in the hope that it will be useful,
#     but WITHOUT ANY WARRANTY; without even the implied warranty of
#     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#     GNU General Public License for more details.
# 
#     You should have received a copy of the GNU General Public License
#     along with this program; if not, write to the Free Software
#     Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307 USA
#
#----------------------------------------------------------------------------------------------------#
#     Contact:
#
#     Eric Lecoutre
#     lecoutre@stat.ucl.ac.be
#
#     Institut de statistique
#     Voie du Roman Pays, 20
#     1348 Louvain-la-Neuve
#     BELGIQUE
#
#----------------------------------------------------------------------------------------------------#

###
###  HTML print FUNCTIONS
###

#----------------------------------------------------------------------------------------------------#

"HTML"<- function(x, ...) { UseMethod("HTML") }

#----------------------------------------------------------------------------------------------------#

"HTML.default"<-
function(x, File = .HTML.File, ...)
{
	a <- attributes(x)
	if(cl <- length(class(x)) > 0)
		x <- unclass(x)	# avoid methods (class is in a, so is printed)

	if (mode(x)=="function") HTML.fun(x,File=File)
	else
{

	if(length(a) - (is.recursive(x) && length(names(x))) > 0)
		HTML.structure(x, a, File = File, ...)
	else switch(mode(x),
			numeric = ,
			logical = ,
			complex = ,
			character = HTML.atomic(x, File = File, ...), 
			list = HTML.list(x, File = File, ...),
			graphics = HTML.graphics(x, File = File, ...),
			dput(x))
	}
	if(cl)
		class(x) <- a$class
	invisible(x)
}

#----------------------------------------------------------------------------------------------------#

"HTML.formula"<-function(x,File=.HTML.File,...) { HTML(deparse(unclass(x)),File=File)}

#----------------------------------------------------------------------------------------------------#

"HTML.array"<- function(x, File = .HTML.File, ...)
{

	# note: odometer is an utilitary function to knwow if a value belongs in a circle.
	odometer <- function(current, radix)
	{
		if(any(c(current, radix) < 0))
			stop("arguments must be non-negative")
		lc <- length(current)
		if(length(radix) != lc)
			radix <- rep(radix, length = lc)
		radix <- radix - 1
		for(i in 1:lc) {
			if((ii <- current[i]) < radix[i]) {
				current[i] <- ii + 1
				return(current)
			}
			else current[i] <- 0
		}
		current
	}


	d <- dim(x)
	ndim <- length(d)
	dn <- dimnames(x)
	if(ndim == 1)
		HTML.matrix(matrix(x, 1, dimnames = list("", if(is.null(
			dn)) paste("[", 1:d[1], "]", sep = "") else dn[[1]])), 
			File = File, ...)
	else if(ndim == 2)
		HTML.matrix(x, Border = 0, File = File, ...)
	else {
		if(length(dn) < ndim)
			dn <- vector("list", ndim)
		for(i in 3:ndim)
			if(length(dn[[i]]) < d[i]) dn[[i]] <- paste(1:d[i])
		xm <- array(x[1], d[1:2])
		dimnames(xm) <- dn[1:2]
		d <- d[ - (1:2)]
		nm <- length(xm)
		which <- 1:nm
		dn <- dn[ - (1:2)]
		ndim <- ndim - 2
		counter <- rep(0, length(d))
		for(i in 1:(length(x)/nm)) {
			cat("<BR>, , ", file = File, append = TRUE)
			for(j in 1:ndim)
				cat(dn[[j]][counter[j] + 1], if(j < ndim) ", "
				   else "<BR>", sep = "", file = File, append
				   = TRUE)
			xm[1:nm] <- x[which]
			HTML.matrix(xm, Border = 0, File = File, ...)
			counter <- odometer(counter, d)
			which <- which + nm
		}
	}
	invisible(x)
}

#----------------------------------------------------------------------------------------------------#

"HTML.by"<- function (x, File=.HTML.File,vsep="<HR SIZE=1 WIDTH=100%>",...) 
{
    d <- dim(x)
    dn <- dimnames(x)
    dnn <- names(dn)
    if (missing(vsep)) 
        vsep <- "\n<HR SIZE=1 WIDTH=100%>\n"
    lapply(seq(along = x), function(i, x, labs, vsep, ...) {
        if (i != 1 && !is.null(vsep)) 
            HTML(vsep, File=File)
        ii <- i - 1
        for (j in seq(along = dn)) {
            iii <- ii%%d[j] + 1
            ii <- ii%/%d[j]
            HTML(paste(dnn[j], ": ", dn[[j]][iii], "\n<BR>", sep = ""),File=File)
        }
        HTML(x[[i]], File=File)
    }, x, labs, vsep, ...)
    invisible(x)
}

#----------------------------------------------------------------------------------------------------#

"HTML.family" <- function (x, File=.HTML.File,...) 
{
    HTML(paste("\n<BR><B>Family</B>:", x$family, "\n<BR>",sep=""),File=.HTML.File)
    HTML(paste("\n<B>Link function</B>:", x$link, "\n<BR>\n<BR>",sep=""),File=.HTML.File)
}

#----------------------------------------------------------------------------------------------------#

"HTML.terms" <- function (x, File=.HTML.File,...) HTML.default(unclass(x),File=File)

#----------------------------------------------------------------------------------------------------#

"HTML.factor" <- function (x, File=.HTML.File,...) 
{
    if (length(x) <= 0) 
        HTML("factor(0)\n<BR>",File=File)
    else HTML(as.character(x), File=File, ...)
    HTMLbr(File=File)
    HTML(paste("Levels: ", paste(levels(x), collapse = " "), "\n<BR>",sep=""),File=File)
    invisible(x)
}

#----------------------------------------------------------------------------------------------------#
"HTML.density" <- function (x, digits=4,File=.HTML.File, ...) 
{

    HTML(paste("\n<BR><B>Call</B>:\n      ", deparse(x$call), "<BR><BR>\n\n<B>Data</B>: ", x$data.name, 
        " (", x$n, " obs.);", " <B>Bandwidth</B> 'bw' = ", round(x$bw, digits), "\n<BR>\n<BR>", sep = ""),append=TRUE,file=File)
    HTML(summary(as.data.frame(x[c("x", "y")])), ...)
    invisible(x)
}


#----------------------------------------------------------------------------------------------------#
"HTML.infl" <- function (x, digits = max(3, getOption("digits") - 4), File=.HTML.File,...) 
{
    HTML(paste("Influence measures of\n<BR>        ", deparse(x$call), ":\n<BR>\n<BR>",sep=""),File=File)
    is.star <- apply(x$is.inf, 1, any, na.rm = TRUE)
    HTML(data.frame(round(x$infmat,digits), inf = ifelse(is.star, "*", " ")),File=File, ...)
    invisible(x)
}


#----------------------------------------------------------------------------------------------------#

"HTML.lm"<-function(x,digits= max(3, getOption("digits") - 3),File=.HTML.File,...)
{
	HTMLli(paste("Call: ",deparse(x$call),sep=""),File=File)
	HTMLli("Coefficients<BR>",File=File)
	HTML(round(x$coeff,3),File=File)

}

#----------------------------------------------------------------------------------------------------#
"HTML.lm.null" <- function (x, digits = max(3, getOption("digits") - 3), File=.HTML.File,...) 
{
    HTMLli(paste("Call: ", deparse(x$call), "\n<BR>", sep = ""),File=File)
    HTMLli("No coefficients<BR>\n")
    invisible(x)
}
#----------------------------------------------------------------------------------------------------#


"HTML.ftable" <- function (x, digits = getOption("digits"), File=.HTML.File,...) 
{
 if (!inherits(x, "ftable")) 
        stop("x must be an `ftable'")

	# This is write.ftable function excepted the end 
	# This function arranges and prepares the flat table

    ox <- x
    makeLabels <- function(lst) {
        lens <- sapply(lst, length)
        cplensU <- c(1, cumprod(lens))
        cplensD <- rev(c(1, cumprod(rev(lens))))
        y <- NULL
        for (i in rev(seq(along = lst))) {
            ind <- 1 + seq(from = 0, to = lens[i] - 1) * cplensD[i + 
                1]
            tmp <- character(length = cplensD[i])
            tmp[ind] <- lst[[i]]
            y <- cbind(rep(tmp, times = cplensU[i]), y)
        }
        y
    }
    makeNames <- function(x) {
        nmx <- names(x)
        if (is.null(nmx)) nmx <- rep("", length = length(x))
        nmx
    }
    xrv <- attr(x, "row.vars")
    xcv <- attr(x, "col.vars")
    LABS <- cbind(rbind(matrix("", nr = length(xcv), nc = length(xrv)), makeNames(xrv), makeLabels(xrv)), c(makeNames(xcv),rep("", times = nrow(x) + 1)))
    DATA <- rbind(t(makeLabels(xcv)), rep("", times = ncol(x)), format(unclass(x), digits = digits))
    x <- cbind(apply(LABS, 2, format, justify = "left"), apply(DATA, 2, format, justify = "right"))
    HTML(x,File=File)
    invisible(ox)
}

#----------------------------------------------------------------------------------------------------#

"HTML.POSIXlt" <- function (x, File=.HTML.File,...) HTML(paste("<P>",format(x, usetz = TRUE),"</P>",sep=""), File=File)


"HTML.POSIXct" <- function (x, File=.HTML.File,...) HTML(paste("<P>",format(x, usetz = TRUE),"</P>",sep=""), File=File)

    
#----------------------------------------------------------------------------------------------------#

"HTML.octmode" <- function (x, File=.HTML.File,...)  HTML(paste("<P>",format(x),"</P>",sep=""), File=File)

#----------------------------------------------------------------------------------------------------#

"HTML.rle" <- function (x, digits = getOption("digits"), File=.HTML.File,...) 
{
    HTML("<B><CENTER>Run Length Encoding</CENTER></B>\n<BR>\n",File=File)
	tab<-rbind(x$length,x$values)
	tab<-cbind(c("Length","Values"),tab)
    HTML(tab,File=File)
}

#----------------------------------------------------------------------------------------------------#

"HTML.logLik" <- function (x, digits = getOption("digits"),File=.HTML.File, ...)    HTML(paste("<P>`log Lik.' ", format(c(x), digits = digits), " (df=",  format(attr(x, "df")), ")\n</P>", sep = ""),File=File)

#----------------------------------------------------------------------------------------------------#

 "HTML.xtabs" <- function (x,File=.HTML.File, ...) 
{
    ox <- x
    attr(x, "call") <- NULL
    HTML.table(x,File=File, ...)
    invisible(ox)
}

#----------------------------------------------------------------------------------------------------#

"HTML.summary.lm"<-function (x, digits = max(3, getOption("digits") - 3), symbolic.cor = p >   4, signif.stars = getOption("show.signif.stars"), File=.HTML.File,...) 
{

	HTML("<HR SIZE=1><FONT SIZE=+3>Regression report</FONT><BR>\n",File=File)
	HTMLli(paste("Call: ",deparse(x$call),"\n", sep = "", collapse = ""),File=File) 

	resid <- x$residuals
	df <- x$df
	rdf <- df[2]

	HTMLli(paste(if (!is.null(x$w) && diff(range(x$w))) "Weighted "," Residuals<BR>\n"),File=File)
	if (rdf > 5) {
	    nam <- c("Min", "1Q", "Median", "3Q", "Max")
	    rq <- if (length(dim(resid)) == 2) 
		structure(apply(t(resid), 1, quantile), dimnames = list(nam,   dimnames(resid)[[2]]))
	    else structure(quantile(resid), names = nam)
	    HTML(rq,  File=File)
	}
	else if (rdf > 0) {
	    HTML(resid,File=File, ...)
	}
	else {
	    HTML(paste("ALL", df[1], "residuals are 0: no residual degrees of freedom!<BR>\n",sep=""),File=File)
	}
	if (nsingular <- df[3] - df[1]) 

		HTMLli(paste("Coefficients (",nsingular, "not defined because of singularities)<BR>\n",sep=""),File=File)
	else HTMLli("Coefficients\n")		


	HTML.coefmat(x$coef, digits = digits, signif.stars = signif.stars, File=File,   ...)
	
	HTMLli(paste("Residuals standard error: ",round(x$sigma,digits)," on ",rdf," degrees of freedom\n",sep=""),File=File)
 
	

	if (!is.null(x$fstatistic)) {
		HTMLli(paste("Multiple R-Squared:<B>",round(x$r.squared,digits),"</B>",sep=""),File=File)
		HTMLli(paste("Adjusted R-Squared:<B>",round(x$adj.r.squared,digits),"</B>",sep=""),File=File)
	    	HTMLli(paste("FALSE-statistics: <B>", round(x$fstatistic[1],digits), "</B> on ",x$fstatistic[2], " and ", x$fstatistic[3], " DF. P-value:<B>",round(1-pf(x$fstatistic[1],x$fstatistic[2],x$fstatistic[3]),digits),"</B>." ,sep=""),File=File)
	 	}
	correl <- x$correlation
	if (!is.null(correl)) {
	    p <- NCOL(correl)
	    if (p > 1) {
		HTMLli("Correlation of Coefficients:\n",File=File)
		if (symbolic.cor) 
		    HTML(symnum(correl)[-1, -p],File=File)
		else {
		    correl[!lower.tri(correl)] <- NA
		    HTML(correl[-1, -p, drop = FALSE],File=File)
		}
	    }
	}
	HTML("<HR SIZE=1>",File=File)
	invisible(NULL)
}


#----------------------------------------------------------------------------------------------------#
"HTML.coefmat"<- function (x, digits = max(3, getOption("digits") - 2), signif.stars = getOption("show.signif.stars"), 
    dig.tst = max(1, min(5, digits - 1)), cs.ind = 1:k, tst.ind = k + 
        1, zap.ind = integer(0), P.values = NULL, has.Pvalue = nc >= 
        4 && substr(colnames(x)[nc], 1, 3) == "Pr(", na.print = "",File=.HTML.File,    ...) 
{
    if (is.null(d <- dim(x)) || length(d) != 2) 
        stop("1st arg. 'x' must be coefficient matrix/d.f./...")
    nc <- d[2]
    if (is.null(P.values)) {
        scp <- getOption("show.coef.Pvalues")
        if (!is.logical(scp) || is.na(scp)) {
            warning("option `show.coef.Pvalues' is invalid: assuming TRUE")
            scp <- TRUE
        }
        P.values <- has.Pvalue && scp
    }
    else if (P.values && !has.Pvalue) 
        stop("'P.values is TRUE, but has.Pvalue not!")
    if (has.Pvalue && !P.values) {
        d <- dim(xm <- data.matrix(x[, -nc, drop = FALSE]))
        nc <- nc - 1
        has.Pvalue <- FALSE
    }
    else xm <- data.matrix(x)
    k <- nc - has.Pvalue - (if (missing(tst.ind)) 
        1
    else length(tst.ind))
    if (!missing(cs.ind) && length(cs.ind) > k) 
        stop("wrong k / cs.ind")
    Cf <- array("", dim = d, dimnames = dimnames(xm))
    ok <- !(ina <- is.na(xm))
    if (length(cs.ind) > 0) {
        acs <- abs(coef.se <- xm[, cs.ind, drop = FALSE])
        digmin <- 1 + floor(log10(range(acs[acs != 0], na.rm = TRUE)))
        Cf[, cs.ind] <- format(round(coef.se, max(1, digits - 
            digmin)), digits = digits)
    }
    if (length(tst.ind) > 0) 
        Cf[, tst.ind] <- format(round(xm[, tst.ind], dig = dig.tst), 
            digits = digits)
    if (length(zap.ind) > 0) 
        Cf[, zap.ind] <- format(zapsmall(xm[, zap.ind], dig = digits), 
            digits = digits)
    if (any(r.ind <- !((1:nc) %in% c(cs.ind, tst.ind, zap.ind, 
        if (has.Pvalue) nc)))) 
        Cf[, r.ind] <- format(xm[, r.ind], digits = digits)
    okP <- if (has.Pvalue) 
        ok[, -nc]
    else ok
    x0 <- xm[okP] == 0 != (as.numeric(Cf[okP]) == 0)
    if (length(not.both.0 <- which(x0 & !is.na(x0)))) {
        Cf[okP][not.both.0] <- format(xm[okP][not.both.0], digits = max(1, 
            digits - 1))
    }
    if (any(ina)) 
        Cf[ina] <- na.print
    if (P.values) {
        if (!is.logical(signif.stars) || is.na(signif.stars)) {
            warning("option `show.signif.stars' is invalid: assuming TRUE")
            signif.stars <- TRUE
        }
        pv <- xm[, nc]
        if (any(okP <- ok[, nc])) {
            Cf[okP, nc] <- format.pval(pv[okP], digits = dig.tst)
            signif.stars <- signif.stars && any(pv[okP] < 0.1)
            if (signif.stars) {
                Signif <- symnum(pv, corr = FALSE, na = FALSE, 
                  cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1), 
                  symbols = c("***", "**", "*", ".", " "))
                Cf <- cbind(Cf, format.char(Signif))
            }
        }
        else signif.stars <- FALSE
    }
    else signif.stars <- FALSE
    
    HTML.matrix(Cf, File=File,  ...)
    if (signif.stars)     HTML(paste("\n<P>--- Signif. codes: ", attr(Signif, "legend"), "</P>\n",sep=""),File=File)
    invisible(x)
}


#----------------------------------------------------------------------------------------------------#

"HTML.table"<-function(x,File=.HTML.File,...) HTML(unclass(x),File=File)

#----------------------------------------------------------------------------------------------------#

"HTML.listof" <- function (x, File=.HTML.File,...) 
{
    nn <- names(x)
    ll <- length(x)
    if (length(nn) != ll) 
        nn <- paste("Component ", seq(ll))
    for (i in seq(length = ll)) {
        HTMLli(paste(nn[i],":\n<BR>",sep=""),File=File)
        HTML(x[[i]], File=File)
    }
    invisible(x)
}

#----------------------------------------------------------------------------------------------------#

"HTML.ts" <- function (x, calendar, File=.HTML.File,...) 
{
    x.orig <- x
    x <- as.ts(x)
    fr.x <- frequency(x)
    if (missing(calendar)) 
        calendar <- any(fr.x == c(4, 12))
    if (!calendar) 
        header <- function(x) {
            if ((fr.x <- frequency(x)) != 1) 
		HTML(paste("\n<BR><B>Time series</B>:\n<BR><LI>Start=",deparse(start(x)),"\n<BR><LI>End=",deparse(end(x)),"\n<BR><LI>Frequency=",deparse(fr.x),"\n<BR>",sep=""),File=File)
            else
            HTML(paste("\n<BR><B>Time series</B>:\n<BR><LI>Start=",format(tsp(x)[1]),"\n<BR><LI>End=",format(tsp(x)[2]),"\n<BR><LI>Frequency=",deparse(fr.x),"\n<BR>",sep=""),File=File)
	        }
    if (NCOL(x) == 1) {
        if (calendar) {
            if (fr.x > 1) {
                dn2 <- if (fr.x == 12) 
                  month.abb
                else if (fr.x == 4) {
                  c("Qtr1", "Qtr2", "Qtr3", "Qtr4")
                }
                else paste("p", 1:fr.x, sep = "")
                if (NROW(x) <= fr.x && start(x)[1] == end(x)[1]) {
                  dn1 <- start(x)[1]
                  dn2 <- dn2[1 + (start(x)[2] - 2 + seq(along = x))%%fr.x]
                  x <- matrix(format(x, ...), nrow = 1, byrow = TRUE, 
                    dimnames = list(dn1, dn2))
                }
                else {
                  start.pad <- start(x)[2] - 1
                  end.pad <- fr.x - end(x)[2]
                  dn1 <- start(x)[1]:end(x)[1]
                  x <- matrix(c(rep("", start.pad), format(x, 
                    ...), rep("", end.pad)), nc = fr.x, byrow = TRUE, 
                    dimnames = list(dn1, dn2))
                }
            }
            else {
                tx <- time(x)
                attributes(x) <- NULL
                names(x) <- tx
            }
        }
        else {
            header(x)
            attr(x, "class") <- attr(x, "tsp") <- attr(x, "na.action") <- NULL
        }
    }
    else {
        if (calendar && fr.x > 1) {
            tm <- time(x)
            t2 <- 1 + round(fr.x * ((tm + 0.001)%%1))
            p1 <- format(floor(tm))
            rownames(x) <- if (fr.x == 12) 
                paste(month.abb[t2], p1, sep = " ")
            else paste(p1, if (fr.x == 4) 
                c("Q1", "Q2", "Q3", "Q4")[t2]
            else format(t2), sep = " ")
        }
        else {
            if (!calendar) 
                header(x)
            rownames(x) <- format(time(x))
        }
        attr(x, "class") <- attr(x, "tsp") <- attr(x, "na.action") <- NULL
    }
    NextMethod("HTML", x, File=File, ...)
    invisible(x.orig)
}

#----------------------------------------------------------------------------------------------------#

"HTML.list"<- function (x, File=.HTML.File,...) 
{
	# Writing to the file a <TABLE>
	list.name<-deparse(substitute(x))

	HTML(paste("<TABLE ALIGN=CENTER WIDTH=90%><TR CLASS=ListBackTitle> List: ","</TR><TR CLASS=ListBackMain><TD ALIGN=left CLASS=ListBackMain>",sep=""),File=File)
	

 	### Functions created by Mark.Bravington to recursively handle lists ###
	
	my.index<- function( var, ...) {
	  pg<- .Primitive( '[[')
	  vv<- as.name( 'var')
	  for( i in c(...)) vv<- call( 'pg', vv, i)
	  eval( vv)}

	my.index.exists<- function( i, a.list) {
	  for( ii in 1:length( i))
	    if( i[ ii] > length( a.list))
	return( FALSE)
	    else
	      a.list<- a.list[[ i[ ii] ]]
	return( TRUE) }

 	### Function do.at.terminal created by Mark.Bravington modified by Eric Lecoutre - 11/06/2002 ###

	HTML.at.terminals<- function( the.list,File=.HTML.File,...) {
	  assign( '[[', my.index)
	  i<- 1
	  while( my.index.exists( i, the.list)) { 
	    if( (is.recursive( the.list[[ i]])) & (mode(the.list[[i]])!="function")        )
	      i<- c( i, 1)
	    else {
		i.name<-paste("<I>Node at indices: ",if (length(i)>1) paste(i,collapse=",") else i,"</I>",sep="")
		HTML(paste(if (length(i)>1) paste(rep("<UL>",length(i)),collapse="") else "<UL>","<LI>",i.name,"<BR>",if (length(i)>1) paste(rep("</UL>",length(i)),collapse="") else "</UL>",sep=""),File=File)
		HTML(the.list[[i]],File=File)
		HTMLbr(File=File)
	      while( (length( i)>1) && (length( the.list[[ i[ -length(i)] ]]) == i[length( i)]) )
		i<- i[ -length( i)]
	   	i[ length(i)]<- i[ length( i)]+1
	    }
	  }
	  NULL
	}

	# Let's do the job
	HTML.at.terminals(x)


	HTML("</TD></TR></TABLE>",File=File)
}





#----------------------------------------------------------------------------------------------------#

"HTML.data.frame"<-
function(x, File = .HTML.File, ...)
{
		
	x<-as.matrix(x)
	NextMethod("HTML",Border=0,...)
	
}

#----------------------------------------------------------------------------------------------------#

"HTML.matrix"<-
function(x, File = .HTML.File, Border = 1, classfirstline = "firstline", classfirstcolumn = 
	"firstcolumn", classcellinside = "cellinside",  digits=6,...)
{
	if (is.numeric(x)) x<-round(x,digits=digits)
	txt <- paste("<P ALIGN=CENTER>",if (Border==1) "<TABLE CELLSPACING=0 BORDER=1><TD>","<TABLE CELLSPACING=0>", sep = "")
	if(is.null(dimnames(x)[[2]]) == FALSE) {
		VecDebut <- c(if(is.null(dimnames(x)[[1]]) == FALSE) paste(
				"<TH>", sep = ""), 
			rep(paste("<TH>", sep = ""), dim(
			x)[2] - 1))
		VecMilieu <- c(if(is.null(dimnames(x)[[1]]) == FALSE) "", 
			as.character(dimnames(x)[[2]]))
		VecFin <- c(if(is.null(dimnames(x)[[1]]) == FALSE) "</TH>", rep(
			"</TH>", dim(x)[2] - 1), "</TH>")
		txt <- paste(txt,"<TR CLASS=",classfirstline,">", paste(VecDebut, VecMilieu, VecFin, sep = "", 
			collapse = ""),"</TR>")
	}
	for(i in 1:dim(x)[1]) {
		if(i == 1) {
			VecDebut <- c(if(is.null(dimnames(x)[[1]]) == FALSE) paste(
				  "<TR><TD CLASS=", classfirstcolumn, ">", sep = ""), 
				paste("<TD CLASS=", classcellinside, ">", sep = ""), 
				rep(paste("<TD CLASS=", classcellinside, ">", sep = 
				""), dim(x)[2] - 1))
			VecMilieu <- c(if(is.null(dimnames(x)[[1]]) == FALSE) 
				  dimnames(x)[[1]][i], HTMLReplaceNA(as.matrix(
				x[i,  ])))
			VecFin <- c(if(is.null(dimnames(x)[[1]]) == FALSE) "</TD>", 
				rep("</TD>", dim(x)[2] - 1), "</TD></TR>")
		}
		else {
			VecDebut <- c(if(is.null(dimnames(x)[[1]]) == FALSE) paste(
				  "<TR><TD CLASS=", classfirstcolumn, ">", sep = ""), 
				paste(rep(paste("<TD CLASS=", classcellinside, ">", sep
				 = ""), dim(x)[2])))
			VecMilieu <- c(if(is.null(dimnames(x)[[1]]) == FALSE) 
				  dimnames(x)[[1]][i], HTMLReplaceNA(as.matrix(
				x[i,  ])))
			VecFin <- c(if(is.null(dimnames(x)[[1]]) == FALSE) "</TD>", 
				rep("</TD>", dim(x)[2] - 1), "</TD></TR>")
		}
		txt <- paste(txt, paste(VecDebut, VecMilieu, VecFin, sep = "", 
			collapse = ""))
	}
	txt <- paste(txt, "</TABLE>",if (Border==1)"</TD></TABLE>","</P><BR>")
	cat(txt, "\n", file = File, sep = "", append = TRUE)
}
	
#----------------------------------------------------------------------------------------------------#

"HTML.structure"<-
function(x, a = attributes(x), prefix = "", File = .HTML.File, ...)
{
	n <- length(dim(x))
	nn <- names(a)
	ate <- character(0)
	if(n > 0) {
		if(n == 2)
			HTML.matrix(x, File = File, ...)
		else HTML.array(x, File = File, ...)
		ate <- c("dim", "dimnames")
		if(n == 1)
			ate <- c(ate, "names")
	}
	else if(!is.atomic(x)) {
		HTML(as.vector(x), File = File, ...)
		ate <- "names"
	}
	else if(length(tsp(x))) {
		HTML.ts(x, File = File, ...)
		ate <- "tsp"
	}
	else if(length(names(x))) {
		HTML.matrix(matrix(x, 1, dimnames = list("", names(x))), 
			File = File, ...)
		ate <- "names"
	}
	else HTML(as.vector(x), File = File, ...)
	ii <- !match(nn, ate, nomatch = FALSE)
	nn <- nn[ii]
	a <- a[ii]
	for(i in seq(nn)) {
		this <- paste("attr(", prefix, ", \"", nn[i], "\")", sep = "")
		HTML(this, File=File)
		HTML(a[[i]], File = File,  ...)
	}
	invisible(x)
}

#----------------------------------------------------------------------------------------------------#

"HTMLCommand" <- function(x,File=.HTML.File,Num="",menu=FALSE,target="index<-main.html")
	{

	if (menu==TRUE)
	cat(paste("<BR><LI><A CLASS=command HREF='./",target,"#Num",Num,"' TARGET=main> ",paste(x,collapse=""),"</A>",sep=""),file=File,append=TRUE,sep="")
	else {
	if (Num!="") cat(paste("<A NAME=Num",Num,">&nbsp;</A>",sep=""),file=File,append=TRUE,sep="")
	cat(paste("<P><XMP CLASS=command>> ",x,"</XMP></P>",sep=""),file=File,append=TRUE,sep="")
	}
	}

#----------------------------------------------------------------------------------------------------#

"HTML.title"<-
function(x, HR = 2, class = "bigtitle", File = .HTML.File, ...)
{
	cat(paste("\n <H", HR, " class=", class, "> ", x, "</H", HR, ">", sep = 
		""), file = File, append = TRUE, sep = "")
}

#----------------------------------------------------------------------------------------------------#

"HTMLReplaceNA"<-
function(Vec, Replace = " ")
{
	Vec <- as.character(Vec)
	
	for(i in 1:length(Vec)) 
	{
		try(if((Vec[i] == "NA") | (Vec[i] == "NaN") | is.na(Vec[i])){ Vec[i] <- Replace})
	}	
	Vec
}


#----------------------------------------------------------------------------------------------------#

"as.title"<-
function(x)
{
	if (!is.character(x)) {
		x<-try(as.character(x))
		if (!is.character(x)) stop("Input argument must be of character mode")
	}
	class(x) <- "title"
	return(x)
}

#----------------------------------------------------------------------------------------------------#

"HTML.connection" <- function(x,File=.HTML.File,...) HTML(unlist(summary(x)),File=File)

#----------------------------------------------------------------------------------------------------#

"HTML.socket" <- function (x, File=.HTML.File,...) 
{
    if (length(port <- as.integer(x$socket)) != 1) 
        stop("invalid `socket' argument")
    HTML(paste("Socket connection #", x$socket, "to", x$host, "on port", 
        x$port, "\n<BR>",sep=""),File=File)
    invisible(x)
}
 
#----------------------------------------------------------------------------------------------------#

"HTML.htest" <- function (x, digits = 4, quote = TRUE, prefix = "",File=.HTML.File, ...) 
{
            HTML(as.title(x$method),File=File)
            HTMLli(paste("\n data:",x$data.name,"\n",sep=""),File=File)
           out <- character()
            if (!is.null(x$statistic)) 
                        out <- c(out, paste(names(x$statistic), "=<B>", format(round(x$statistic,4)),"</B>"))
            if (!is.null(x$parameter)) 
                        out <- c(out, paste(names(x$parameter), "=<B>", format(round(x$parameter,3)),"</B>"))
            if (!is.null(x$p.value)) 
                        out <- c(out, paste("p-value =<B>", format.pval(x$p.value,digits = digits),"</B>"))
            HTMLli(paste(out,collapse=" , "),File=File)
    if (!is.null(x$alternative)) {
        HTMLli("alternative hypothesis: ")
        if (!is.null(x$null.value)) {
            if (length(x$null.value) == 1) {
               alt.char <- switch(x$alternative, two.sided = "not equal to", 
                  less = "less than", greater = "greater than")
                HTML(paste("true", names(x$null.value), "is", alt.char, 
                 x$null.value, "\n"),File=File)
            }
            else {
               HTMLli(paste(x$alternative, "\nnull values:\n<BR>"),File=File)
               HTML(x$null.value, File=File)
            }
        }
        else HTMLli(paste(x$alternative, "\n<BR>"),File=File)
    }
    if (!is.null(x$conf.int)) {
        HTMLli(paste("<B>",format(100 * attr(x$conf.int, "conf.level")), "</B> percent confidence interval:\n", 
         "<B>[", paste(format(c(x$conf.int[1], x$conf.int[2])),sep="",collapse=" ;"),"]</B>",sep=""),File=File)
    }
    if (!is.null(x$estimate)) {
        HTMLli(paste("sample estimates:<B>\n",x$estimate,"</B>",sep=""),File=File)
    }
    invisible(x)
}
 
#----------------------------------------------------------------------------------------------------#
 
"HTML.atomic"<- function(x, File = .HTML.File, ...){ cat(paste("<P>",paste(x,collapse="&nbsp; "),"</P>",sep="",collapse=""), file= File, append = TRUE, sep = " ")}

#----------------------------------------------------------------------------------------------------#
 
 "HTML.aov" <- function (x, intercept = FALSE, tol = .Machine$double.eps^0.5, File=.HTML.File,...) 
{
    if (!is.null(cl <- x$call))  HTMLli(paste("Call:\n<BR>", deparse(cl)),File=File)
    asgn <- x$assign[x$qr$pivot[1:x$rank]]
    effects <- x$effects
    if (!is.null(effects)) 
        effects <- as.matrix(effects)[seq(along = asgn), , drop = FALSE]
    rdf <- x$df.resid
    uasgn <- unique(asgn)
    nmeffect <- c("(Intercept)", attr(x$terms, "term.labels"))[1 + uasgn]
    nterms <- length(uasgn)
    nresp <- NCOL(effects)
    df <- numeric(nterms)
    ss <- matrix(NA, nterms, nresp)
    if (nterms) {
        for (i in seq(nterms)) {
            ai <- asgn == uasgn[i]
           df[i] <- sum(ai)
            ef <- effects[ai, , drop = FALSE]
            ss[i, ] <- if (sum(ai) > 1) 
                colSums(ef^2)
            else ef^2       }
        keep <- df > 0
        if (!intercept && uasgn[1] == 0) 
            keep[1] <- FALSE
        nmeffect <- nmeffect[keep]
        df <- df[keep]
        ss <- ss[keep, , drop = FALSE]
        nterms <- length(df)    }
    HTMLli("Terms:\n<BR>",File=File)
    if (nterms == 0) {
        if (rdf > 0) {
            ss <- colSums(as.matrix(x$residuals)^2)
            ssp <- sapply(ss, format)
            if (!is.matrix(ssp)) 
                ssp <- t(ssp)
            tmp <- as.matrix(c(ssp, format(rdf)))
            if (length(ss) > 1) {
                rn <- colnames(x$fitted)
                if (is.null(rn)) 
                  rn <- paste("resp", 1:length(ss))
            }
            else rn <- "Sum of Squares"
            dimnames(tmp) <- list(c(rn, "Deg. of Freedom"), "Residuals")
            HTML(as.data.frame(tmp), File=File,..)
            HTMLli(paste("Residual standard error:", paste(sapply(sqrt(ss/rdf),format),collapse=" "), "\n"),File=File)
        }
        else HTML.matrix(matrix(0, 2, 1, dimnames = list(c("Sum of Squares","Deg. of Freedom"), "<empty>")),File=File)
    }
    else {
        if (rdf > 0) {
            resid <- as.matrix(x$residuals)
            nterms <- nterms + 1
            df <- c(df, rdf)
            ss <- rbind(ss, colSums(resid^2))
            nmeffect <- c(nmeffect, "Residuals")        }
        ssp <- apply(zapsmall(ss), 2, format)
        tmp <- t(cbind(ssp, format(df)))
        if (ncol(effects) > 1) {
            rn <- colnames(x$coef)
            if (is.null(rn)) 
                rn <- paste("resp", seq(ncol(effects)))        }
        else rn <- "Sum of Squares"
        dimnames(tmp) <- list(c(rn, "Deg. of Freedom"), nmeffect)
        HTML(as.data.frame(tmp), File=File)
       rank <- x$rank
        int <- attr(x$terms, "intercept")
        nobs <- NROW(x$residuals) - !(is.null(int) || int ==      0)
        if (rdf > 0) {
            rs <- sqrt(colSums(as.matrix(x$residuals)^2)/rdf)
            HTMLli(paste("Residual standard error:", paste(sapply(rs,format),collapse=" "), "\n"),File=File)       }
        coef <- as.matrix(x$coef)[, 1]
        R <- x$qr$qr
       R <- R[1:min(dim(R)), , drop = FALSE]
        R[lower.tri(R)] <- 0
        if (rank < (nc <- length(coef))) {
            HTMLli(paste(nc - rank, "out of", nc, "effects not estimable\n"),File=File)
            R <- R[, 1:rank, drop = FALSE]        }
        d2 <- sum(abs(diag(R)))
        diag(R) <- 0
        if (sum(abs(R))/d2 > tol) 
            HTMLli("Estimated effects may be unbalanced\n",File=File)
        else HTMLli("Estimated effects are balanced\n",File=File)
    }
    invisible(x)
}

#----------------------------------------------------------------------------------------------------#

"HTML.anova" <- function (x, digits = max(getOption("digits") - 2, 3), signif.stars = getOption("show.signif.stars"),File=.HTML.File,...) 
{
    if (!is.null(heading <- attr(x, "heading"))) 
        HTML(paste("<P><B>",heading, "</B></P>"),File=File)
   nc <- (d <- dim(x))[2]
    if (is.null(cn <- colnames(x))) 
        stop("anova object must have colnames(.)!")
   ncn <- nchar(cn)
    has.P <- substr(cn[nc], 1, 3) == "Pr("
    zap.i <- 1:(if (has.P) nc - 1 else nc)
    i <- which(substr(cn, 2, 7) == " value")
    i <- c(i, which(!is.na(match(cn, c("FALSE", "Cp", "Chisq")))))
    if (length(i)) 
        zap.i <- zap.i[!(zap.i %in% i)]
    tst.i <- i
    if (length(i <- which(substr(cn, ncn - 1, ncn) == "Df"))) 
        zap.i <- zap.i[!(zap.i %in% i)]
    HTML.coefmat(x, digits = digits, signif.stars = signif.stars, 
        has.Pvalue = has.P, P.values = has.P, cs.ind = NULL, 
        zap.ind = zap.i, tst.ind = tst.i, na.print = "", File=File)
    invisible(x)
}
 
#----------------------------------------------------------------------------------------------------#

"HTML.glm" <- function (x, digits = max(3, getOption("digits") - 3), na.print = "", File=.HTML.File,...) 
{
    HTMLli(paste("Call: ", deparse(x$call), "\n<BR>\n<BR>"),File=File)
    HTMLli("Coefficients",File=File)
    if (is.character(co <- x$contrasts)) 
        HTML(paste("  [contrasts: ", apply(cbind(names(co), co), 1, 
            paste, collapse = "="), "]"),File=File)
    HTMLbr(File=File)
    HTML(format(x$coefficients, digits = digits),File=File)
    HTMLli(paste("\nDegrees of Freedom:<B>", x$df.null, "</B>Total (i.e. Null);<B> ", 
        x$df.residual, "</B> Residual\n"),File=File)
    HTMLli(paste("Null Deviance:<B>    ", format(signif(x$null.deviance, 
        digits)), "</B> &nbsp;&nbsp; Residual Deviance:<B>", format(signif(x$deviance, 
        digits)), " </B>&nbsp;&nbsp;    AIC:<B>  ", format(signif(x$aic, digits)), "</B>\n<BR>"),File=File)
    invisible(x)
}


#----------------------------------------------------------------------------------------------------#

 "HTML.tables.aov" <-  function (x, digits = 4, File=.HTML.File,...) 
 {
HTML("<CENTER>",File=File)
     tables.aov <- x$tables
     n.aov <- x$n
     se.aov <- if (se <- !is.na(match("se", names(x)))) 
         x$se
     type <- attr(x, "type")
     switch(type, effects = HTML("<P CLASS=partitle>Tables of effects\n</P>",File=File), means = HTML("<P CLASS=partitle>Tables of means\n</P>",File=File), 
         residuals = if (length(tables.aov) > 1) 
             HTML("<P CLASS=partitle>Table of residuals from each stratum\n</P>",File=File))
     if (!is.na(ii <- match("Grand mean", names(tables.aov)))) {
         HTML("<P>Grand mean\n</P>",File=File)
         gmtable <- tables.aov[[ii]]
         HTML.mtable(gmtable, digits = digits, File=File)
     }
     for (i in names(tables.aov)) {
         if (i == "Grand mean") 
             next
         table <- tables.aov[[i]]
         HTML(paste("\n<P>", i, "\n</P>"),File=File)
         if (!is.list(n.aov)) 
             HTML.mtable(table, digits = digits,File=File, ...)
         else {
             n <- n.aov[[i]]
             if (length(dim(table)) < 2) {
                 table <- rbind(table, n)
                 rownames(table) <- c("", "rep")
                 HTML(table, digits = digits, File=File)
             }
             else {
                 ctable <- array(c(table, n), dim = c(dim(table), 
                   2))
                 dim.t <- dim(ctable)
                 d <- length(dim.t)
                 ctable <- aperm(ctable, c(1, d, 2:(d - 1)))
                 dim(ctable) <- c(dim.t[1] * dim.t[d], dim.t[-c(1, 
                   d)])
                 dimnames(ctable) <- c(list(format(c(rownames(table), 
                   rep("rep", dim.t[1])))), dimnames(table)[-1])
                 ctable <- eval(parse(text = paste("ctable[as.numeric(t(matrix(seq(nrow(ctable)),ncol=2)))", 
                   paste(rep(", ", d - 2), collapse = " "), "]")))
                 names(dimnames(ctable)) <- names(dimnames(table))
                 class(ctable) <- "mtable"
                 HTML.mtable(ctable, digits = digits,File=File, ...)
             }
         }
     }
     if (se) {
         if (type == "residuals") 
             rn <- "df"
         else rn <- "replic."
         switch(attr(se.aov, "type"), effects = HTML("\n<P CLASS=partitle>Standard errors of effects\n</P>",File=File), 
             means = HTML("\n<P CLASS=partitle>Standard errors for differences of means\n</P>",File=File), 
             residuals = HTML("\n<P CLASS=partitle>Standard errors of residuals\n</P>",File=File))
         if (length(unlist(se.aov)) == length(se.aov)) {
             n.aov <- n.aov[!is.na(n.aov)]
             se.aov <- unlist(se.aov)
             cn <- names(se.aov)
             se.aov <- rbind(format(se.aov, digits = digits), 
                 format(n.aov))
             dimnames(se.aov) <- list(c(" ", rn), cn)
             HTML.matrix(se.aov,File=File)
         }
         else for (i in names(se.aov)) {
             se <- se.aov[[i]]
             if (length(se) == 1) {
                 se <- rbind(se, n.aov[i])
                 dimnames(se) <- list(c(i, rn), "")
                 HTML(se, File=File)
             }
             else {
                 dimnames(se)[[1]] <- ""
                 HTML(paste("\n<P>", i, "\n</P>"),File=File)
                 HTML("When comparing means with same levels of:\n<BR>",File=File)
                 HTML(se, File=File, ...)
                 HTML(paste("replic.", n.aov[i], "\n<BR>"),File=File)
             }
         }
     }
	HTML("</CENTER>",File=File)
     invisible(x)
 }


#----------------------------------------------------------------------------------------------------#

"HTML.mtable" <- function (x, digits = getOption("digits"),File=.HTML.File,...) 
{
    xxx <- x
    xx <- attr(x, "Notes")
    nn <- names(dimnames(x))
    a.ind <- match(names(a <- attributes(x)), c("dim", "dimnames", 
        "names"))
    a <- a[!is.na(a.ind)]
    class(x) <- attributes(x) <- NULL
    attributes(x) <- a
    if (length(x) == 1 && is.null(names(x)) && is.null(dimnames(x))) 
        names(x) <- rep("", length(x))
    if (length(dim(x)) && is.numeric(x)) {
        xna <- is.na(x)
        x <- format(zapsmall(x, digits))
        x[xna] <- "  "
    }
    HTML(x, File=File, ...)
    if (length(xx)) {
        HTML("\n<BR>Notes:\n<BR>",File=File)
        HTML(xx,File=File)
    }
    invisible(xxx)
}

#----------------------------------------------------------------------------------------------------#

"HTML.integrate" <- function (x, digits = getOption("digits"), File=.HTML.File,...) 
{
    if (x$message == "OK") 
        HTML(paste("<P>",format(x$value, digits = digits), " with absolute error < ", 
            format(x$abs.error, digits = 2), "\n</P>", sep = ""),File=File)
    else HTML(paste("<P>failed with message `", x$message, "'\n</P>", sep = ""),File=File)
    invisible(x)
}


#----------------------------------------------------------------------------------------------------#

"HTML.summary.lm.null" <- function (x, digits = max(3, getOption("digits") - 3), File=.HTML.File,...) 
{
    
    HTMLli(paste("<BR><P>Call: ", paste(deparse(x$call), sep = "\n<BR>", collapse = "\n<BR>"), "</P>" ),File=File)
    resid <- x$residuals
    df <- x$df
    rdf <- df[2]
    if (rdf > 5) {
        HTMLli("Residuals:\n<BR>",File=File)
        if (length(dim(resid)) == 2) {
            rq <- apply(t(resid), 1, quantile)
            dimnames(rq) <- list(c("Min", "1Q", "Median", "3Q", 
                "Max"), dimnames(resid)[[2]])
        }
        else {
            rq <- quantile(resid)
            names(rq) <- c("Min", "1Q", "Median", "3Q", "Max")
        }
        HTML(round(rq, digits) ,File=File)
    }
    else if (rdf > 0) {
        HTMLli("Residuals:\n<BR>",File=File)
        HTML(round(resid, digits ), File=File)
    }
    else HTMLli("\n<BR>No Coefficients:\n<BR>",File=File)
    HTMLli(paste("\n<BR>Residual standard error:<B> ", format(signif(x$sigma, 
        digits)), "on <B> ", rdf, " </B>degrees of freedom\n<BR><BR>",sep=""),File=File)
    invisible(x)
}
 
#----------------------------------------------------------------------------------------------------#

"HTML.summary.glm" <- function (x, digits = max(3, getOption("digits") - 3), na.print = "", 
    symbolic.cor = p > 4, signif.stars = getOption("show.signif.stars"), File=.HTML.File,
    ...) 
{
    HTMLli(paste("\n<P>Call: ",paste(deparse(x$call),collapse=" ")),File=File)
    
    HTML("<P>Deviance Residuals: \n</P>",File=File)
    if (x$df.residual > 5) {
        x$deviance.resid <- quantile(x$deviance.resid, na.rm = TRUE)
        names(x$deviance.resid) <- c("Min", "1Q", "Median", "3Q", 
            "Max")
    }
    HTML.default(round(x$deviance.resid,digits) , File=File)
    HTML("\n<P>Coefficients:\n</P>",File=File)
    HTML.coefmat(x$coef, signif.stars = signif.stars, File=File)
    
    HTML(paste("\n<P>(Dispersion parameter for ", x$family$family, " family taken to be ", 
        format(x$dispersion), ")\n</P>\n"),File=File)
        
       HTML(paste("<LI>Null deviance:<B>", x[c("null.deviance")], "</B> on <B>", x[c("df.null")],"</B> degrees of freedom."),File=File)
       
       HTML(paste("<LI>Residual deviance:<B>", x[c("deviance")], "</B> on <B>", x[c("df.residual")],"</B> degrees of freedom."),File=File)
       
       
       HTML(paste("<P>AIC:<B> ", format(x$aic, digits = max(4, digits + 1)), "</B>\n</P>\n<P>Number of Fisher Scoring iterations: <B>",     x$iter, "</B>\n</P>", sep = ""),File=File)
    correl <- x$correlation
    if (!is.null(correl)) {
        p <- NCOL(correl)
        if (p > 1) {
            HTML("\n<P>Correlation of Coefficients:\n</P>")
            if (symbolic.cor) 
                HTML(symnum(correl)[-1, -p],File=File)
            else {
                correl[!lower.tri(correl)] <- NA
                HTML(correl[-1, -p, drop = FALSE], File=File)
            }
        }
    }
    HTMLbr(File=File)
    invisible(x)
}


#----------------------------------------------------------------------------------------------------#

"HTML.hsearch" <- function (x, File=.HTML.File,...) 
{
    fields <- paste(x$fields, collapse = " or ")
    db <- x$matches
    if (NROW(db) > 0) {
        HTML(paste("<P>Help files with ", fields, " matching `", 
            x$pattern, "',\n", "type `help(FOO, package = PKG)' to inspect ", 
            "entry `FOO(PKG) TITLE':", "\n</P>", sep = ""), File=File)
        dbnam <- paste(db[, "name"], "(", db[, "Package"], ")",sep = "")
        dbtit <- paste(db[, "title"], sep = "")
        HTML(cbind(dbnam, dbtit), File=File)
    }
    else HTML(paste("<P>No help files found with ", fields, " matching `", x$pattern, "'\n</P>", sep = ""),File=File)
}

#----------------------------------------------------------------------------------------------------#

"HTML.aovlist" <- function (x, File=.HTML.File,...) 
{
    cl <- attr(x, "call")
    if (!is.null(cl)) {
        cat("\nCall:\n")
        dput(cl)
    }
    if (!is.null(attr(x, "weights"))) 
        cat("Note: The results below are on the weighted scale\n")
    nx <- names(x)
    if (nx[1] == "(Intercept)") {
        mn <- x[[1]]$coef
        if (is.matrix(mn)) {
            cat("\nGrand Means:\n")
            print(format(mn[1, ]), quote = FALSE)
        }
        else cat("\nGrand Mean:", format(mn[1]), "\n")
        nx <- nx[-1]
    }
    for (ii in seq(along = nx)) {
        i <- nx[ii]
        cat("\nStratum ", ii, ": ", i, "\n", sep = "")
        xi <- x[[i]]
        print(xi, ...)
    }
    invisible(x)
}


#----------------------------------------------------------------------------------------------------#

"HTML.SavedPlots" <- function (x, File=.HTML.File,...) 
{
    if (x[[1]] != 31416) {
        HTML("<P>object is not of class `SavedPlots'</P>\n<BR>",File=File)
        return()
    }
    HTML("<P>Saved Plots from R version 1.4.0 or later</P>\n<BR>\n<BR>")
    HTML("  Contains", x[[2]], "out of a maximum", x[[3]], "plots\n")
    lens <- sapply(x[[5]], length)[1:x[[2]]]
    cat("  #plot calls are", paste(lens, collapse = ", "), "\n")
    cat("  Current position is plot", 1 + x[[4]], "\n")
}

#----------------------------------------------------------------------------------------------------#

"HTML.ordered" <- function (x, quote = FALSE,File=.HTML.File, ...) 
{
    if (length(x) <= 0) 
        HTML("\n<P>ordered(0)\n</P>",File=File)
    else HTML(as.character(x), File,File, ...)
    HTML(paste("\n<P>Levels: ", paste(levels(x), collapse = " < "), "\n</P>"),File=File)
    invisible(x)
}

#----------------------------------------------------------------------------------------------------#

"HTML.difftime" <- function (x, digits = getOption("digits"),File=.HTML.File, ...) 
{
    if (length(x) > 1) 
        HTML(paste("<P>Time differences of ", paste(format(unclass(x), 
            digits = digits), collapse = ", "), " ", attr(x, 
            "units"), "\n</P>", sep = ""),File=File)
    else HTML(paste("<P>Time difference of ", format(unclass(x), digits = digits), 
        " ", attr(x, "units"), "\n", sep = ""),File=File)
    invisible(x)
}


#----------------------------------------------------------------------------------------------------#

"HTML.dummy.coef" <- function (x, ..., File=.HTML.File,title) 
{
    terms <- names(x)
    n <- length(x)
    nm <- max(sapply(x, length))
    ans <- matrix("", 2 * n, nm)
    rn <- rep("", 2 * n)
    line <- 0
    for (j in seq(n)) {
        this <- x[[j]]
        n1 <- length(this)
        if (n1 > 1) {
            line <- line + 2
            ans[line - 1, 1:n1] <- names(this)
            ans[line, 1:n1] <- format(this, ...)
            rn[line - 1] <- paste(terms[j], ":   ", sep = "")
        }
        else {
            line <- line + 1
            ans[line, 1:n1] <- format(this, ...)
            rn[line] <- paste(terms[j], ":   ", sep = "")
        }
    }
    rownames(ans) <- rn
    colnames(ans) <- rep("", nm)
    HTML(paste("\n<P>",if (missing(title)) 
        "Full coefficients are"
    else title, "\n</P>"),File=File)
    HTML.matrix(ans[1:line, , drop = FALSE],File=File)
    invisible(x)
}


#----------------------------------------------------------------------------------------------------#

"HTML.dummy.coef.list" <- function (x, File=.HTML.File,...) 
{
    for (strata in names(x)) HTML.dummy.coef(x[[strata]], ...,File=File, title = paste("\n<P>     Error:", strata,"</P>"))
    invisible(x)
}


#----------------------------------------------------------------------------------------------------#

 "HTML.glm.null" <- function (x, digits = max(3, getOption("digits") - 3), na.print = "", 
    File=.HTML.File,...) 
{
    HTMLli(paste(" Call: ", deparse(x$call), "\n<BR>\n"),File=File)
    HTMLli("No coefficients\n<BR>")
    HTMLli(paste("Degrees of Freedom:<B>", length(x$residuals), "</B> Total; <B>", 
        x$df.residual, " </B>Residual\n<BR>"),File=File)
    HTMLli(paste("Null Deviance:<B>", format(signif(x$null.deviance, digits)), 
        "</B>\n<BR>"),File=File)
    HTMLli(paste("Residual Deviance: <B>", format(signif(x$deviance, digits)), 
        " </B><BR>\n"),File=File)
    HTMLli(paste("AIC:<B>", format(signif(x$aic, digits)), "</B><BR>\n"),File=File)
    invisible(x)
}


#----------------------------------------------------------------------------------------------------#

"HTML.libraryIQR" <- function (x,File=.HTML.File, ...) 
{
    sQuote <- function(s) paste("`", s, "'", sep = "")
    db <- x$results
    out <- if (nrow(db) == 0) 
        NULL
    else lapply(split(1:nrow(db), db[, "LibPath"]), function(ind) db[ind, 
        c("Package", "Title"), drop = FALSE])
    first <- TRUE
    for (lib in names(out)) {
        HTML(paste(paste("<P>Packages in library ", 
            sQuote(lib), ":</P>", sep = "")),File=File)
        HTML(cbind(out[[lib]][, "Package"], out[[lib]][, 
            "Title"]), File=File)
        first <- FALSE
    }
    if (first) {
        HTML("<P>no packages found</P>",File=File)    }
    invisible(x)
}

#----------------------------------------------------------------------------------------------------#

"HTML.summary.aov" <- function (x, digits = max(3, getOption("digits") - 3), symbolic.cor = p > 
    4, signif.stars = getOption("show.signif.stars"), File=.HTML.File,...) 
{
    if (length(x) == 1) 
        HTML(x[[1]], File=File)
    else NextMethod()
    invisible(x)
}


#----------------------------------------------------------------------------------------------------#

"HTML.summary.aovlist" <- function (x, File=.HTML.File,...) 
{
    nn <- names(x)
    for (i in nn) {
        HTMLli(paste(i, "\n<BR>", sep = ""),File=File)
        HTML(x[[i]], File=File)
    }
    invisible(x)
}


#----------------------------------------------------------------------------------------------------#

"HTML.summary.glm.null" <- function (x, digits = max(3, getOption("digits") - 3), na.print = "", 
    File=.HTML.File,...) 
{
    HTMLli(paste("\nCall: ",paste(deparse(x$call), sep = "\n", collapse = "\n"), 
        "\n<BR>\n", sep = ""),File=File)
    HTMLli("Deviance Residuals: \n<BR>",File=File)
    if (x$df.residual > 5) {
        x$deviance.resid <- quantile(x$deviance.resid)
        names(x$deviance.resid) <- c("Min", "1Q", "Median", "3Q", 
            "Max")
    }
    HTML.default(x$deviance.resid, digits = digits, na = "",File=File)
    HTMLli("No coefficients\n<BR>")
    HTMLli(paste("\n(Dispersion parameter for ", x$family$family, 
        " family taken to be ", x$dispersion, ")\n\n    Null deviance:<B> ", 
        x$null.deviance, " </B>on <B>", x$df.null, " </B>degrees of freedom\n\n", 
        "Residual deviance: <B>", x$deviance, " </B>on<B> ", x$df.residual, 
        " </B>degrees of freedom\n\n", "Number of Fisher Scoring iterations<B>: ", 
        x$iter, "</B>\n<BR>\n", sep = ""),File=File)
    invisible(x)
}


#----------------------------------------------------------------------------------------------------#

"HTML.summary.manova" <- function (x, digits = getOption("digits"),File=.HTML.File, ...) 
{
    if (length(stats <- x$stats)) {
        HTML.anova(stats,File=File)
    }
    else {
        HTML("<P>No error degrees of freedom</P>\n")
        HTML(data.frame(Df = x$Df, row.names = x$row.names),File=File)
    }
    invisible(x)
}



#----------------------------------------------------------------------------------------------------#

"HTML.summary.table" <- function (x, digits = max(1, getOption("digits") - 3), File=.HTML.File,...) 
{
    if (!inherits(x, "summary.table")) 
        stop("x must inherit from class `summary.table'")
    if (!is.null(x$call)) {
        HTMLli(paste("Call: ", x$call),File=File)
    }
    HTMLli(paste("Number of cases in table:<B>", x$n.cases, "</B>\n<BR>"),File=File)
    HTMLli(paste("Number of factors:<B>", x$n.vars, "</B>\n<BR>"),File=File)
    if (x$n.vars > 1) {
        HTMLli("Test for independence of all factors:\n<BR>",File=File)
        ch <- x$statistic
        HTML(paste(" Chisq = <B>", format(round(ch, max(0, digits - log10(ch)))), 
            "</B>, df = <B>", x$parameter, "</B>, p-value = <B>", format.pval(x$p.value, 
                digits, eps = 0), "</B>\n<BR>", sep = ""),File=File)
        if (!x$approx.ok) 
            HTML("<P>Chi-squared approximation may be incorrect</P>\n",File=File)
    }
    invisible(x)
}


#----------------------------------------------------------------------------------------------------#
"HTML.TukeyHSD" <- function (x, File=.HTML.File, ...) 
{
    HTML("<CENTER><P><B>Tukey multiple comparisons of means</B></P>\n")
    HTML(paste("<P>", format(100 * attr(x, "conf.level"), 2), "% family-wise confidence level</P></CENTER>\n", 
        sep = ""),File=File)
    
    if (attr(x, "ordered")) 
        HTML("<CENTER><P>factor levels have been ordered</P></CENTER>\n",File=File)
    HTMLli(paste("Fit: ", deparse(attr(x, "orig.call")), "\n<BR>\n", sep = ""),File=File)
    attr(x, "orig.call") <- attr(x, "conf.level") <- attr(x, "ordered") <- NULL
	lapply(unclass(x),HTML,File=File,...)
    #HTML.default(unclass(x), File=File,...)
    invisible(return(x))
}


#----------------------------------------------------------------------------------------------------#

"HTML.simple.list" <- function (x, File=.HTML.File,...) 
HTML(noquote(cbind("<-" = unlist(x))), File=File,...)

#----------------------------------------------------------------------------------------------------#

"HTML.noquote" <- function (x, File=.HTML.File,...) 
{
    if (!is.null(cl <- attr(x, "class"))) {
        cl <- cl[cl != "noquote"]
        attr(x, "class") <- (if (length(cl) > 0) 
            cl
        else NULL)
    }
    HTML(x, File=File, ...)
}



###
### PACKAGES FUNCTIONS
###


### PACKAGES 

#----------------------------------------------------------------------------------------------------#

"HTML.ar" <- function (x, digits = max(3, getOption("digits") - 3), File=.HTML.File,...) 
{
    HTMLli(paste("Call:\n", deparse(x$call), "\n\n", sep = ""),File=File)
    nser <- NCOL(x$var.pred)
    if (nser > 1) {
        if (!is.null(x$x.intercept)) 
            res <- x[c("ar", "x.intercept", "var.pred")]
        else res <- x[c("ar", "var.pred")]
        res$ar <- aperm(res$ar, c(2, 3, 1))
        HTML(res, digits = digits,File=File)
    }
    else {
        if (x$order > 0) {
            HTMLli("Coefficients:\n",File=File)
            coef <- drop(round(x$ar, digits = digits))
            names(coef) <- seq(length = x$order)
            HTML.default(coef, File=File)
        }
        if (!is.null(xint <- x$x.intercept) && !is.na(xint)) 
            HTML(paste("<P>Intercept: <B>", format(xint, digits = digits), 
                "</B> (", format(x$asy.se.coef$x.mean, digits = digits), 
                ") ", "\n</P>", sep = ""),File=File)
        HTML(paste("<P>Order selected <B>", x$order, " </B>sigma^2 estimated as <B>", 
            format(x$var.pred, digits = digits), "</B>\n<</P>"),File=File)
    }
    invisible(x)
}

#----------------------------------------------------------------------------------------------------#

"HTML.Arima" <- function (x, digits = max(3, getOption("digits") - 3), se = TRUE, 
    File=.HTML.File,...) 
{
    HTMLli(paste("nCall:", deparse(x$call, width = 75), "", sep = "\n"),File=File)
    HTMLli("Coefficients:\n<BR>",File=File)
    coef <- round(x$coef, digits = digits)
    if (se && nrow(x$var.coef)) {
        ses <- rep(0, length(coef))
        ses[x$mask] <- round(sqrt(diag(x$var.coef)), digits = digits)
        coef <- matrix(coef, 1, dimnames = list(NULL, names(coef)))
        coef <- rbind(coef, s.e. = ses)
    }
    HTML.default(coef,File=File)
    cm <- x$call$method
    if (is.null(cm) || cm != "CSS") 
        HTML(paste("\n<P>sigma^2 estimated as <B>", format(x$sigma2, digits = digits), 
            "</B>:  log likelihood = <B>", format(round(x$loglik, 2)), 
            "</B>,  aic = <B>", format(round(x$aic, 2)), "</B>\n</P>", sep = ""),File=File)
    else HTML("<P>sigma^2 estimated as <B>", format(x$sigma2, digits = digits), 
        "</B>:  part log likelihood =<B> ", format(round(x$loglik, 2)), 
        "</B>\n</P>", sep = "")
    invisible(x)
}


#----------------------------------------------------------------------------------------------------#

"HTML.arima0" <- function (x, digits = max(3, getOption("digits") - 3), se = TRUE, 
    File=.HTML.File,...) 
{
    HTMLli(paste("\nCall:", deparse(x$call, width = 75), "", sep = "\n"),File=File)
    HTMLli("Coefficients:\n<BR>",File=File)
    coef <- round(x$coef, digits = digits)
    if (se && nrow(x$var.coef)) {
        ses <- rep(0, length(coef))
        ses[x$mask] <- round(sqrt(diag(x$var.coef)), digits = digits)
        coef <- matrix(coef, 1, dimnames = list(NULL, names(coef)))
        coef <- rbind(coef, s.e. = ses)
    }
    HTML.default(coef, File=File)
    cm <- x$call$method
    if (is.null(cm) || cm != "CSS") 
        HTML(paste("\n<P>sigma^2 estimated as <B>", format(x$sigma2, digits = digits), 
            "</B>:  log likelihood = <B>", format(round(x$loglik, 2)), 
            "</B>,  aic = <B>", format(round(x$aic, 2)), "</B>\n</P>", sep = ""),File=File)
    else HTML(paste("\n<P>sigma^2 estimated as <B>", format(x$sigma2, digits = digits), 
        "</B>:  part log likelihood =<B> ", format(round(x$loglik, 2)), 
        "</B>\n</P>", sep = ""),File=File)
    invisible(x)
}

#----------------------------------------------------------------------------------------------------#

"HTML.HoltWinters" <- function (x, File=.HTML.File,...) 
{
    HTML(paste("<P><B>Holt-Winters exponential smoothing", if (x$beta == 0) 
        "without"
    else "with", "trend and", if (x$gamma == 0) 
        "without"
    else paste(if (x$beta == 0) 
        "with ", x$seasonal, sep = ""), "seasonal componenent.\n</B></P>"),File=File)
        
    HTMLli(paste("\nCall:\n", deparse(x$call), "\n<BR>"),File=File)
    HTMLli("Smoothing parameters:\n<UL>",File=File)
    HTMLli(paste(" alpha: ", x$alpha, "\n"),File=File)
    HTMLli(paste(" beta: ", x$beta, "\n"),File=File)
    HTMLli(paste(" gamma: ", x$gamma, "\n<BR>"),File=File)
    HTML("</UL>",File=File)
    HTMLli("Coefficients:\n",File=File)
    HTML(t(t(x$coefficients)),File=File)
}


#----------------------------------------------------------------------------------------------------#

"HTML.stl" <- function (x, File=.HTML.File,...) 
{
    HTMLli(paste("Call:\n ",deparse(x$call),"\n<BR>"),File=File)
    
    HTMLli("\nComponents\n",File=File)
    HTML(x$time.series, File=File,...)
    invisible(x)
}

#----------------------------------------------------------------------------------------------------#

"HTML.StructTS" <- function (x, digits = max(3, getOption("digits") - 3), File=.HTML.File,...) 
{
    HTMLli(paste("\nCall:", deparse(x$call, width = 75), "\n", sep = " "),File=File)
    HTMLli("Variances:\n",File=File)
    HTML(x$coef,  digits=digits,File=File)
    invisible(x)
}


#----------------------------------------------------------------------------------------------------#

"HTML.tskernel" <- function (x, digits = max(3, getOption("digits") - 3), File=.HTML.File,...) 
{
    y <- c(rev(x$coef[2:(x$m + 1)]), x$coef)
    i <- -x$m:x$m
    HTML(paste("<P>",attr(x, "name"), "</P>\n"),File=File)
    HTML(paste( paste("coef[", format(i), "] = ", format(y, digits = digits),sep = ""),collapse="<BR>\n", sep = "\n<BR>"),File=File)
}


### PACKAGE CTEST

#----------------------------------------------------------------------------------------------------#

"HTML.pairwise.htest" <- function (x, File=.HTML.File,...) 
{
    HTMLli(paste("Pairwise comparisons using", x$method, "\n<BR>\n<BR>"),File=File)
    HTMLli(paste("data: ", x$data.name, "\n<BR>\n<BR>"),File=File)
    pp <- format.pval(x$p.value, 2, na.form = "-")
    attributes(pp) <- attributes(x$p.value)
    HTML(pp, File=File)
    HTMLli(paste("\nP value adjustment method:", x$p.adjust.method, "\n"),File=File)
}

#----------------------------------------------------------------------------------------------------#

"HTML.power.htest" <- function (x, File=.HTML.File,...) 
{
    HTMLli(paste(x$method,"<BR>"), File=File)
    note <- x$note
    x[c("method", "note")] <- NULL
    HTML(paste(paste(format.char(names(x), width = 15, flag = "+"), 
        format(x), sep = " = 	"), sep = "\n<BR>",collapse="\n<BR>"),File=File)
    if (!is.null(note)) 
        HTML(paste("\n<P>", "NOTE:", note, "\n</P>\n"),File=File)
    else HTMLbr(File=File)
}


#----------------------------------------------------------------------------------------------------#

"HTML.boot" <- function (x, digits = options()$digits, index = 1:ncol(boot.out$t), File=.HTML.File,  ...) 
{
    boot.out <- x
    sim <- boot.out$sim
    cl <- boot.out$call
    t <- matrix(boot.out$t[, index], nrow = nrow(boot.out$t))
    allNA <- apply(t, 2, function(t) all(is.na(t)))
    ind1 <- index[allNA]
    index <- index[!allNA]
    t <- matrix(t[, !allNA], nrow = nrow(t))
    rn <- paste("t", index, "*", sep = "")
    if (length(index) == 0) 
        op <- NULL
    else if (is.null(t0 <- boot.out$t0)) {
        if (is.null(boot.out$call$weights)) 
            op <- cbind(apply(t, 2, mean, na.rm = TRUE), sqrt(apply(t, 
                2, function(t.st) var(t.st[!is.na(t.st)]))))
        else {
            op <- NULL
            for (i in index) op <- rbind(op, imp.moments(boot.out, 
                index = i)$rat)
            op[, 2] <- sqrt(op[, 2])
        }
        dimnames(op) <- list(rn, c("mean", "std. error"))
    }
    else {
        t0 <- boot.out$t0[index]
        if (is.null(boot.out$call$weights)) {
            op <- cbind(t0, apply(t, 2, mean, na.rm = TRUE) - 
                t0, sqrt(apply(t, 2, function(t.st) var(t.st[!is.na(t.st)]))))
            dimnames(op) <- list(rn, c("original", " bias  ", 
                " std. error"))
        }
        else {
            op <- NULL
            for (i in index) op <- rbind(op, imp.moments(boot.out, 
                index = i)$rat)
            op <- cbind(t0, op[, 1] - t0, sqrt(op[, 2]), apply(t, 
                2, mean, na.rm = TRUE))
            dimnames(op) <- list(rn, c("original", " bias  ", 
                " std. error", " mean(t*)"))
        }
    }
    if (cl[[1]] == "boot") {
        if (sim == "parametric") 
            HTML(as.title("PARAMETRIC BOOTSTRAP"),File=File)
        else if (sim == "antithetic") {
            if (is.null(cl$strata)) 
		HTML(as.title("ANTITHETIC BOOTSTRAP"),File=File)
            else 
            HTML(as.title("STRATIFIED ANTITHETIC BOOTSTRAP"),File=File)
            
        }
        else if (sim == "permutation") {
            if (is.null(cl$strata)) 
		HTML(as.title("DATA PERMUTATION"),File=File)
           else HTML(as.title("STRATIFIED DATA PERMUTATION"),File=File)
        }
        else if (sim == "balanced") {
            if (is.null(cl$strata) && is.null(cl$weights)) 
                HTML(as.title("BALANCED BOOTSTRAP"),File=File)
            else if (is.null(cl$strata)) 
                HTML(as.title("BALANCED WEIGHTED BOOTSTRAP"),File=File)
            else if (is.null(cl$weights)) 
		HTML(as.title("STRATIFIED BALANCED BOOTSTRAP"),File=File)
            else HTML(as.title("STRATIFIED WEIGHTED BALANCED BOOTSTRAP"),File=File)
        }
        else {
            if (is.null(cl$strata) && is.null(cl$weights)) 
		HTML(as.title("ORDINARY NONPARAMETRIC BOOTSTRAP"),File=File)
            else if (is.null(cl$strata)) 
 		HTML(as.title("WEIGHTED BOOTSTRAP"),File=File)
             else if (is.null(cl$weights)) 
		HTML(as.title("STRATIFIED BOOTSTRAP"),File=File)
                else HTML(as.title("STRATIFIED WEIGHTED BOOTSTRAP"),File=File)
        }
    }
    else if (cl[[1]] == "tilt.boot") {
        R <- boot.out$R
        th <- boot.out$theta
        if (sim == "balanced") 
		HTML(as.title("BALANCED TITLED BOOTSTRAP"),File=File)
        else HTML(as.title("TILTED BOOTSTRAP"),File=File)
        if ((R[1] == 0) || is.null(cl$tilt) || eval(cl$tilt)) 
            HTML("<P>Exponential tilting used\n</P>",File=File)
        else HTML("<P>Frequency Smoothing used\n</P>",File=File)
        i1 <- 1
        if (boot.out$R[1] > 0) 
            HTML(paste("<P>First", R[1], "replicates untilted,\n</P>"),File=File)
        else {
            HTML(paste("<P>First ", R[2], " replicates tilted to ", 
                signif(th[1], 4), ",\n</P>", sep = ""),File=File)
            i1 <- 2
        }
        if (i1 <= length(th)) {
            for (j in i1:length(th)) HTML(paste("<P>Next ", R[j + 
                1], " replicates tilted to ", signif(th[j], 4), 
                ifelse(j != length(th), ",\n", ".\n</P>"), sep = ""),File=File)
        }
        op <- op[, 1:3]
    }
    else if (cl[[1]] == "tsboot") {
        if (!is.null(cl$indices)) 
		HTML(as.title("TIME SERIES BOOTSTRAP USING SUPPLIED INDICES"),File=File)
            else if (sim == "model") 
            HTML(as.title("MODEL BASED BOOTSTRAP FOR TIME SERIES"),File=File)
        else if (sim == "scramble") {
		HTML(as.title("PHASE SCRAMBLED BOOTSTRAP FOR TIME SERIES"),File=File)
            if (boot.out$norm) 
                HTML("<P>Normal margins used.\n</P>",File=File)
            else HTML("<P>Observed margins used.\n</P>",File=File)
        }
        else if (sim == "geom") {
            if (is.null(cl$ran.gen)) 
                HTML(as.title("STATIONARY BOOTSTRAP FOR TIME SERIES"),File=File)
            else  HTML(as.title("POST-BLACKENED STATIONARY BOOTSTRAP FOR TIME SERIES"),File=File)
		HTML(paste("<P>Average Block Length of", boot.out$l, 
                "\n</P>"),File=File)
        }
        else {
            if (is.null(cl$ran.gen)) 
		HTML("<P>BLOCK BOOTSTRAP FOR TIME SERIES</P>",File=File)
            else HTML("<P>POST-BLACKENED BLOCK BOOTSTRAP FOR TIME SERIES</P>",File=File)
            HTML(paste("<P>Fixed Block Length of", boot.out$l, "\n</P>"),File=File)
        }
    }
    else {
        cat("\n")
        if (sim == "weird") {
            if (!is.null(cl$strata)) 
                HTML(as.title("STRATIFIED BOOTSTRAP FOR CENSORED DATA"),File=File)
       }
        else if ((sim == "ordinary") || ((sim == "model") && 
            is.null(boot.out$cox))) {
            if (!is.null(cl$strata)) 
 		 HTML(as.title("STRATIFIED CASE RESAMPLING BOOTSTRAP FOR CENSORED DATA"),File=File)
        }
        else if (sim == "model") {
            if (!is.null(cl$strata)) 
 
		HTML(as.title("STRATIFIED MODEL BASED BOOTSTRAP FOR COX REGRESSION MODEL"),File=File) 
        }
        else if (sim == "cond") {
            if (!is.null(cl$strata)) 
 	HTML(as.title("STRATIFIED CONDITIONAL BOOTSTRAP"),File=File)
            if (is.null(boot.out$cox)) 
                HTML("<P>FOR CENSORED DATA\n</P>\n",File=File)
            else HTML("<P>FOR COX REGRESSION MODEL\n</P>\n",File=File)
        }
    }
    HTMLli(paste("\nCall: ",deparse(cl)),File=File)
    
    HTMLli("Bootstrap Statistics :\n<BR>",File=File)
    if (!is.null(op)) 
        HTML(op, digits = digits,File=File)
    if (length(ind1) > 0) 
        for (j in ind1) HTML(paste("<P>WARNING: All values of t", 
            j, "* are NA\n</P>", sep = ""),File=File)
    invisible(boot.out)
}

#----------------------------------------------------------------------------------------------------#

"HTML.simplex" <- function (x, File=.HTML.File,...) 
{
    simp.out <- x
    HTML("\n<P><B>Linear Programming Results\n</B></P>\n",File=File)
    cl <- simp.out$call
    HTMLli(paste("Call : ",deparse(cl)),File=File)
	HTML(paste("<P>", if (simp.out$maxi) "Maximization" else "Minimization", " Problem with Objective Function Coefficients\n</P>"),File=File)
    HTML(simp.out$obj,File=File)
    if (simp.out$solved == 1) {
        HTML("\n<P>\nOptimal solution has the following values\n</P>",File=File)
        HTML(simp.out$soln,File=File)
        HTML(paste("<P>The optimal value of the objective ", " function is ", 
            simp.out$value, ".\n</P>", sep = ""),File=File)
    }
    else if (simp.out$solved == 0) {
        HTML("\n<P>\nIteration limit exceeded without finding solution\n</P>",File=File)
        HTML("<P>The coefficient values at termination were\n</P>",File=File)
        HTML(simp.out$soln,File=File)
        HTML(paste("<P>The objective function value was ", simp.out$value, 
            ".\n</P>", sep = ""),File=File)
    }
    else HTML("\n<P>No feasible solution could be found\n</P>",File=File)
}

#----------------------------------------------------------------------------------------------------#

"HTML.saddle.distn" <- function (x, File=.HTML.File,...) 
{
    sad.d <- x
    cl <- sad.d$call
    rg <- range(sad.d$points[, 1])
    mid <- mean(rg)
    digs <- ceiling(log10(abs(mid)))
    if (digs <= 0) 
        digs <- 4
    else if (digs >= 4) 
        digs <- 0
    else digs <- 4 - digs
    rg <- round(rg, digs)
    level <- 100 * sad.d$quantiles[, 1]
    quans <- format(round(sad.d$quantiles, digs))
    quans[, 1] <- paste( format(level), "%     ", sep = "")
    HTML("\n<P><B>Saddlepoint Distribution Approximations\n</B></P>\n",File=File)
    HTMLli(paste("Call : ",paste(deparse(cl),collapse="")),File=File)
    HTML("\n<P>Quantiles of the Distribution\n</P>",File=File)
    HTML(t(t(quans)),File=File)
    HTML(paste("\n<P>\nSmoothing spline used ", nrow(sad.d$points), 
        " points in the range ", rg[1], " to ", rg[2], ".</P>", sep = ""),File=File)
    if (sad.d$LR) 
        HTMLli("Lugananni-Rice approximations used.",File=File)
       HTMLbr(File=File)
    invisible(sad.d)
}

#----------------------------------------------------------------------------------------------------#

"HTML.bootci" <- function (x, hinv = NULL, File=.HTML.File,...) 
{
    ci.out <- x
    cl <- ci.out$call
    ntypes <- length(ci.out) - 3
    nints <- nrow(ci.out[[4]])
    t0 <- ci.out$t0
    if (!is.null(hinv)) 
        t0 <- hinv(t0)
    digs <- ceiling(log10(abs(t0)))
    if (digs <= 0) 
        digs <- 4
    else if (digs >= 4) 
        digs <- 0
    else digs <- 4 - digs
    intlabs <- NULL
    basrg <- strg <- perg <- bcarg <- NULL
    if (!is.null(ci.out$normal)) 
        intlabs <- c(intlabs, "     Normal        ")
    if (!is.null(ci.out$basic)) {
        intlabs <- c(intlabs, "     Basic         ")
        basrg <- range(ci.out$basic[, 2:3])
    }
    if (!is.null(ci.out$student)) {
        intlabs <- c(intlabs, "   Studentized     ")
        strg <- range(ci.out$student[, 2:3])
    }
    if (!is.null(ci.out$percent)) {
        intlabs <- c(intlabs, "    Percentile     ")
        perg <- range(ci.out$percent[, 2:3])
    }
    if (!is.null(ci.out$bca)) {
        intlabs <- c(intlabs, "      BCa          ")
        bcarg <- range(ci.out$bca[, 2:3])
    }
    level <- 100 * ci.out[[4]][, 1]
    if (ntypes == 4) 
        n1 <- n2 <- 2
    else if (ntypes == 5) {
        n1 <- 3
        n2 <- 2
    }
    else {
        n1 <- ntypes
        n2 <- 0
    }
    ints1 <- matrix(NA, nints, 2 * n1 + 1)
    ints1[, 1] <- level
    n0 <- 4
    for (i in n0:(n0 + n1 - 1)) {
        j <- c(2 * i - 6, 2 * i - 5)
        nc <- ncol(ci.out[[i]])
        nc <- c(nc - 1, nc)
        if (is.null(hinv)) 
            ints1[, j] <- ci.out[[i]][, nc]
        else ints1[, j] <- hinv(ci.out[[i]][, nc])
    }
    n0 <- 4 + n1
    ints1 <- format(round(ints1, digs))
    ints1[, 1] <- paste("\n<BR>", level, "%  ", sep = "")
    ints1[, 2 * (1:n1)] <- paste("(", ints1[, 2 * (1:n1)], ",", 
        sep = "")
    ints1[, 2 * (1:n1) + 1] <- paste(ints1[, 2 * (1:n1) + 1], 
        ")  ")
    if (n2 > 0) {
        ints2 <- matrix(NA, nints, 2 * n2 + 1)
        ints2[, 1] <- level
        j <- c(2, 3)
        for (i in n0:(n0 + n2 - 1)) {
            if (is.null(hinv)) 
                ints2[, j] <- ci.out[[i]][, c(4, 5)]
            else ints2[, j] <- hinv(ci.out[[i]][, c(4, 5)])
            j <- j + 2
        }
        ints2 <- format(round(ints2, digs))
        ints2[, 1] <- paste("\n<BR>", level, "%  ", sep = "")
        ints2[, 2 * (1:n2)] <- paste("(", ints2[, 2 * (1:n2)], 
            ",", sep = "")
        ints2[, 2 * (1:n2) + 1] <- paste(ints2[, 2 * (1:n2) + 
            1], ")  ")
    }
    R <- ci.out$R
    HTML(as.title("BOOTSTRAP CONFIDENCE INTERVAL CALCULATIONS"),File=File)
    HTML(paste("<P>Based on", R, "bootstrap replicates\n\n</P>"),File=File)
    HTMLli(paste("CALL : ",paste(deparse(cl),collapse=" ")),File=File)
    HTML("\n<P>Intervals : </P>",File=File)
    HTML(paste("\n<P>Level", intlabs[1:n1],"</P>"),File=File)
    HTML(t(ints1),File=File)
    if (n2 > 0) {
        HTML(paste("\n<P>\nLevel", intlabs[(n1 + 1):(n1 + n2)],"</P>"),File=File)
        HTML(t(ints2),File=File)
    }
    if (!is.null(cl$h)) {
        if (is.null(cl$hinv) && is.null(hinv)) 
            HTML("\n<P>Calculations and Intervals on Transformed Scale\n</P>",File=File)
        else HTML("\n<P>Calculations on Transformed Scale;  Intervals on Original Scale\n</P>",File=File)
    }
    else if (is.null(cl$hinv) && is.null(hinv)) 
        HTML("\n<P>Calculations and Intervals on Original Scale\n</P>",File=File)
    else HTML("\n<P>Calculations on Original Scale but Intervals Transformed\n</P>",File=File)
    if (!is.null(basrg)) {
        if ((basrg[1] <= 1) || (basrg[2] >= R)) 
            HTML("\n<P>Warning : Basic Intervals used Extreme Quantiles\n</P>",File=File)
        if ((basrg[1] <= 10) || (basrg[2] >= R - 9)) 
            HTML("\n<P>Some basic intervals may be unstable\n</P>",File=File)
    }
    if (!is.null(strg)) {
        if ((strg[1] <= 1) || (strg[2] >= R)) 
            HTML("\n<P>Warning : Studentized Intervals used Extreme Quantiles\n</P>",File=File)
        if ((strg[1] <= 10) || (strg[2] >= R - 9)) 
            HTML("\n<P>Some studentized intervals may be unstable\n</P>",File=File)
    }
    if (!is.null(perg)) {
        if ((perg[1] <= 1) || (perg[2] >= R)) 
            HTML("\n<P>Warning : Percentile Intervals used Extreme Quantiles\n</P>",File=File)
        if ((perg[1] <= 10) || (perg[2] >= R - 9)) 
            HTML("\n<P>Some percentile intervals may be unstable\n</P>",File=File)
    }
    if (!is.null(bcarg)) {
        if ((bcarg[1] <= 1) || (bcarg[2] >= R)) 
            HTML("\n<P>Warning : BCa Intervals used Extreme Quantiles\n</P>",File=File)
        if ((bcarg[1] <= 10) || (bcarg[2] >= R - 9)) 
            HTML("\n<P>Some BCa intervals may be unstable\n</P>",File=File)
    }
    invisible(ci.out)
}


#----------------------------------------------------------------------------------------------------#

### PACKAGE MVA

#----------------------------------------------------------------------------------------------------#

"HTML.dist" <- function (x, diag = NULL, upper = NULL, File=.HTML.File,...) 
{
    if (is.null(diag)) 
        diag <- if (is.null(a <- attr(x, "Diag"))) 
            FALSE
        else a
    if (is.null(upper)) 
        upper <- if (is.null(a <- attr(x, "Upper"))) 
            FALSE
        else a
    size <- attr(x, "Size")
    df <- as.matrix.dist(x)
    if (!upper) 
        df[row(df) < col(df)] <- NA
    if (!diag) 
        df[row(df) == col(df)] <- NA
    HTML(if (diag || upper) 
        df
    else df[-1, -size], File=File, ...)
    invisible(x)
}

#----------------------------------------------------------------------------------------------------#

"HTML.factanal" <- function (x, digits = 3, File=.HTML.File,...) 
{
    HTMLli(paste("\nCall:\n", deparse(x$call), "\n<BR>\n", sep = ""),File=File)
    HTMLli("Uniquenesses:\n<BR>",File=File)
    HTML(round(x$uniquenesses, digits),File=File,...)
    HTML(x$loadings, digits = digits,File=File, ...)
    p <- nrow(x$loadings)
    factors <- x$factors
    if (!is.na(x$n.obs) && x$dof > 0) {
        dof <- x$dof
        stat <- (x$n.obs - 1 - (2 * p + 5)/6 - (2 * factors)/3) * 
            x$criteria["objective"]
        HTMLli(paste("\n<P>Test of the hypothesis that", factors, if (factors == 
            1) 
            "factor is"
        else "factors are", "sufficient.\n</P>"),File=File)
        HTML(paste("<P>The chi square statistic is <B>", round(stat, 2), " </B> on <B>", 
            dof, if (dof == 1) 
                " </B>degree"
            else "</B>degrees", "of freedom.\n<BR>The p-value is <B>", signif(pchisq(stat, 
                dof, lower.tail = FALSE), 3), "</B>\n</P>"),File=File)
    }
    else {
        HTML(paste("\n<P>The degrees of freedom for the model is <B>", 
            x$dof, " </B>and the fit was <B>", round(x$criteria["objective"], 
                4), "</B>\n</P>"),File=File)
    }
    invisible(x)
}


#----------------------------------------------------------------------------------------------------#

"HTML.loadings" <- function (x, digits = 3, cutoff = 0.1, sort = FALSE, File=.HTML.File,...) 
{
    Lambda <- unclass(x)
    p <- nrow(Lambda)
    factors <- ncol(Lambda)
    if (sort) {
        mx <- max.col(abs(Lambda))
        ind <- cbind(1:p, mx)
        mx[abs(Lambda[ind]) < 0.5] <- factors + 1
        Lambda <- Lambda[order(mx, 1:p), ]
    }
    HTMLli("Loadings:\n<BR>",File=File)
    fx <- format(round(Lambda, digits))
    names(fx) <- NULL
    nc <- nchar(fx[1])
    fx[abs(Lambda) < cutoff] <- paste(rep("&nbsp;", nc), collapse = "")
    HTML(fx, File=File, ...)
    vx <- colSums(x^2)
    varex <- rbind("SS loadings" = vx)
    if (is.null(attr(x, "covariance"))) {
        varex <- rbind(varex, "Proportion Var" = vx/p)
        if (factors > 1) 
            varex <- rbind(varex, "Cumulative Var" = cumsum(vx/p))
    }
    HTMLbr(File=File)
    HTML(round(varex, digits),File=File)
    invisible(x)
}


#----------------------------------------------------------------------------------------------------#

"HTML.hclust" <- function (x, File=.HTML.File,...) 
{
    if (!is.null(x$call)) 
        HTMLli(paste("Call : ", deparse(x$call), "\n<UL>\n", sep = ""),File=File)
    if (!is.null(x$method)) 
        HTMLli(paste("Cluster method :", x$method, "\n"),File=File)
    if (!is.null(x$dist.method)) 
        HTMLli(paste("Distance : ", x$dist.method, "\n"),File=File)
    HTMLli(paste("Number of objects: ", length(x$height) + 1, "\n"),File=File)
	HTML("</UL><BR>&nbsp;<BR>",File=File)
	invisible(x)
}


#----------------------------------------------------------------------------------------------------#

"HTML.prcomp" <- function (x, print.x = FALSE, File=.HTML.File,...) 
{
    HTML("<P>Standard deviations:\n</P>")
    HTML(x$sdev, File,File,...)
    HTML("\n<P>Rotation:\n</P>")
    HTML(x$rotation, File=File,...)
    if (print.x && length(x$x)) {
        HTML("\n<P>Rotated variables:\n</P>")
        HTML(x$x, File=File,...)
    }
    invisible(x)
}



#----------------------------------------------------------------------------------------------------#

"HTML.princomp" <- function (x, File=.HTML.File,...) 
{
    HTMLli(paste("Call: ",deparse(x$call)),File=File)
    HTML("\n<P>Standard deviations:\n</P>",File=File)
    HTML(x$sdev, File=File,...)
    HTML(paste("\n<P><B>", length(x$scale), " </B>variables and <B>", x$n.obs, " </B>observations.\n</P>"),File=File)
    invisible(x)
}


#----------------------------------------------------------------------------------------------------#

"HTML.summary.prcomp" <- function (x, digits = min(3, getOption("digits") - 3), File=.HTML.File,...) 
{
    HTML("<P>Importance of components:\n</P>",File=File)
    HTML(x$importance, digits = digits,File=File)
    invisible(x)
}


#----------------------------------------------------------------------------------------------------#

"HTML.summary.princomp" <- function (x, digits = 3, loadings = x$print.loadings, cutoff = x$cutoff, File=.HTML.File,  ...) 
{
    vars <- x$sdev^2
    vars <- vars/sum(vars)
    HTML("<P>Importance of components:\n</P>",File=File)
    HTML(rbind("Standard deviation" = x$sdev, "Proportion of Variance" = vars, 
        "Cumulative Proportion" = cumsum(vars)),File=File)
    if (loadings) {
        HTMLli("Loadings:\n")
        cx <- format(round(x$loadings, digits = digits))
        cx[abs(x$loadings) < cutoff] <- substring("&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;", 
            1, nchar(cx[1, 1]))
        HTML(cx, quote = FALSE, File=File,...)
    }
    invisible(x)
}

#----------------------------------------------------------------------------------------------------#
### PACKAGE EDA
#----------------------------------------------------------------------------------------------------#

"HTML.medpolish" <- function (x, digits = getOption("digits"), File=.HTML.File,...) 
{
    HTML(paste("\n<P><B>Median Polish Results (Dataset: \"", x$name, "\")\n</B></P>", 
        sep = ""),File=File)
    HTML(paste("\n<P>Overall:", x$overall, "\n</P>\n<P>Row Effects:\n</P>"),File=File)
    HTML(x$row, digits = digits, File=File,...)
    HTML("\n<P>Column Effects:\n</P>")
    HTML(x$col, digits = digits, File=File,...)
    HTML("\n<P>Residuals:\n</P>",File=File)
    HTML(x$residuals, digits = max(2, digits - 2), File=File,...)
    HTMLbr(File=File)
    invisible(x)
}

#----------------------------------------------------------------------------------------------------#

"HTML.tukeyline" <- function (x, digits = max(3, getOption("digits") - 3), File=.HTML.File,...) 
{
    HTMLli(paste("Call:\n", deparse(x$call), "\n<BR>\n", sep = ""),File=File)
    HTML("<P>Coefficients:\n</P>")
    print.default(format(coef(x), digits = digits),File=File)
    HTMLbr(File=File)
    invisible(x)
}

#----------------------------------------------------------------------------------------------------#

"HTML.tukeysmooth" <- function (x, File=.HTML.File,...) 
{
    HTML(paste("<P><B>",attr(x, "kind"), " Tukey smoother resulting from ", deparse(attr(x, 
        "call")), "\n",   if (twiced <- attr(x, "twiced")) " <-<-twiced<-<- ", 
        if (!is.null(it <- attr(x, "iter"))) paste(" used", it, "iterations\n"),
        if (!is.null(ch <- attr(x, "changed"))) paste(if (!ch) " NOT ", " changed\n</B></P>")),File=File)
    if (length(class(x)) > 1) 
        NextMethod()
    else {
        y <- x
        attributes(y) <- NULL
        HTML(y,File=File, ...)
        invisible(x)
    }
}


#----------------------------------------------------------------------------------------------------#
### PACKAGE EDA
#----------------------------------------------------------------------------------------------------#


"HTML.grob" <- function (x, File=.HTML.File,...) 
{
    cl <- class(get.value.grob(x))
    HTML(paste(cl[1:(length(cl) - 1)], collapse = "&nbsp;"),File=File)
    invisible(x)
}

#----------------------------------------------------------------------------------------------------#

"HTML.unit" <- function (x, File=.HTML.File,...) 
{
    HTML(as.character(x), File=File)
    invisible(x)
}

#----------------------------------------------------------------------------------------------------#

"HTML.viewport" <- function (x, File=.HTML.File,...) 
{
    HTML(class(x),File=File)
    invisible(x)
}


#----------------------------------------------------------------------------------------------------#
### PACKAGE LATTICE
#----------------------------------------------------------------------------------------------------#

"HTML.shingle" <- function (x, File=.HTML.File,...) 
{
    HTML("\n<P>Data:\n</P>",File=File)
    HTML(as.numeric(x),File=File)
    l <- levels(x)
    n <- nlevels(x)
    if (n < 1) 
        HTML("\n<P>no intervals\n</P>",File=File)
    else {
        int <- data.frame(min = numeric(n), max = numeric(n), 
            count = numeric(n))
        for (i in 1:n) {
            int$min[i] <- l[[i]][1]
            int$max[i] <- l[[i]][2]
            int$count[i] <- length(x[x >= l[[i]][1] & x <= l[[i]][2]])
        }
        HTML("\n<P>Intervals:\n</P>")
        HTML(int,File=File)
        olap <- numeric(n - 1)
        if (n > 2) 
            for (i in 1:(n - 1)) olap[i] <- length(x[x >= l[[i]][1] & 
                x <= l[[i]][2] & x >= l[[i + 1]][1] & x <= l[[i + 
                1]][2]])
        HTML("\n<P>Overlap between adjacent intervals:\n</P>",File=File)
        HTML(olap,File=File)
    }
    invisible(x)
}


#----------------------------------------------------------------------------------------------------#

 "HTML.shingleLevel" <- function (x, File=.HTML.File,...) 
{
    HTML(do.call("rbind", x),File=File)
    invisible(x)
}
	


#----------------------------------------------------------------------------------------------------#
### PACKAGE MASS
#----------------------------------------------------------------------------------------------------#

"HTML.abbrev" <- function (x, File=.HTML.File,...) 
{
    if (is.list(x)) 
        x <- unlist(x)
    NextMethod("HTML")
}


#----------------------------------------------------------------------------------------------------#

"HTML.Anova" <- function (x, File=.HTML.File,...) 
{
    heading <- attr(x, "heading")
    if (!is.null(heading)) 
        HTML(paste("<P>",heading,"</P>", sep = " ",collapse="<BR>"),File=File)
    attr(x, "heading") <- NULL
    HTML.data.frame(x,File=File)
}

#----------------------------------------------------------------------------------------------------#

"HTML.anova.loglm" <- function (x, File=.HTML.File,...) 
{
#    rjustify <- function(str) {
#        m <- max(n <- nchar(str))
# blanks <- format(c("", str[n == m][1]))[1]
#        paste(substring(blanks, 0, m - n), str, sep = "")
#    }
    y <- x
    y[, 5] <- round(y[, 5], 5)
    R <- array("", dim(x), dimnames(x))
    for (j in 1:5) {
        colj <- c(colnames(x)[j], format(y[, j]))
        R[, j] <- colj[-1]
        colnames(R)[j] <- colj[1]
    }
    R[1, 3:5] <- ""
#    pform <- function(form) if (length(form) == 2) 
#        form
#    else form[c(2, 1, 3)]
    forms <- attr(x, "formulae")
    HTML("<P><B>LR tests for hierarchical log-linear models</B>\n</P>\n",File=File)
    for (i in seq(along = forms)) 
    HTML(paste(paste("<P>Model ", i, ":<BR>", sep = ""), paste(deparse(forms[[i]]),collapse=""), "</P>"),File=File)
    HTMLbr(File=File)
    HTML(R,File=File)
    invisible(x)
}


#----------------------------------------------------------------------------------------------------#

"HTML.correspondence" <- function (x, File=.HTML.File,...) 
{
    HTML(paste("<P>First canonical correlation(s):", format(x$cor, ...), "\n</P>"),File=File)
    rcn <- names(dimnames(x$Freq))
    HTML(paste("\n<P>", rcn[1], "scores:\n</P>"),File=File)
    HTML(x$rscore,File=File)
    HTML(paste("\n<P>", rcn[2], "scores:\n</P>"),File=File)
    HTML(x$cscore,File=File)
    invisible(x)
}


#----------------------------------------------------------------------------------------------------#

"HTML.fitdistr" <- function (x, digits = getOption("digits"), File=.HTML.File,...) 
{
    ans <- format(rbind(x$estimate, x$sd), digits = digits)
    ans[1, ] <- sapply(ans[1, ], function(x) paste("", x))
    ans[2, ] <- sapply(ans[2, ], function(x) paste("(", x, ")", 
        sep = ""))
    dn <- dimnames(ans)
    dn[[1]] <- rep("", 2)
    dn[[2]] <- paste(substring("  ", 1, (nchar(ans[2, ]) - 
        nchar(dn[[2]]))%/%2), dn[[2]])
    dn[[2]] <- paste(dn[[2]], substring("  ", 1, (nchar(ans[2, 
        ]) - nchar(dn[[2]]))%/%2))
    dimnames(ans) <- dn
    HTML(ans, File=File)
    invisible(x)
}


#----------------------------------------------------------------------------------------------------#

"HTML.fractions" <- function (x, File=.HTML.File,...) 
{
    y <- attr(x, "fracs")
    att <- attributes(x)
    att$fracs <- att$class <- NULL
    x <- do.call("structure", c(list(y), att))
    NextMethod("HTML", File=File)
}


#----------------------------------------------------------------------------------------------------#

"HTML.gamma.shape" <- function (x,File=.HTML.File, ...) 
{
    y <- x
    x <- array(unlist(x), dim = 2:1, dimnames = list(c("Alpha ", "SE "), ""))
    NextMethod("HTML",File=File)
    invisible(y)
}

#----------------------------------------------------------------------------------------------------#

"HTML.glm.dose" <- function (x, File=.HTML.File,...) 
{
    M <- cbind(x, attr(x, "SE"))
    dimnames(M) <- list(names(x), c("Dose", "SE"))
    x <- M
    NextMethod("HTML",File=File)
}

#----------------------------------------------------------------------------------------------------#

"HTML.lda" <- function (x, File=.HTML.File,...) 
{
    if (!is.null(cl <- x$call)) {
        names(cl)[2] <- ""
        HTMLli(paste("Call: ",deparse(cl)),File=File)
    }
    HTML("\n<P>Prior probabilities of groups:\n</P>",File=File)
    HTML(x$prior, File=File,...)
    HTML("\n<P>Group means:\n</P>",File=File)
    HTML(x$means, File=File,...)
    HTML("\n<P>Coefficients of linear discriminants:\n</P>",File=File)
    HTML(x$scaling, File=File,...)
    svd <- x$svd
    names(svd) <- dimnames(x$scaling)[[2]]
    if (length(svd) > 1) {
        HTML("\n<P>Proportion of trace:\n</P>",File=File)
        HTML(round(svd^2/sum(svd^2), 4), File=File,...)
    }
    invisible(x)
}

#----------------------------------------------------------------------------------------------------#

"HTML.loglm" <- function (x,File=.HTML.File, ...) 
{
    HTMLli(paste("Call: ",deparse(x$call)),File=File)
    ts.array <- rbind(c(x$lrt, x$df, if (x$df > 0) 1 - pchisq(x$lrt, 
        x$df) else 1), c(x$pearson, x$df, if (x$df > 0) 1 - pchisq(x$pearson, 
        x$df) else 1))
    dimnames(ts.array) <- list(c("Likelihood Ratio", "Pearson"), 
        c("X^2", "df", "P(> X^2)"))
    HTML("\n<P>Statistics:\n</P>",File=File)
    HTML(ts.array,File=File)
    invisible(x)
}

#----------------------------------------------------------------------------------------------------#

"HTML.mca" <- function (x, File=.HTML.File,...) 
{
    if (!is.null(cl <- x$call)) HTMLli(paste("Call: ",deparse(cl)),File=File)
    
    HTML(paste("\n<P>Multiple correspondence analysis of <B>", nrow(x$rs), 
        " </B>cases of <B> ", x$p, " </B>factors\n</P>"),File=File)
    
    
    p <- 100 * cumsum(x$d)/(x$p - 1)
    HTML(paste("\n<P>Correlations ",paste(round(x$d, 3),collapse=" "),"  cumulative % explained ", paste(round(p, 2),collapse=" "),"</P>" ),File=File)
    
    invisible(x)
}


#----------------------------------------------------------------------------------------------------#

"HTML.polr" <- function (x, File=.HTML.File,...) 
{
    if (!is.null(cl <- x$call)) HTMLli(paste("Call: ",deparse(cl)),File=File)
    if (length(coef(x))) {
        HTML("\n<P>Coefficients:\n</P>",File=File)
        HTML(coef(x), File=File,...)
    }
    else {
        HTML("\n<P>No coefficients\n</P>",File=File)
    }
    HTML("\n<P>Intercepts:\n</P>",File=File)
    HTML(x$zeta, File=File,...)
    HTML(paste("\n<P>Residual Deviance: <B>", format(x$deviance, nsmall = 2), "</B>\n</P>"),File=File)
    HTML(paste("<P>AIC:<B>", format(x$deviance + 2 * x$edf, nsmall = 2), "</B>\n</P>"),File=File)
    invisible(x)
}

#----------------------------------------------------------------------------------------------------#

"HTML.qda" <- function (x, File=.HTML.File,...) 
{
    if (!is.null(cl <- x$call)) {
        names(cl)[2] <- ""
        HTMLli(paste("Call: ",deparse(cl)),File=File)
    }
    HTML("\n<P>Prior probabilities of groups:\n</P>",File=File)
    HTML(x$prior, File=File,...)
    HTML("\n<P>Group means:\n</P>",File=File)
    HTML(x$means, File=File,...)
    invisible(x)
}

#----------------------------------------------------------------------------------------------------#

"HTML.ridgelm" <- function (x, File=.HTML.File,...) 
{
    scaledcoef <- t(as.matrix(x$coef/x$scales))
    if (x$Inter) {
        inter <- x$ym - scaledcoef %*% x$xm
        scaledcoef <- cbind(Intercept = inter, scaledcoef)
    }
    HTML(drop(scaledcoef), File=File,...)
    invisible(x)
}

#----------------------------------------------------------------------------------------------------#

"HTML.rlm" <- function (x,File=.HTML.File, ...) 
{
    if (!is.null(cl <- x$call)) {
        HTMLli(paste("Call: ",paste(deparse(cl),collapse=" ")),File=File)
    }
    if (x$converged) 
        HTML(paste("<P>Converged in <B>", length(x$conv), "</B> iterations\n</P>"),File=File)
    else HTML(paste("<P>Ran <B>", length(x$conv), " </B>iterations without convergence\n</P>"),File=File)
    coef <- x$coef
    HTML("\n<P>Coefficients:\n</P>",File=File)
    HTML(coef, File=File,...)
    nobs <- length(x$resid)
    rdf <- nobs - length(coef)
    HTML(paste("\n<P>Degrees of freedom: <B>", nobs, " </B>total; <B>", rdf, " </B>residual\n</P>"),File=File)
    HTML(paste("<P>Scale estimate:<B>", paste(format(signif(x$s, 3)),collapse=" "), "</B>\n</P>"),File=File)
    invisible(x)
}

#----------------------------------------------------------------------------------------------------#

"HTML.rms.curv" <- function (x, File=.HTML.File,...) 
{
    HTML(paste("<P><LI>Parameter effects: c^theta x sqrt(FALSE) =<B>", round(x$pe, 
        4), "</B>\n<BR><LI>", "Intrinsic: c^iota  x sqrt(FALSE) =<B>", round(x$ic, 
        4), "\n</B></P>"),File=File, ...)
    invisible(x)
}

#----------------------------------------------------------------------------------------------------#

"HTML.summary.loglm" <- function (x, File=.HTML.File,...) 
{
    HTML("<P>Formula:\n</P>",File=File)
    HTML(formula(x),File=File)
    HTML("\n<P>Statistics:\n</P>",File=File)
    HTML(x$tests,File=File)
    if (!is.null(x$oe)) {
        HTML("\n<P>Observed (Expected):\n</P>",File=File)
        HTML(x$oe, File=File	)
    }
    invisible(x)
}


#----------------------------------------------------------------------------------------------------#

"HTML.summary.negbin" <- function (x, File=.HTML.File,...) 
{
	NextMethod(x,File=File)
	dp <- 2 - floor(log10(x$SE.theta))
    	HTML(paste("<P><LI>Theta:<B> ", round(x$theta, dp), "</B>\n<LI>Std. Err.:<B> ", round(x$SE.theta,  dp), "</B>\n</P>"),File=File)
    	if (!is.null(x$th.warn)) 
    	HTML(paste("<P>Warning while fitting theta:", x$th.warn, "\n</P>"),File=File)
	HTML(paste("\n<P><LI> 2 x log-likelihood: ", format(round(x$twologlik, 3), nsmall = dp), "\n</P>"),File=File)
	invisible(x)
}


#----------------------------------------------------------------------------------------------------#

"HTML.summary.polr" <- function (x, digits = x$digits, File=.HTML.File,...) 
{
    if (!is.null(cl <- x$call)) {
        HTMLli(paste("Call: ",deparse(cl)),File=File)
    }
    coef <- format(round(x$coef, digits = digits))
    pc <- x$pc
    if (pc > 0) {
        HTML("\n<P>Coefficients:\n</P>",File=File)
        HTML(x$coef[seq(len = pc), ], File=File, ...)
    }
    else {
        HTML("\n<P>No coefficients\n</P>",File=File)
    }
    HTML("\n<P>Intercepts:\n</P>",File=File)
    HTML(coef[(pc + 1):nrow(coef), ], File=File, ...)
    HTML(paste("\n<P>Residual Deviance:<B>", format(x$deviance, nsmall = 2), "</B>\n</P>"),File=File)
    HTML(paste("\n<P>AIC:<B>", format(x$deviance + 2 * x$edf, nsmall = 2), "</B>\n</P>"),File=File)
    if (!is.null(correl <- x$correlation)) {
        cat("\n<P>Correlation of Coefficients:\n</P>",File=File)
        ll <- lower.tri(correl)
        correl[ll] <- format(round(correl[ll], digits))
        correl[!ll] <- ""
        HTML(correl[-1, -ncol(correl)], File=File, ...)
    }
    invisible(x)
}

#----------------------------------------------------------------------------------------------------#

"HTML.summary.rlm" <- function (x, digits = max(3, .Options$digits - 3), File=.HTML.File,...) 
{
    HTMLli(paste("\nCall: ",deparse(x$call)),File=File)
    resid <- x$residuals
    df <- x$df
    rdf <- df[2]
    if (rdf > 5) {
        HTML("<P>Residuals:\n</P>",File=File)
        if (length(dim(resid)) == 2) {
            rq <- apply(t(resid), 1, quantile)
            dimnames(rq) <- list(c("Min", "1Q", "Median", "3Q", 
                "Max"), colnames(resid))
        }
        else {
            rq <- quantile(resid)
            names(rq) <- c("Min", "1Q", "Median", "3Q", "Max")
        }
        HTML(rq, File=File)
    }
    else if (rdf > 0) {
        HTML("<P>Residuals:\n</P>",File=File)
        HTML(resid,File=File)
    }
    if (nsingular <- df[3] - df[1]) 
        HTML(paste("\n<P>Coefficients: (", nsingular, " not defined because of singularities)\n</P>",sep = ""),File=File)
    else HTML("\n<P>Coefficients:\n</P>",File=File)
    HTML(format(round(x$coef, digits = digits)), File=File)
    HTML(paste("\n<P>Residual standard error:<B>", format(signif(x$sigma, 
        digits)), " </B>on <B> ", rdf, " </B>degrees of freedom\n</P>"),File=File)
    if (!is.null(correl <- x$correlation)) {
        p <- dim(correl)[2]
        if (p > 1) {
            HTML("\n<P>Correlation of Coefficients:\n</P>",File=File)
            ll <- lower.tri(correl)
            correl[ll] <- format(round(correl[ll], digits))
            correl[!ll] <- ""
            HTML(correl[-1, -p, drop = FALSE], File=File)
        }
    }
    invisible(x)
}



#----------------------------------------------------------------------------------------------------#
### PACKAGE NNET
#----------------------------------------------------------------------------------------------------#

"HTML.multinom" <- function (x, File=.HTML.File,...) 
{
    if (!is.null(cl <- x$call)) {
        HTMLli(paste("Call: ",paste(deparse(cl),collapse="")),File=File)
    }
    HTML("\n<P>Coefficients:\n</P>",File=File)
    HTML(coef(x), File=File)
    HTML(paste("\n<P>Residual Deviance: <B>", format(x$deviance), "</B>\n</P>"),File=File)
    HTML(paste("<P>AIC:<B>", format(x$AIC), "</B>\n</P>"),File=File)
    invisible(x)
}


#----------------------------------------------------------------------------------------------------#

"HTML.nnet" <- function (x, File=.HTML.File,...) 
{
    if (!inherits(x, "nnet")) 
        stop("Not legitimate a neural net fit")
    HTML(paste("<P><B>a ", x$n[1], "-", x$n[2], "-", x$n[3], " network with ", length(x$wts), " weights.</B></P>", sep = ""),File=File)
    
    if (length(x$coefnames)) 
        HTML(paste("<P>inputs:", x$coefnames, "\noutput(s):", deparse(formula(x)[[2]]), "\n</P>"),File=File)
    HTML("<P>options were -</P>",File=File)
    tconn <- diff(x$nconn)
    if (tconn[length(tconn)] > x$n[2] + 1) 
        HTMLli(" skip-layer connections ",File=File)
    if (x$nunits > x$nsunits && !x$softmax) 
        HTMLli(" linear output units ",File=File)
    if (x$entropy) 
        HTMLli(" entropy fitting ",File=File)
    if (x$softmax) 
        HTMLli(" softmax modelling ",File=File)
    if (x$decay[1] > 0) 
        HTMLli(paste(" decay=", x$decay[1], sep = ""),File=File)
    HTMLbr(File=File)
    invisible(x)
}


#----------------------------------------------------------------------------------------------------#

"HTML.summary.multinom" <- function (x, digits = x$digits, File=.HTML.File,...) 
{
    if (!is.null(cl <- x$call)) {
        HTMLli(paste("Call:",paste(deparse(cl),collapse=" ")),File=File)
    }
    HTML("\n<P>Coefficients:\n</P>",File=File)
    if (x$is.binomial) {
        HTML(cbind(Values = x$coefficients, "Std. Err." = x$standard.errors, 
            "Value/SE" = x$Wald.ratios), File=File)
    }
    else {
        HTML(x$coefficients, File=File)
        HTML("\n<P>Std. Errors:\n</P>",File=File)
        HTML(x$standard.errors, File=File)
        if (!is.null(x$Wald.ratios)) {
            HTML("\n<O>Value/SE (Wald statistics):\n</P>",File=File)
            HTML(x$coefficients/x$standard.errors, File=File)
        }
    }
    HTML(paste("\n<P>Residual Deviance:<B>", format(x$deviance), "</B>\n</P>"),File=File)
    HTML(paste("\n<P>AIC:<B>", format(x$AIC), "</B>\n</P>"),File=File)
    if (!is.null(correl <- x$correlation)) {
        p <- dim(correl)[2]
        if (p > 1) {
            HTML("\n</P>Correlation of Coefficients:\n</P>",File=File)
            ll <- lower.tri(correl)
            correl[ll] <- format(round(correl[ll], digits))
            correl[!ll] <- ""
            HTML(correl[-1, -p], File= File)
        }
    }
    invisible(x)
}

#----------------------------------------------------------------------------------------------------#

"HTML.summary.nnet" <- function (x, File=.HTML.File,...) 
{
    
     HTML(paste("<P><B>a ", x$n[1], "-", x$n[2], "-", x$n[3], " network with ", length(x$wts), " weights.</B></P>", sep = ""),File=File)
        
        HTML("<P>options were -</P>",File=File)
        tconn <- diff(x$nconn)
        if (tconn[length(tconn)] > x$n[2] + 1) 
            HTMLli(" skip-layer connections ",File=File)
        if (x$nunits > x$nsunits && !x$softmax) 
            HTMLli(" linear output units ",File=File)
        if (x$entropy) 
            HTMLli(" entropy fitting ",File=File)
        if (x$softmax) 
            HTMLli(" softmax modelling ",File=File)
        if (x$decay[1] > 0) 
        HTMLli(paste(" decay=", x$decay[1], sep = ""),File=File)
    wts <- format(round(coef.nnet(x), 2))
    lapply(split(wts, rep(1:x$nunits, tconn)), function(x) HTML(x,File=File))
    invisible(x)
}

#----------------------------------------------------------------------------------------------------#
### PACKAGE CLUSTER
#----------------------------------------------------------------------------------------------------#


"HTML.agnes" <- function (x, File=.HTML.File,...) 
{
    HTML("<P>Merge:\n</P>",File=File)
    HTML(x$merge, File=File,...)
    HTML("<P>Order of objects:\n</P>",File=File)
    HTML(if (length(x$order.lab) != 0) 
        x$order.lab
    else x$order, File=File, ...)
    HTML("<P>Height:\n</P>",File=File)
    HTML(x$height, File=File,...)
    HTML("<P>Agglomerative coefficient:\n</P>",File=File)
    HTML(x$ac, File=File,...)
    HTML("\n<P>Available components:\n</P>",File=File)
    HTML(names(x), File=File,...)
    invisible(x)
}


#----------------------------------------------------------------------------------------------------#

"HTML.clara" <- function (x, File=.HTML.File,...) 
{
    HTML("<P>Best sample:\n</P>",File=File)
    HTML(x$sample, File=File, ...)
    HTML("<P>Medoids:\n</P>",File=File)
    HTML(x$medoids, File=File,...)
    HTML("<P>Clustering vector:\n</P>")
    HTML(x$clustering, File=File,...)
    HTML("<P>Objective function:\n</P>",File=File)
    HTML(x$objective, File=File,...)
    HTML("\n<P>Available components:\n</P>",File=File)
    HTML(names(x),File=File, ...)
    invisible(x)
}

#----------------------------------------------------------------------------------------------------#

"HTML.diana" <- function (x, File=.HTML.File,...) 
{
    HTML("<P>Merge:\n</P>",File=File)
    HTML(x$merge, File=File,...)
    HTML("<P>Order of objects:\n</P>",File=File)
    HTML(if (length(x$order.lab) != 0)  x$order.lab    else x$order, File= File, ...)
    HTML("<P>Height:\n</P>",File=File)
    HTML(x$height, File=File,...)
    HTML("<P>Divisive coefficient:\n</P>",File=File)
    HTML(x$dc,File=File, ...)
    HTML("\n<P>Available components:\n</P>",File=File)
    HTML(names(x),File=File, ...)
    invisible(x)
}

#----------------------------------------------------------------------------------------------------#

"HTML.dissimilarity" <- function (x, File=.HTML.File,...) 
{
    HTML("<P>Dissimilarities :\n</P>",File=File)
    HTML(as.vector(x),File=File, ...)
    if (!is.null(attr(x, "na.message"))) 
        HTML(paste("<P>Warning : ", attr(x, "NA.message"), "\n</P>"),File=File)
    HTML(paste("<P>Metric : ", attr(x, "Metric"), "\n</P>"),File=File)
    HTML(paste("<P>Number of objects : ", attr(x, "Size"), "\n</P>"),File=File)
    invisible(x)
}

#----------------------------------------------------------------------------------------------------#

"HTML.ellipsoid" <- function (x, digits = max(1, getOption("digits") - 2), File=.HTML.File,...) 
{
    d <- length(x$loc)
    HTML(paste("<P>`ellipsoid' in <B> ", d, " </B>dimensions:<BR> center = (<B>", paste(format(x$loc, 
        digits = digits),collapse=" "), "</B>); squared ave.radius d^2 = <B>", format(x$d2, 
        digits = digits), " </B>\n<BR> and shape matrix =\n</P>"),File=File)
    HTML(x$cov, File=File, ...)
    HTML(paste("<P>&nbsp;  hence,", if (d == 2) 
        " area "
    else " volume ", " = <B>", format(volume(x), digits = digits), 
        "\n</B></P>"),File=File)
    invisible(x)
}


#----------------------------------------------------------------------------------------------------#

"HTML.fanny" <- function (x,File=.HTML.File, ...) 
{
    HTML(x$objective, File=File,...)
    HTML("<P>Membership coefficients:\n</P>", File=File)
    HTML(x$membership, File=File, ...)
    HTML("<P>Coefficients:\n</P>", File=File)
    HTML(x$coeff, File=File, ...)
    HTML("<P>Closest hard clustering:\n</P>", File=File)
    HTML(x$clustering, File=File, ...)
    HTML("\n<P>Available components:\n</P>", File=File)
    HTML(names(x), File=File, ...)
    invisible(x)
}


#----------------------------------------------------------------------------------------------------#

"HTML.mona" <- function (x, File=.HTML.File,...) 
{
    HTML("<P>Revised data:\n</P>",File=File)
    HTML(x$data,File=File,  ...)
    HTML("<P>Order of objects:\n</P>",File=File)
    HTML(if (length(x$order.lab) != 0)  x$order.lab else x$order,File=File, ...)
    HTML("<P>Variable used:\n</P>",File=File)
    HTML(x$variable, File=File, ...)
    HTML("<P>Separation step:\n</P>",File=File)
    HTML(x$step,File=File, ...)
    HTML("\n<P>Available components:\n</P>",File=File)
    HTML(names(x),File=File, ...)
    invisible(x)
}

#----------------------------------------------------------------------------------------------------#

"HTML.pam" <- function (x, File=.HTML.File,...) 
{
    HTML("<P>Medoids:\n</P>",File=File)
    HTML(x$medoids,File=File, ...)
    HTML("<P>Clustering vector:\n</P>",File=File)
    HTML(x$clustering,File=File, ...)
    HTML("<P>Objective function:\n</P>",File=File)
    HTML(x$objective,File=File, ...)
    HTML("\n<P>Available components:\n</P>",File=File)
    HTML(names(x),File=File, ...)
    invisible(x)
}

#----------------------------------------------------------------------------------------------------#

"HTML.summary.agnes" <- function(x,File=.HTML.File,...) 
{
    HTML("<P>Merge:\n</P>",File=File)
    HTML(x$merge, File=File, ...)
    HTML("<P>Order of objects:\n</P>",File=File)
    HTML(if (length(x$order.lab) != 0) 
        x$order.lab
    else x$order, File=File, ...)
    HTML("<P>Height:\n</P>",File=File)
    HTML(x$height, File=File, ...)
    HTML("<P>Agglomerative coefficient:\n</P>",File=File)
    HTML(x$ac, File=File, ...)
    HTML("<P>\n</P>",File=File)
    HTML(x$diss, File=File, ...)
    HTML("<P>\nAvailable components:\n</P>",File=File)
    HTML(names(x), File=File, ...)
    invisible(x)
}

#----------------------------------------------------------------------------------------------------#

"HTML.summary.clara" <- function(x,File=.HTML.File,...) 
{
    HTML("<P>Best sample:\n</P>",File=File)
    HTML(x$sample, File=File, ...)
    HTML("<P>Medoids:\n</P>",File=File)
    HTML(x$medoids, File=File, ...)
    HTML("<P>Clustering vector:\n</P>",File=File)
    HTML(x$clustering, File=File, ...)
    HTML("<P>Objective function:\n</P>",File=File)
    HTML(x$objective, File=File, ...)
    HTML("<P>\nNumerical information per cluster:\n</P>",File=File)
    HTML(x$clusinfo, File=File, ...)
    if (length(x$silinfo) != 0) {
        HTML("<P>\nSilhouette plot information for best sample:\n</P>",File=File)
        HTML(x$silinfo[[1]], File=File, ...)
        HTML("<P>Average silhouette width per cluster:\n</P>",File=File)
        HTML(x$silinfo[[2]], File=File, ...)
        HTML("<P>Average silhouette width of best sample:\n</P>",File=File)
        HTML(x$silinfo[[3]], File=File, ...)
    }
    HTML("<P>\n</P>",File=File)
    HTML(x$diss, File=File, ...)
    HTML("<P>\nAvailable components:\n</P>",File=File)
    HTML(names(x), File=File, ...)
    invisible(x)
}

#----------------------------------------------------------------------------------------------------#

"HTML.summary.diana" <- function(x,File=.HTML.File,...) 
{
    HTML("<P>Merge:\n</P>",File=File)
    HTML(x$merge, File=File, ...)
    HTML("<P>Order of objects:\n</P>",File=File)
    HTML(if (length(x$order.lab) != 0) 
        x$order.lab
    else x$order, File=File, ...)
    HTML("<P>Height:\n</P>",File=File)
    HTML(x$height, File=File, ...)
    HTML("<P>Divisive coefficient:\n</P>",File=File)
    HTML(x$dc, File=File, ...)
    HTML("<P>\n</P>",File=File)
    HTML(x$diss, File=File, ...)
    HTML("<P>\nAvailable components:\n</P>",File=File)
    HTML(names(x), File=File, ...)
    invisible(x)
}

#----------------------------------------------------------------------------------------------------#
 
 "HTML.summary.fanny" <- function(x,File=.HTML.File,...) 
{
    HTML(x$objective, File=File, ...)
    HTML("<P>Membership coefficients:\n</P>",File=File)
    HTML(x$membership, File=File, ...)
    HTML("<P>Coefficients:\n</P>",File=File)
    HTML(x$coeff, File=File, ...)
    HTML("<P>Closest hard clustering:\n</P>",File=File)
    HTML(x$clustering, File=File, ...)
    if (length(x$silinfo) != 0) {
        HTML("<P>\nSilhouette plot information:\n</P>",File=File)
        HTML(x$silinfo[[1]], File=File, ...)
        HTML("<P>Average silhouette width per cluster:\n</P>",File=File)
        HTML(x$silinfo[[2]], File=File, ...)
        HTML("<P>Average silhouette width of total data set:\n</P>",File=File)
        HTML(x$silinfo[[3]], File=File, ...)
    }
    HTML("<P>\n</P>",File=File)
    HTML(x$diss, File=File, ...)
    HTML("<P>\nAvailable components:\n</P>",File=File)
    HTML(names(x), File=File, ...)
    invisible(x)
}

#----------------------------------------------------------------------------------------------------#

"HTML.summary.mona" <- function(x,File=.HTML.File,...) 
{
    HTML.mona(x, File=File, ...)
    invisible(x)
}

#----------------------------------------------------------------------------------------------------#

"HTML.summary.pam" <- function(x,File=.HTML.File,...) 
{
    HTML("<P>Medoids:\n</P>",File=File)
    HTML(x$medoids, File=File, ...)
    HTML("<P>Clustering vector:\n</P>",File=File)
    HTML(x$clustering, File=File, ...)
    HTML("<P>Objective function:\n</P>",File=File)
    HTML(x$objective, File=File, ...)
    HTML("<P>\nNumerical information per cluster:\n</P>",File=File)
    HTML(x$clusinfo, File=File, ...)
    HTML("<P>\nIsolated clusters:\n</P>",File=File)
    HTML("<P>L-clusters: ")
    HTML(names(x$isolation[x$isolation == "L"]), 
        ...)
    HTML("<P>L*-clusters: ")
    HTML(names(x$isolation[x$isolation == "L*"]), 
        ...)
    if (length(x$silinfo) != 0) {
        HTML("<P>\nSilhouette plot information:\n</P>",File=File)
        HTML(x$silinfo[[1]], File=File, ...)
        HTML("<P>Average silhouette width per cluster:\n</P>",File=File)
        HTML(x$silinfo[[2]], File=File, ...)
        HTML("<P>Average silhouette width of total data set:\n</P>",File=File)
        HTML(x$silinfo[[3]], File=File, ...)
    }
    HTML("<P>\n</P>",File=File)
    HTML(x$diss, File=File, ...)
    HTML("<P>\nAvailable components:\n</P>",File=File)
    HTML(names(x), File=File, ...)
    invisible(x)
}

#----------------------------------------------------------------------------------------------------#
### PACKAGE MGCV
#----------------------------------------------------------------------------------------------------#

"HTML.gam" <- function (x, File=.HTML.File,...) 
{
    HTML(x$family,File=File)
    HTML("<P>Formula:\n</P>",File=File)
    HTML(x$formula,File=File)
    if (x$dim == 0) 
        HTML(paste("<P>Total model degrees of freedom <B>", x$nsdf, " </B>\n</P>"),File=File)
    else HTML(paste("\n<P>Estimated degrees of freedom:<B>", paste(x$edf,collapse=" , "), "</B>  total = <B>", 
        paste(sum(x$edf) + x$nsdf,collapse=" , "), "</B>\n</P>"),File=File)
    gcv <- x$df.null * x$sig2/(x$df.null - sum(x$edf) - x$nsdf)
    HTML("\n<P>GCV score:</P> ",,File=File)
    HTML(gcv,File=File)
    invisible(x)
}

#----------------------------------------------------------------------------------------------------#

"HTML.summary.gam" <- function (x, File=.HTML.File,...) 
{
    HTML(x$family,File=File)
    HTML("<P>Formula:\n</P>",File=File)
    HTML(x$formula,File=File)
    if (length(x$p.coeff) > 0) {
        HTML("\n<P>Parametric coefficients:\n</P>",File=File)
        width <- max(nchar(names(x$p.coeff)))

        HTML("\n<P ALIGN=CENTER><TABLE CELLSPACING=0 BORDER=1><TD><TABLE CELLSPACING=0> <TR CLASS= firstline >        <TH></TH><TH>Estimate</TH><TH>std.err.</TH><TH>t ratio</TH><TH>Pr(>|t[)</TH></TR>\n",File=File)

        
        for (i in 1:length(x$p.coeff)) HTML(paste("<TR><TD CLASS=firstcolumn>",formatC(names(x$p.coeff)[i], width = width),"</TD><TD CLASS=CellInside>", formatC(x$p.coeff[i], width = 10,digits = 5),"</TD><TD CLASS=CellInside>", formatC(x$se[i], width = 10, digits = 4),"</TD><TD CLASS=CellInside>",formatC(x$p.t[i], width = 10, digits = 4), "</TD><TD CLASS=CellInside>",format.pval(x$p.pv[i]),"</TD></TR>\n", sep = ""),File=File)
            
           HTML("\n</TABLE></TD></TABLE></CENTER>",File=File) 
        
    }
	HTMLbr( File=File)
    if (x$m > 0) {
        HTML("<P>Approximate significance of smooth terms:\n</P>",File=File)
        width <- max(nchar(names(x$chi.sq)))

        HTML("\n<P ALIGN=CENTER><TABLE CELLSPACING=0 BORDER=1><TD><TABLE CELLSPACING=0> <TR CLASS= firstline > <TH></TH><TH>edf</TH><TH>chi.sq</TH><TH>p-value</TH></TR>\n",File=File)
        for (i in 1:x$m)
        
        HTML(paste("<TR><TD CLASS=firstcolumn>",formatC(names(x$chi.sq)[i], width = width),
        "</TD><TD CLASS=CellInside>", formatC(x$edf[i], width = 10, digits = 4), "</TD><TD CLASS=CellInside>",
            formatC(x$chi.sq[i], width = 10, digits = 5),"</TD><TD CLASS=CellInside>",
            format.pval(x$s.pv[i]), "</TD></TR>\n", sep = ""),File=File)
            
           HTML("\n</TABLE></TD></TABLE></CENTER>",File=File) 
            
    }
    HTML(paste("\n<P>Adjusted r-sq. = <B>", formatC(x$r.sq, digits = 3, width = 5), 
        " </B>   GCV score = <B>", formatC(x$gcv, digits = 5), "</B> \n<BR>Scale estimate = <B>", 
        formatC(x$scale, digits = 5, width = 8, flag = "-"), 
        "    </B>     n = <B>", x$n, "</B>\n</P>", sep = ""),File=File)
        invisible(x)
}


#----------------------------------------------------------------------------------------------------#
### PACKAGE RPART
#----------------------------------------------------------------------------------------------------#


"HTML.rpart" <- function (x, minlength = 0, spaces = 2, cp, digits = getOption("digits"), 
    File=.HTML.File,...) 
{
    if (!inherits(x, "rpart")) 
        stop("Not legitimate rpart object")
    if (!is.null(x$frame$splits)) 
        x <- rpconvert(x)
    if (!missing(cp)) 
        x <- prune.rpart(x, cp = cp)
    frame <- x$frame
    ylevel <- attr(x, "ylevels")
    node <- as.numeric(row.names(frame))
    depth <- tree.depth(node)
    indent <- paste(rep(" ", spaces * 32), collapse = " ")
    if (length(node) > 1) {
        indent <- substring(indent, 1, spaces * seq(depth))
        indent <- paste(c("", indent[depth]), format(node), ")", 
            sep = "")
    }
    else indent <- paste(format(node), ")", sep = "")
    tfun <- (x$functions)$print
    if (!is.null(tfun)) {
        if (is.null(frame$yval2)) 
            yval <- tfun(frame$yval, ylevel, digits)
        else yval <- tfun(frame$yval2, ylevel, digits)
    }
    else yval <- format(signif(frame$yval, digits = digits))
    term <- rep(" ", length(depth))
    term[frame$var == "<leaf>"] <- "*"
    z <- labels(x, digits = digits, minlength = minlength, ...)
    n <- frame$n
    z <- paste(indent, z, n, format(signif(frame$dev, digits = digits)), 
        yval, term)
    omit <- x$na.action
    if (length(omit)) 
        HTML(paste("<P>n=<B>", n[1], "</B> (", naprint(omit), ")\n</P>\n", sep = ""),File=File)
    else HTML(paste("<P>n=<B>", n[1], "</B>\n</P>\n"),File=File)
    if (x$method == "class") 
        HTML("<P>node), split, n, loss, yval, (yprob)\n</P>",File=File)
    else HTML("<P>node), split, n, deviance, yval\n</P>",File=File)
    HTML("<P>      * denotes terminal node\n\n</P>",File=File)
    HTML(paste("<XMP>", paste(z, sep = "\n",collapse="\n"),"</XMP>"),File=File)
    invisible(x)
}

#----------------------------------------------------------------------------------------------------#
### PACKAGE MODREG
#----------------------------------------------------------------------------------------------------#

"HTML.loess" <- function (x, digits = max(3, getOption("digits") - 3),File=.HTML.File, ...) 
{
    if (!is.null(cl <- x$call)) HTMLli(paste("Call: ",paste(deparse(cl),collapse=" ")),File=File)
    HTML(paste("\n<P>Number of Observations:<B>", x$n, "</B>\n</P>"),File=File)
    HTML(paste("<P>Equivalent Number of Parameters:<B>", format(round(x$enp, 
        2)), "</B>\n</P>"),File=File)
    HTML(paste("<P>Residual", if (x$pars$family == "gaussian") 
        " Standard Error: <B>"
    else " Scale Estimate:<B> ", format(signif(x$s, digits)), "</B>\n</P>"),File=File)
    invisible(x)
}

#----------------------------------------------------------------------------------------------------#

"HTML.ppr" <- function (x, File=.HTML.File,...) 
{
    if (!is.null(cl <- x$call)) HTMLli(paste("Call:",paste(deparse(cl),collapse=" ")),File=File)
    mu <- x$mu
    ml <- x$ml
    HTML("\n<P>Goodness of fit:\n</P>",File=File)
    gof <- x$gofn
    names(gof) <- paste(1:ml, "terms")
    HTML(format(gof[mu:ml], ...), File=File)
    invisible(x)
}


#----------------------------------------------------------------------------------------------------#

"HTML.smooth.spline" <- function (x, digits = getOption("digits"), File=.HTML.File,...) 
{
    if (!is.null(cl <- x$call)) HTMLli(paste("Call:",paste(deparse(cl),collapse=" ")),File=File)
    ip <- x$iparms
    cv <- cl$cv
    if (is.null(cv)) 
        cv <- FALSE
    else if (is.name(cv)) 
        cv <- eval(cv)
    HTML(paste("\n<P>Smoothing Parameter  spar=<B>", format(x$spar, digits = digits), 
        "</B> lambda=<B>", format(x$lambda, digits = digits),"</B>", if (ip["ispar"] != 
            1) paste("(", ip["iter"], " iterations)", sep = ""), "\n</P>"),File=File)
    HTML(paste("<P>Equivalent Degrees of Freedom (Df):<B>", format(x$df, digits = digits), 
        "</B>\n</P>"),File=File)
    HTML(paste("<P>Penalized Criterion:<B>", format(x$pen.crit, digits = digits), 
        "</B>\n</P>"),File=File)
    HTML(paste ("<P>",if (cv) "PRESS:"
    else "GCV:", "<B>",format(x$cv.crit, digits = digits), "</B>\n</P>"),File=File)
    invisible(x)
}

#----------------------------------------------------------------------------------------------------#

"HTML.summary.loess" <- function (x, digits = max(3, getOption("digits") - 3), File=.HTML.File,...) 
{
   if (!is.null(cl <- x$call)) HTMLli(paste("Call:",paste(deparse(cl),collapse=" ")),File=File)
	HTML(paste("\n<P>Number of Observations:<B>", x$n, "</B>\n</P>"),File=File)
    	HTML(paste("<P>Equivalent Number of Parameters:<B>", format(round(x$enp, 2)), "</B>\n</P>"),File=File)
    if (x$pars$family == "gaussian") 
        HTML("<P>Residual Standard Error:</P>",File=File)
    else HTML("<P>Residual Scale Estimate:</P>",File=File) 
        HTML(format(signif(x$s, digits)),File=File) 
    HTML("<P>Trace of smoother matrix:</P>",File=File)
    HTML(format(round(x$trace.hat,2)), File=File)
    HTML("\n<P>Control settings:\n</P><UL>",File=File)
    HTMLli(paste("normalize: ", x$pars$normalize, "\n"),File=File)
    HTMLli(paste("  span     : ", format(x$pars$span), "\n"),File=File)
    HTMLli(paste("  degree   : ", x$pars$degree, "\n"),File=File)
    HTMLli(paste("  family   : ", x$pars$family),File=File)
    if (x$pars$family != "gaussian") 
        HTMLli(paste("       iterations =", x$pars$iterations),File=File)
    	HTML("</UL>",File=File)
    HTML(paste("\n<P>  surface  : ", x$pars$surface, if (x$pars$surface == "interpolate")  paste("  cell =", format(x$pars$cell)),"</P>"),File=File)
    invisible(x)
}


#----------------------------------------------------------------------------------------------------#

"HTML.summary.ppr" <- function (x, File=.HTML.File,...) 
{
    HTML.ppr(x,File=File, ...)
    mu <- x$mu
    HTML("\n<P>Projection direction vectors:\n</P>",File=File)
    HTML(format(x$alpha, ...),File=File)
    HTML("\n<P>Coefficients of ridge terms:\n</P>",File=File)
    HTML(format(x$beta, ...), File=File)
    if (any(x$edf > 0)) {
        HTML("\n<P>Equivalent df for ridge terms:\n</P>")
        edf <- x$edf
        names(edf) <- paste("term", 1:mu)
        HTML(round(edf, 2),File=File, ...)
    }
    invisible(x)
}


#----------------------------------------------------------------------------------------------------#
### PACKAGE SPLINES
#----------------------------------------------------------------------------------------------------#



"HTML.bSpline" <- function (x, File=.HTML.File,...) 
{
    value <- c(rep(NA, splineOrder(x)), coef(x))
    names(value) <- format(splineKnots(x), digits = 5)
    HTML(paste("<P> bSpline representation of spline", if (!is.null(form <- attr(x, "formula"))) paste (" for", paste(deparse(as.vector(form)),collapse=" "))  ,"</P>"),File=File)
    HTML(value, File=File,...)
    invisible(x)
}


#----------------------------------------------------------------------------------------------------#

"HTML.polySpline" <- function (x,File=.HTML.File, ...) 
{
    coeff <- coef(x)
    dnames <- dimnames(coeff)
    if (is.null(dnames[[2]])) 
        dimnames(coeff) <- list(format(splineKnots(x)), c("constant", 
            "linear", "quadratic", "cubic", paste(4:29, "th", 
                sep = ""))[1:(dim(coeff)[2])])
    HTML(    paste(    "<P>Polynomial representation of spline ",    if (!is.null(form <- attr(x, "formula")))     	paste(" for ", paste(deparse(as.vector(form)),collapse=" ")  )    ,"</P>")    ,File=File    ) 
    HTML(coeff, File=File,...)
    invisible(x)
}

#----------------------------------------------------------------------------------------------------#

"HTML.ppolySpline" <- function (x,File=.HTML.File, ...) 
{
    HTML("<P>periodic </P>",File=File)
    HTML(paste("\n<P>Period:<B>", format(x[["period"]]), "</B>\n</P>"),File=File)
    NextMethod("HTML",File=File)
    invisible(x)
}



#----------------------------------------------------------------------------------------------------#
### PACKAGE LSQ
#----------------------------------------------------------------------------------------------------#

"HTML.lqs" <- function (x, digits = max(3, getOption("digits") - 3), File=.HTML.File,...) 
{
	if (!is.null(cl <- x$call)) HTMLli(paste("Call:",paste(deparse(cl),collapse=" ")),File=File)    
	
	HTML("<P>Coefficients:\n</P>",File=File)
    HTML.default(format(coef(x), digits = digits), File=File)
    HTML(paste("\n<P>Scale estimates ", paste(format(x$scale, digits = digits),collapse=" "),
        "\n\n</P>"),File=File)
       invisible(x)
}


#----------------------------------------------------------------------------------------------------#
### PACKAGE NLS
#----------------------------------------------------------------------------------------------------#

"HTML.nls" <- function (x, File=.HTML.File,...) 
{
    HTML("<P><B>Nonlinear regression model\n</B></P>",File=File)
    HTMLli(paste("Model: ", paste(deparse(formula(x)),collapse=" "), "\n"),File=File)
    HTMLli(paste("Data: ", as.character(x$data), "\n"),File=File)
    HTML(x$m$getAllPars(),File=File)
    HTMLli(paste("Residual sum-of-squares: ", format(x$m$deviance()),"\n"),File=File)
    invisible(x)
}


#----------------------------------------------------------------------------------------------------#

"HTML.summary.nls" <- function (x, digits = max(3, getOption("digits") - 3), symbolic.cor = p > 
    4, signif.stars = getOption("show.signif.stars"), File=.HTML.File,...) 
{
    
    HTML(paste("<P>Formula:",paste(deparse(x$formula), collapse = " "),"</P>"),File=File)
    df <- x$df
    rdf <- df[2]
    HTML("\n<P>Parameters:\n</P>",File=File)
    HTML.coefmat(x$parameters, digits = digits, signif.stars = signif.stars, 
        File=File,...)
    HTML(paste("\n<P>Residual standard error:<B> ", format(signif(x$sigma, 
        digits)), " </B>on <B>", rdf, " </B>degrees of freedom\n</P>"),File=File)
    correl <- x$correlation
    if (!is.null(correl)) {
        p <- dim(correl)[2]
        if (p > 1) {
            HTML("\n<P>Correlation of Parameter Estimates:\n</P>",File=File)
            if (symbolic.cor) 
                HTML(symnum(correl)[-1, -p],File=File)
            else {
                correl[!lower.tri(correl)] <- NA
                HTML(correl[-1, -p, drop = FALSE], File=File)
            }
        }
    }
    HTMLbr(File=File)
    invisible(x)
}


#----------------------------------------------------------------------------------------------------#
### PACKAGE STEPFUN
#----------------------------------------------------------------------------------------------------#

"HTML.ecdf" <- function (x, digits = getOption("digits") - 2, File=.HTML.File,...) 
{
    numform <- function(x) paste(formatC(x, dig = digits), collapse = ", ")
    HTML(paste("<P><B>Empirical CDF</B></P> \n<LI>Call: ", paste(deparse(attr(x, "call")),collapse=" ")), File=File)
    n <- length(xx <- eval(expression(x), env = environment(x)))
    i1 <- 1:min(3, n)
    i2 <- if (n >= 4) 
        max(4, n - 1):n
    else integer(0)
    HTML(paste(" x[1:", n, "] = ", numform(xx[i1]), if (n > 3) 
        ", ", if (n > 5) 
        " ..., ", numform(xx[i2]), "\n<BR>", sep = ""),File=File)
    invisible(x)
}


#----------------------------------------------------------------------------------------------------#

 "HTML.stepfun" <- function (x, digits = getOption("digits") - 2, File=.HTML.File,...) 
{
    numform <- function(x) paste(formatC(x, dig = digits), collapse = ", ")
    i1 <- function(n) 1:min(3, n)
    i2 <- function(n) if (n >= 4) 
        max(4, n - 1):n
    else integer(0)
    HTML(paste("<P><B>Step function</B></P>\n<LI>Call: ",paste(deparse(attr(x, "call")) ,collapse=" ")),File=File)
    env <- environment(x)
    n <- length(xx <- eval(expression(x), env = env))
    HTML(paste(" x[1:", n, "] = ", numform(xx[i1(n)]), if (n > 3) 
        ", ", if (n > 5) 
        " ..., ", numform(xx[i2(n)]), "\n<BR>", sep = ""),File=File)
    y <- eval(expression(c(yleft, y)), env = env)
    HTML(paste(n + 1, " step heights = ", numform(y[i1(n + 1)]), if (n + 
        1 > 3) 
        ", ", if (n + 1 > 5) 
        " ..., ", numform(y[i2(n + 1)]), "\n<BR>", sep = ""),File=File)
    invisible(x)
}

#----------------------------------------------------------------------------------------------------#
### PACKAGE SURVIVAL
#----------------------------------------------------------------------------------------------------#

"HTML.date" <- function (x, quote, prefix, File=.HTML.File,...) 
{
	y<-x
    fun <- options()$print.date
    if (is.null(fun)) x <- date.ddmmmyy(x)
    else x <- get(fun)(x)
    if (missing(quote))  quote <- FALSE
    HTML.atomic(x, File=File)
    invisible(y)
}


#----------------------------------------------------------------------------------------------------#

"HTML.cox.zph" <- function (x, digits = max(options()$digits - 4, 3), File=.HTML.File,...) 
HTML(x$table, File=File)

#----------------------------------------------------------------------------------------------------#

"HTML.coxph.null" <- function (x, digits = max(options()$digits - 4, 3), File=.HTML.File,...) 
{
	if (!is.null(cl <- x$call)) HTMLli(paste("Call:",paste(deparse(cl),collapse=" ")),File=File)        
    HTML(paste("<P>Null model  log likelihood=<B>", format(x$loglik), "</B>\n</P>"),File=File)
    omit <- x$na.action
    if (length(omit)) HTML(paste("<P>  n=<B>", x$n, " </B>(", naprint(omit), ")\n</P>", sep = ""),File=File)
    else HTML(paste("<P>  n=<B>", x$n, "</B>\n</P>"),File=File)
}

#----------------------------------------------------------------------------------------------------#
### UTILITARY FUNCTIONS
#----------------------------------------------------------------------------------------------------#

"HTMLbr"<- function(x=1,File = .HTML.File) { cat(paste("\n",rep("<br>&nbsp;",x),"\n",sep=""), append = TRUE, file = File)}

#----------------------------------------------------------------------------------------------------#

"HTMLhr"<- function(File = .HTML.File, Width = "100%", Size = "1"){ cat(paste("<hr width=", Width, " size=", Size, ">", sep = ""), file = File, append = TRUE, sep = "")}

#----------------------------------------------------------------------------------------------------#

"HTMLli"<- function(txt="", File = .HTML.File) { cat(paste("<br><li>", txt, sep = ""), sep = "", append = TRUE, file = File)}

#----------------------------------------------------------------------------------------------------#

"HTML.fun"<-function(x,File=.HTML.File,...){
cat(paste("<BR><XMP CLASS=function>",paste(attributes(x)$source,collapse="\n"),"</XMP><BR>",sep=""),file=File,append=TRUE,sep="<BR>")
invisible(x)
}

#----------------------------------------------------------------------------------------------------#

"HTMLplot" <- function (Caption = "", File = .HTML.File, GraphDirectory = ".", GraphFileName = "", GraphSaveAs = "png", GraphBorder = 1,  Align = "center", ...)
{
    if (GraphFileName == "") {
        nowd <- date()
        GraphFileName <- paste("GRAPH_", substring(nowd, 5, 7), substring(nowd, 9, 10), "_", substring(nowd, 12, 13), substring(nowd, 15,  16), substring(nowd, 18, 19), sep = "")
    }
    GraphFileName <- paste(GraphFileName, ".", GraphSaveAs, sep = "")
    AbsGraphFileName <- file.path(GraphDirectory, GraphFileName)
    if (GraphSaveAs=="png") dev.print(png, file = AbsGraphFileName, width = 400)
    else if (GraphSaveAs=="jpg") dev.print(jpeg, file = AbsGraphFileName, width = 400)
    else if (GraphSaveAs=="gif") dev.print(gif, file = AbsGraphFileName, width = 400)
    else stop("GraphSaveAs must be either jpg, png or gif")
    cat(paste("<p align=", Align, "><img src='", GraphFileName, "' border=", GraphBorder, ">", sep = "", collapse = ""), file = File, append = TRUE, sep = "")
    if (Caption != "") {
        cat(paste("<br><i>", Caption, "</i>"), file = File, append = TRUE, sep = "")
    }
    cat("</P>", file = File, append = TRUE, sep = "\n")
    try(assign(".HTML.graph", TRUE, env = get("HTMLenv", envir = .GlobalEnv)))
    invisible(return())
}

#----------------------------------------------------------------------------------------------------#
###   R2HTML CORE
#----------------------------------------------------------------------------------------------------#



"HTMLStart" <- function(outdir=".",filename="index",extension="html",echo=FALSE, HTMLframe=TRUE, withprompt="HTML> ",CSSFile="R2HTML.CSS",BackGroundColor="FFFFFF",BackGroundImg="",Title="R output")
{
	# Creation of an environment to save some parameters

	assign("HTMLenv",new.env(parent=.GlobalEnv),envir=.GlobalEnv)

	assign("oldprompt",getOption("prompt"),envir=get("HTMLenv",envir=.GlobalEnv))
	assign("HTMLframe",HTMLframe,envir=get("HTMLenv",envir=.GlobalEnv))
	assign(".HTML.outdir",outdir,envir=get("HTMLenv",envir=.GlobalEnv))

	options(prompt=withprompt)

	# Utilitary functions replacement 
	

	 fix<-function (x, ...) 	{
	    subx <- substitute(x)
	    if (is.name(subx)) 
		subx <- deparse(subx)
	    if (!is.character(subx) || length(subx) != 1) 
		stop("fix requires a name")
		
		assign(".HTML.fix",TRUE,env=get("HTMLenv",envir=.GlobalEnv))
		assign(".HTML.fixed",subx,env=get("HTMLenv",envir=.GlobalEnv))
		
		parent <- parent.frame()
	    if (exists(subx, envir = parent, inherits = TRUE)) 
		x <- edit(get(subx, envir = parent), ...)
	    else {
		x <- edit(function() {
		}, ...)
		environment(x) <- .GlobalEnv	    }
	
	assign(subx, x, env = .GlobalEnv)
	}

	assign("fix",fix,env=.GlobalEnv)

	assign(".HTML.fix",FALSE,envir=get("HTMLenv",envir=.GlobalEnv))
	assign(".HTML.graph",FALSE,env=get("HTMLenv",envir=.GlobalEnv))	
	
	# Creation of required HTML files

	try(.HTML.File <- HTMLInitFile(outdir = outdir,filename=filename,extension=extension,HTMLframe=HTMLframe, BackGroundColor = BackGroundColor, BackGroundImg = BackGroundImg, Title = Title,CSSFile=CSSFile))
	

	ToHTML <- function(File,echo,HTMLframe,HTMLMenuFile,target,outdir)
	{
		NumCom<-0
		function(expr,value,ok,visible)
		{
		
		NumCom<<- NumCom+1

		if (NumCom>1){
			
			ToPrint<-TRUE
			
			if (get(".HTML.fix",envir=get("HTMLenv",envir=.GlobalEnv))==TRUE)
			{
						ToPrint<-FALSE
						ficName<-paste("fun",format(Sys.time(), "%j%m%H%M%S"),"-",floor(runif(1,1,10000)),".txt",sep="")
						AbsficName<-file.path(outdir,ficName)
						
						FunName<-get(".HTML.fixed",envir=get("HTMLenv",envir=.GlobalEnv))
						if (echo) HTMLCommand(paste("fix(",FunName,")",sep=""),File,NumCom) else cat(paste("<A NAME=Num",NumCom,">&nbsp</A>",sep=""),file=File,sep="",append=TRUE)
						if (HTMLframe) HTMLCommand(paste("fix(",FunName,")",sep=""),HTMLMenuFile,NumCom,menu=TRUE,target=target)					
						dput(get(FunName),file=AbsficName)
						HTML(paste("<P> Function <a href=", ficName, " target=_blank>", FunName, "</a> fixed. </P>",sep=""))
						
						assign(".HTML.fix",FALSE,envir=get("HTMLenv",envir=.GlobalEnv))

			}
			
			else
			{
			
			
				if (get(".HTML.graph",envir=get("HTMLenv",envir=.GlobalEnv))==TRUE)
					{
						ToPrint <- FALSE
						assign(".HTML.graph",FALSE,envir=get("HTMLenv",envir=.GlobalEnv))	
					}
				else
					{
			
			
					if (length(expr)>1) {if (expr[[1]]=="<-") ToPrint<-FALSE}
			

			# Print the commands and/or it's number 
			
				if (echo) HTMLCommand(deparse(expr),File,NumCom) else cat(paste("<a name=Num",NumCom,">&nbsp</A>",sep=""),file=File,sep="",append=TRUE)
				if (HTMLframe) HTMLCommand(deparse(expr),HTMLMenuFile,NumCom,menu=TRUE,target=target)

				if (ToPrint) HTML(value,File=File)
					}
				
			
			}
		}
		invisible(return(TRUE))
		}	
	}


	on.exit(addTaskCallback(ToHTML(.HTML.File,echo=echo,HTMLframe=HTMLframe,HTMLMenuFile=file.path(outdir,paste(filename,"_menu.",extension,sep="")),target=paste(filename,"_main.",extension,sep=""),outdir=outdir),name="HTML"),add=TRUE)
	
	cat("\n *** Output redirected to directory: ", outdir)
	cat("\n *** You should also put in the same directory the CSS file and the logo.")
	cat("\n     You can found those in the directory where you installed R2THML library.")
	cat("\n *** Use HTMLStop() to end redirection.")
	invisible(return(TRUE))

}
#----------------------------------------------------------------------------------------------------#

"HTMLInitFile"<-function(outdir = ".",filename="index",extension="html",HTMLframe=FALSE, BackGroundColor = "FFFFFF", BackGroundImg = "", Title = "R output",CSSFile="R2HTML.CSS")
{
if (HTMLframe==FALSE){
	File<-file.path(outdir,paste(filename,".",extension,sep=""))

	assign(".HTML.File",File,env=.GlobalEnv)
	cat(paste("<HTML>\n <META NAME=Generated by a library created by Eric Lecoutre> \n \n<HEAD> \n <TITLE>",Title,"\n</TITLE> <META http-equiv=content-type content=text/html;charset=iso-8859-1> \n <LINK rel=stylesheet href=R2HTML.css type=text/css> \n </HEAD> \n <BODY BGCOLOR=", BackGroundColor, if(is.null(BackGroundImg) == FALSE) paste(" BACKGROUND='", BackGroundImg, "'", sep = ""), ">"), append = FALSE, sep = "", file = File)
	}
else	{
	filemenu<-paste(filename,"_menu.",extension,sep="")
	filemain<-paste(filename,"_main.",extension,sep="")
	absfilemenu<-file.path(outdir,filemenu)
	File<-absfilemain<-file.path(outdir,filemain)
	absfileindex<-file.path(outdir,paste(filename,".",extension,sep=""))
	assign(".HTML.File",absfilemain,env=.GlobalEnv)
	cat(paste("<HTML><META NAME=Generated by a library created by Eric Lecoutre>\n\n<HEAD>	\n <TITLE>R output, generated on :",	date(),	"</TITLE>\n <META http-equiv=content-type content=text/html;charset=iso-8859-1>\n <FRAMESET COLS=250,* BORDER=1 FRAMEBORDER=yes><FRAME SRC=",filemenu," NAME=menu SCROLLING=yes><FRAME SRC=",filemain," NAME=main SCROLLING=yes></FRAMESET></BODY></HTML>"), append = FALSE, sep = "", file = absfileindex)
	cat("<HTML><HEAD><LINK rel=stylesheet href=R2HTML.css type=text/css> </HEAD></HEAD><BODY BGCOLOR=#E5F5FF>  <CENTER> <IMG SRC=R2HTMLlogo.gif> <HR SIZE=1></CENTER><BR>",sep="",append=FALSE,file=absfilemenu)
	cat(paste("<HTML><META NAME=Generated by a library created by Eric Lecoutre>\n\n<HEAD>	\n <TITLE>R output, generated on :",	date(),	"</TITLE>\n <META http-equiv=content-type content=text/html;charset=iso-8859-1>\n <LINK rel=stylesheet href=R2HTML.css type=text/css>\n </HEAD><BODY BGCOLOR=", BackGroundColor, if(is.null(BackGroundImg) == FALSE) paste(" BACKGROUND='", BackGroundImg, "'", sep = ""), ">"), append = FALSE, sep = "", file = absfilemain)
	}

	invisible(return(File))
}

#----------------------------------------------------------------------------------------------------#

"HTMLEndFile"<- function(File = .HTML.File)
{
	cat("\n<HR SIZE=1>\n<FONT SIZE=-1>\n\t Generated on: <I>", date(), 
		"</I> - <B>R2HTML</B> - Author: Eric Lecoutre\n</FONT>\n<HR SIZE=1>\n\t</BODY>\n</HTML>",
		sep = "", append = TRUE, file = File)
}


#----------------------------------------------------------------------------------------------------#

"HTMLStop"<-function()
{
invisible(removeTaskCallback("HTML"))
options(prompt=get("oldprompt",envir=get("HTMLenv",envir=.GlobalEnv)))
HTMLEndFile(File=get(".HTML.File",envir=get("HTMLenv",envir=.GlobalEnv)))

on.exit(rm("HTMLenv",envir=.GlobalEnv),add=TRUE)
on.exit(try(rm("fix",pos=1)),add=TRUE)
return("Thanks for using R2HTML")
}

#----------------------------------------------------------------------------------------------------#

.First.lib <- function(lib,pkg)
{

	cat("\n+----------------------------------------------------+")
	cat("\n|                                                    |")
	cat("\n| RRRRR      222    -------------------------------- |")
	cat("\n| R    R    2   2    H     H TTTTTTT MM    MM L      |")
	cat("\n| R    R         2   H     H    T    MM    MM L      |")
	cat("\n| R    R         2   H     H    T    M M  M M L      |")
	cat("\n| R    R         2   H     H    T    M M  M M L      |")
	cat("\n| R   R         2    HHHHHHH    T    M M  M M L      |")
	cat("\n| RRRR         2     H     H    T    M  MM  M L      |")
	cat("\n| R   R       2      H     H    T    M  MM  M L      |")
	cat("\n| R    R     2       H     H    T    M      M L      |")
	cat("\n| R     R   2        H     H    T    M      M LLLLL  |")
	cat("\n| R      R  222222  -------------------------------- |")
	cat("\n|                                                    |")
	cat("\n+----------------------------------------------------+")
	cat("\n| R2HTML - Library of exportation to HTML for R      |")
	cat("\n| GNU Licence - Feel free to use and distribute      |")
	cat("\n| Ver. 1.0 - Eric Lecoutre (lecoutre@stat.ucl.ac.be) |")
	cat("\n+----------------------------------------------------+")
	cat("\n\n")


}


#----------------------------------------------------------------------------------------------------#