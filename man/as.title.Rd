\name{as.title}
\alias{as.title}
\title{
Title Objects
}
\description{
Creates objects of class title
}
\usage{
as.title(x)
}
\arguments{
\item{x}{
x should be a character array
}}
\value{
returns x with class title
}
\section{Side Effects}{
Objects with class title could be used in HTML exportation to create titles in HTML pages.
They are used by default by ´HTMLExport.title'.
An other way to create titles (offering control of appearance) is to use ´HTMLTitre'.
}
\seealso{
´HTMLExport' ; ´HTMLExport.title' ; ´HTMLTitre'
}
\examples{
mytitle_as.title("Here I put a title")
mytitle
#creates automatically a title if HTLKSink is on
HTMLExport(mytitle) 
#manually otherwise
}
\keyword{title}
% Converted by Sd2Rd version 1.10.
