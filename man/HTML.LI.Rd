\name{HTML.LI}
\alias{HTML.LI}
\title{
Create lists
}
\description{
Insert a list bullet in HTML report.
}
\usage{
HTML.LI(txt, File=HTML.File)
}
\arguments{
\item{txt}{
Text to be written after the bullet.
}
\item{File}{
HTML file to write in. If "" is used, then HTML code is redirected in standard output.
}}
\value{
None.
}
\section{Side Effects}{
HTML code is written in File.
}
\examples{


Sigma <- matrix(c(10,3,3,2),2,2)
HTML.LI("1. Firstly, consider matrix Sigma")
HTML.LI("2. We will inverse Sigma")
solve(Sigma)
}
\keyword{list}
% Converted by Sd2Rd version 1.10.
