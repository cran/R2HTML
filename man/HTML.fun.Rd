\name{HTML.fun}
\alias{HTML.fun}

\title{ Writes the code of a function to a target HTML file }
\description{
  Writes the code of a function to a target HTML file 
}
\usage{
HTML.fun(x, File = .HTML.File, ...)
}

\arguments{
  \item{x}{ Name of a function }
  \item{File}{ target HTLM output}
  \item{\dots}{ ...}
}
\details{
  The code of tyhe function is written in the target HTML file, using HTML <XMP> tag. A CSS class called "function" is used to personalise the output.
}
\value{
	no value returned.  
}
\references{ }
\author{ Eric Lecoutre }

\note{ For a discussion about .HTML.File default value for File argument, refer to \code{\link{HTMLStart}} }

\seealso{  \code{\link{HTML}}}

\examples{

## Define a function and export it's code to the file /test.html.
## Target file may be changed when submitting this code...

myfile <- paste(tempfile(),".html",sep="")
myfun <- function(x){
	cat("\n Euclidian norm")
	return(sqrt(sum(x^2)))
 }
HTML.fun(myfun,File=myfile)
cat("\n Test output written in: ",myfile)

}

\keyword{ print }
\keyword{ IO }
\keyword{ file }
