\name{HTML.title}
\alias{HTML.title}
\alias{as.title}

\title{ Writes a title in a target HTML output }
\description{
  A title is a string with the class "title". The function \code{as.title} gives this class to an object, so that title method of HTML could apply to it.
}
\usage{
HTML.title(x, HR = 2, class = "bigtitle", File = .HTML.File, ...)
as.title(x)
}

\arguments{
  \item{x}{ string }
  \item{HR}{ rank attribute of the HTML <H?> tag }
  \item{class}{ CSS class to use for personalised reports }
  \item{File}{ the target HTML file }
  \item{\dots}{ ... }
}
\details{
}
\value{
	no value returned.  
}
\references{  }
\author{ Eric Lecoutre }

\note{ For a discussion about .HTML.File default value for File argument, refer to \code{\link{HTMLStart}} }

\seealso{  \code{\link{HTML}} }

\examples{
## Write a title in the file /test.html.
## Target file may be changed when submitting this code...

myfile <- paste(tempfile(),".html",sep="")

tit1 <- as.title("This is method 1")

HTML(tit1, File=myfile)

HTML.title("This is method 2",File=myfile, HR=3)
cat("\n Test output written in: ",myfile)


}
\keyword{ print }
\keyword{ IO }
\keyword{ file }
