\name{HTML.BR}
\alias{HTML.BR}
\alias{HTML.HR}
\alias{HTML.LI}

\title{ Facility functions to write HTML code }
\description{
  Write <br>, <li> and <hr> tags, which are often used, to an output file.
}
\usage{
HTML.BR(File = .HTML.File)
HTML.LI(txt="",File = .HTML.File)
HTML.HR(File = .HTML.File, Width = "100%", Size = "1")
}

\arguments{
  \item{txt}{ text to appear after the <li> tag }
  \item{File}{ HTML target output file}
  \item{Width}{ value of the width HR optional argument, in pixel or percent}  
  \item{Size}{ value of the size HR optional argument}  
}

\value{
	no value returned.
}

\references{}
\author{ Eric Lecoutre}

\note{ For a discussion about .HTML.File default value for File argument, refer to \code{\link{HTMLStart}} }

 

\seealso{  \code{\link{HTML}}}

\examples{

## Insert a line to a HTML file
## Change the path/name of the file to redirect to your test file

myfile <- paste(tempfile(),".html",sep="")
HTML.HR(File=myfile)
cat("\n Test output written in: ",myfile)

}
\keyword{ print }
\keyword{ IO }
\keyword{ file }
