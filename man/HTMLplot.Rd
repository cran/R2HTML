\name{HTMLplot}
\alias{HTMLplot}

\title{ Insert a graphic into an HTML output }
\description{
  Exports the active graphic to a JPEG or GIF file and add it to a target HTML output, by writing the <IMG> tag.
}
\usage{
HTMLplot(Caption="",File = .HTML.File, GraphDirectory = get(".HTML.outdir", env = get("HTMLenv", envir = .GlobalEnv)), GraphFileName = "", GraphSaveAs = "jpg", GraphBorder = 1, Align = "center", ...)
}

\arguments{
  \item{File}{ the target HTML file }
  \item{GraphDirectory}{ path where file should be stored }
  \item{GraphFileName}{ name of the file to produce (could be missing) }
  \item{GraphSaveAs}{ an existing exportation device, such as jpg or gif }
  \item{GraphBorder}{ HTML border option for the <IMG> tag }
  \item{Align}{ HTML align option for the <IMG> tag}
  \item{Caption}{text to be placed below the graphic, as a caption}
  \item{\dots}{ ... }
}
\details{
Note that this function is coded to work automatically when using automatic exportation with \code{\link{HTMLStart}}. When using manualy, user should pay attention to the GraphDirectory option, so that graph files are in the same directory as HTML output files. 
}
\value{
	no value returned.  
}
\references{}

\author{ Eric Lecoutre }
\note{ For a discussion about .HTML.outdir and HTLMenv, refer to \code{\link{HTMLStart}} }


\seealso{ \code{\link{HTMLStart}}}

\examples{

## Plots a graphic and insert it into the file /test.html.
## Target file and also graph directory should be changed when submitting this code...

myfile <- paste(tempfile(),".html",sep="")
plot(sin, -pi, 2*pi,main="Sinus")
# HTMLplot(File=myfile,GraphDirectory="/",Caption="Look at this curve!")
}

\keyword{ print }
\keyword{ IO }
\keyword{ file }

