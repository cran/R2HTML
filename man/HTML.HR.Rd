\name{HTML.HR}
\alias{HTML.HR}
\title{
Break line
}
\description{
Insert a break line (HTML code <HR> in the HTML report)
}
\usage{
HTML.HR(File=HTML.File, Width="100\%", Size="2")
}
\arguments{
\item{File}{
HTML file to write in. If "" is used, then HTML code is redirected in standard output.


}
\item{Width}{
Width of the break line, either in percent or in pixels.
}
\item{Size}{
Height of the line.
}}
\value{
None.
}
\section{Side Effects}{
HTML code is written in File.
}
\seealso{
´HTMLTitre', ´HTML.BR'
}
\examples{
 HTML.HR()
 HTML.HR(Width=200,Size=4)
}
\keyword{line}
% Converted by Sd2Rd version 1.10.
