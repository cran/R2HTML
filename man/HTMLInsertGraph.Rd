\name{HTMLInsertGraph}
\alias{HTMLInsertGraph}
\title{
Insert a graph in a HTML page
}
\description{
Automatically exports a graph in JPEG (or GIF) format
and places HTML reference tag in the output HTML file.
If HTML Sink is on (default zhen loading library), you should't have to use HTMLInsertGraph.
}
\usage{
HTMLInsertGraph(File=HTML.File, NameG="GSD2", GraphSaveAs="JPG", GraphDirectory=".\\", GraphFileName="", GraphBorder=1, Align="center", \dots)
}
\arguments{
\item{File}{
Name of the HTML file. By default, HTML.File is used, which is the file used by HTMLSink (this is output.html in your S+ home directory).
}
\item{NameG}{
Name of the graphsheet graph is.
}
\item{GraphSaveAs}{
Format to use for exportation (you can use JPG or GIF).
}
\item{GraphDirectory}{
Directory to store your graphs.
}
\item{GraphFileName}{
Name of the file to create. If none (by default), an unique name is given, using system date and time.
}
\item{GraphBorder}{
Size of the border. If 0, graph object will not have border in HTML page.
}
\item{Align}{
Alignment of graph. Accepted values are "center", "left" or "right".
}}
\value{
None
}
\examples{
plot(rnorm(20,0,1))
HTMLInsertGraph()
}
\keyword{graph}
% Converted by Sd2Rd version 1.10.
