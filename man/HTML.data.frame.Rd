\name{HTML.data.frame}
\alias{HTML.data.frame}
\alias{HTML.matrix}
\title{ Write a data.frame (or matrix) to a HTML output}
\description{
  This function exports a data.frame to a HTML file. Many arguments allow to customize the layout of the HTML table.
}
\synopsis{HTML.data.frame(x, file = .HTML.file, Border = 1, innerBorder=0, classfirstline = "firstline",classfirstcolumn = "firstcolumn", classcellinside = "cellinside", digits = 2, append = TRUE, align = "center", caption = "", captionalign = "bottom", classcaption = "captiondataframe", classtable = "dataframe", ...)
}
\usage{
HTML.data.frame(x, file = .HTML.file, Border = 1, innerBorder=0,classfirstline = "firstline",\cr 
classfirstcolumn = "firstcolumn", classcellinside = "cellinside", digits = 2, \cr
append = TRUE, align = "center", caption = "", captionalign = "bottom", \cr
classcaption = "captiondataframe", classtable = "dataframe", ...)
HTML.data.frame(x,file=)
}

\arguments{
  \item{x}{ a data.frame}
  \item{file}{ target HTLM output}
  \item{Border}{ the size of the border around the table. Could be 0,1,... but also NULL}
  \item{innerBorder}{ the size of the border inside the table - see details}
  \item{classfirstline}{ CSS class for the first line (header - variable names) }
  \item{classfirstcolumn}{ CSS class for the first column (rownames)}
  \item{classcellinside}{ CSS class for others cells}
  \item{digits}{ number of digits to use for rounding}
  \item{append}{ logical. If 'TRUE' output will be appended to 'file'; otherwise, it will overwrite the contents of 'file'}
  \item{align}{ alignment to be used: center, left or right}
  \item{caption}{ optional caption to append to the table }
  \item{captionalign}{ alignment to be used for the caption: could be bottom or top}
  \item{classcaption}{ CSS class to use for caption}
  \item{classtable}{ CSS class to be used for the whole table (in html <table> statement)}
   \item{\dots}{ ...}
}
\details{
	For the moment, \code{HTML.matrix} and \code{HTML.data.frame} do have the same options.
	
	Tables are build using two different HTML tables, one beeing encapsulated within the other, which allows to have a table without borders inside but with a border outside. It is nevertheless recommanded to rely on CSS to obtain such results...
}
\value{
	no value returned.  
}

\author{ Eric Lecoutre}




\seealso{ \code{\link{HTML}}}
\examples{
tmpfic=HTMLInitFile(tempdir(),CSSFile="http://www.stat.ucl.ac.be/R2HTML/R2HTML.css")
data(iris)
HTML(as.title("Fisher Iris dataset"),file=tmpfic)
HTML(iris, file=tmpfic)
# File is generated, you can call the browser:
\dontrun{browseURL(tmpfic)}
}
\keyword{datasets}
\keyword{IO}
