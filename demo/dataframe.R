tmpfic=HTMLInitFile(tempdir(),CSSFile=system.file("samples", "R2HTML.css", package="R2HTML"))
data(iris)
HTML(as.title("Fisher Iris dataset"),file=tmpfic)
HTML(iris, file=paste("file://",tmpfic,sep=""))
browseURL(tmpfic)
