 data(iris)
 data <- iris[sample(1:nrow(iris),size=30),]

 .HTML.file = HTMLInitFile(useGrid=TRUE,useLaTeX=FALSE)
 HTML.title("Iris dataset (again)",1,file=.HTML.file)  
 HTML(as.title("20 random observations displayed with HTMLgrid"),HR=3)
 HTML("Try to click on columns headers to sort them")
 HTMLgrid_inline(data)
 HTML(as.title("A summary of those observations displayed with HTMLgrid"),HR=3)
 
 HTMLgrid_summary(data,file=.HTML.file)
 cat("file:", .HTML.file, "is created")
 browseURL(paste("file://",.HTML.file,sep=""))

 
 cat("Some text and then a math mode:",file=.HTML.file,append=TRUE)
 HTML(as.latex("[[a,b],[c,d]]((n),(k))") ,file=.HTML.file)
 cat(". Nice isn't it?",file=.HTML.file,append=TRUE)
 HTML(as.latex("\int_{-\infty}^{1}f(x)dx",inline=FALSE,count=TRUE) ,file=.HTML.file)   
 HTML(as.title("Labelled equations"),HR=3)
 HTML(as.latex("x+b/(2a)=+-sqrt((b^2)/(4a^2)-c/a)",inline=FALSE,label="Label of this equation"))      

