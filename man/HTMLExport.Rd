\name{HTMLExport}
\alias{HTMLExport}
\title{
Export data to HTML - Generic function
}
\description{
Prints HTML code corresponding to data.
}
\usage{
HTMLExport(x, \dots)
}
\arguments{
\item{x}{
any object. Missing values (NA) are not allowed.
}
\item{...}{
some methods have optional arguments. File may be specified ("" stands for screen output rather than physical file)


}}
\seealso{
´HTMLSink()'
}
\examples{


 HTMLExport(diag(rep(1,3)));
 data(cars);
 HTMLExport(cars, File="");


}
\keyword{HTML}
% Converted by Sd2Rd version 1.10.
