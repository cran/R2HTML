\name{HTMLReplaceNA}
\alias{HTMLReplaceNA}
\title{
Replace NA by printable character (-)
}
\description{
For output, NA are not nive characters\dots{} Internallly used, HTMLReplaceNA take a vector and replaces all NA inside by a character, "-" by default.
}
\usage{
HTMLReplaceNA(Vec, Replace="-")
}
\arguments{
\item{Vec}{
Vector of data.
}
\item{Replace}{
Charecter (or string) to be used for replacement.
}}
\value{
Output is the entry vector returned with character mode and missing values changed by Replace.
}
\keyword{missing}
% Converted by Sd2Rd version 1.10.
