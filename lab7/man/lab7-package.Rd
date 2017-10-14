\name{lab7-package}
\alias{lab7-package}
\alias{lab7}
\docType{package}
\title{
\packageTitle{lab7}
}
\description{
\packageDescription{lab7}
}
\details{

The DESCRIPTION file:
\packageDESCRIPTION{Geocode}
\packageIndices{Geocode}
The lab contains creation of a package which will use the ridge regression class.
}
\author{ packageAuthor {Syeda Farha Shazmeen}

Maintainer packageMaintainer{Farhana Chowdhury Tondra}
}

\keyword{ ridge }
\examples{
 data(iris)
 ridgereg_mod <- ridgereg$new(Petal.Length~Species,data=iris,lambda =.25)
ridgereg_mod$print_QR()
ridgereg_mod$coef_QR()
ridgereg_mod$predict_QR()
}
