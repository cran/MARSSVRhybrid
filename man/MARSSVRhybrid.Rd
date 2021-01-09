\name{MARSSVRhybrid}
\alias{MARSSVRhybrid}
 \title{Machine Learning Hybrid Model}
\usage{MARSSVRhybrid(Data,k,deg,funct)}
\description{Multivariate Adaptive Regression Spline (MARS) based Support Vector Regression (SVR) hybrid model is combined Machine learning hybrid approach which selects important variables using MARS and then fits Support Vector Regression on the extracted important variables.}
%-
  \arguments{
    \item{Data}{
      A Multivariate data in regression framework, with first column as Y (dependent variable) and remaining colums      as X (independent variables).}
    \item{k}{
      Partition value for spliting the data set into training and testing

    }
    \item{deg}{
      Degree of Multivariate Adaptive Regression Spline model to be used for model building

    }
    \item{funct}{
    Kernel function of Support Vector Regression to be used for model building
    }
  }
\details{Multivariate Adaptive Regression Spline based Support Vector Regression hybrid uses the combined hybrid approach proposed by Das (2019). Multivariate Adaptive Regression Spline model identify the important variables from a multivariate dataset based on Generalised Cross VAlidation value (GCV) and Residual Sum of Squares (RSS) values. The Support Vector Regression model uses these selected important variables to build the prediction model. Further this developed model is used for predicting the dependent variable.}


\value{It returns the accuracy measures of the fitted Machine learning hybrid model.}

\references{
  %%
    Das, P (2019)<http://krishikosh.egranth.ac.in/handle/1/5810147805>
}
\author{
  Pankaj Das,Achal Lama, Girish Jha
}
\examples{
data(trees)
MARSSVRhybrid(trees,0.8,1,funct="radial")
}
\keyword{Hybrid}
\keyword{Machine learning}
\keyword{MARS}
\keyword{SVR}
