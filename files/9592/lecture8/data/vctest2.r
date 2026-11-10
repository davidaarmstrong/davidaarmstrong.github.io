VC.test2 <- function (obj1, obj2, sig.lev = 0.05) 
{
  require(gamlss)
  if (!is.gamlss(obj1)) 
    stop(paste("Object 1 is not a gamlss object", "\n", ""))
  if (!is.gamlss(obj2)) 
    stop(paste("Object 2 is not a gamlss object", "\n", ""))
  l1 <- logLik(obj1)[1]
  l2 <- logLik(obj2)[1]
  if (l1 == l2) 
    stop("The two competing models have identical log-likelihoods!")
  p1 <- obj1$df.fit
  p2 <- obj2$df.fit
  n <- obj1$N
  pobj1 <- predictAll(obj1)
  pobj2 <- predictAll(obj2)
  fname1 <- obj1$family[1]
  fname2 <- obj2$family[1]
  nopar1 <- eval(gamlss.family(fname1))$nopar
  nopar2 <- eval(gamlss.family(fname2))$nopar
  pdfName1 <- paste("d", fname1, sep = "")
  pdfName2 <- paste("d", fname2, sep = "")
  binomial1 <- fname1 %in% .gamlss.bi.list
  binomial2 <- fname2 %in% .gamlss.bi.list
  model1 <- deparse(substitute(obj1))
  model2 <- deparse(substitute(obj2))
  if (binomial1) {
    linc1 <- switch(nopar1, eval(call(pdfName1, pobj1$y, 
                                      bd = obj1$bd, mu = pobj1$mu, log = TRUE)), eval(call(pdfName1, 
                                                                                             pobj1$y, bd = obj1$bd, mu = pobj1$mu, sigma = pobj1$sigma, 
                                                                                             log = TRUE)), eval(call(pdfName1, pobj1$y, bd = obj1$bd, 
                                                                                                                     mu = pobj1$mu, sigma = pobj1$sigma, nu = pobj1$nu, 
                                                                                                                     log = TRUE)), eval(call(pdfName1, pobj1$y, bd = obj1$bd, 
                                                                                                                                             mu = pobj1$mu, sigma = pobj1$sigma, nu = pobj1$nu, 
                                                                                                                                             tau = pobj1$tau, log = TRUE)))
  }
  else {
    linc1 <- switch(nopar1, eval(call(pdfName1, pobj1$y, 
                                      mu = pobj1$mu, log = TRUE)), eval(call(pdfName1, 
                                                                             pobj1$y, mu = pobj1$mu, sigma = pobj1$sigma, log = TRUE)), 
                    eval(call(pdfName1, pobj1$y, mu = pobj1$mu, sigma = pobj1$sigma, 
                              nu = pobj1$nu, log = TRUE)), eval(call(pdfName1, 
                                                                     pobj1$y, mu = pobj1$mu, sigma = pobj1$sigma, 
                                                                     nu = pobj1$nu, tau = pobj1$tau, log = TRUE)))
  }
  if (binomial2) {
    linc2 <- switch(nopar1, eval(call(pdfName2, pobj2$y, 
                                      bd = obj2$bd, mu = pobj2$mu, log = TRUE)), eval(call(pdfName2, 
                                                                                             pobj2$y, bd = obj2$bd, mu = pobj2$mu, sigma = pobj2$sigma, 
                                                                                             log = TRUE)), eval(call(pdfName2, pobj2$y, bd = obj2$bd, 
                                                                                                                     mu = pobj2$mu, sigma = pobj2$sigma, nu = pobj2$nu, 
                                                                                                                     log = TRUE)), eval(call(pdfName2, pobj2$y, bd = obj2$bd, 
                                                                                                                                             mu = pobj2$mu, sigma = pobj2$sigma, nu = pobj2$nu, 
                                                                                                                                             tau = pobj2$tau, log = TRUE)))
  }
  else {
    linc2 <- switch(nopar2, eval(call(pdfName2, pobj2$y, 
                                      mu = pobj2$mu, log = TRUE)), eval(call(pdfName2, 
                                                                             pobj2$y, mu = pobj2$mu, sigma = pobj2$sigma, log = TRUE)), 
                    eval(call(pdfName2, pobj2$y, mu = pobj2$mu, sigma = pobj2$sigma, 
                              nu = pobj2$nu, log = TRUE)), eval(call(pdfName2, 
                                                                     pobj2$y, mu = pobj2$mu, sigma = pobj2$sigma, 
                                                                     nu = pobj2$nu, tau = pobj2$tau, log = TRUE)))
  }
  li12 <- linc1 - linc2
  w <- sqrt(var(li12) * (n - 1))
  vt <- (l1 - l2 - (p1 - p2)/2 * log(n))/w
  li12b <- li12 - (p1 - p2)/(2 * n) * log(n)
  b <- sum(li12b > 0)
  if (abs(vt) <= qnorm(1 - sig.lev/2)) 
    cat(" Vuong's test:", round(vt, 3), "it is not possible to discriminate between models:", 
        model1, "and", model2, "\n")
  if (vt > qnorm(1 - sig.lev/2)) 
    cat(" Vuong's test:", round(vt, 3), "model", model1, 
        "is preferred over", model2, "\n")
  if (vt < -qnorm(1 - sig.lev/2)) 
    cat(" Vuong's test:", round(vt, 3), "model", model2, 
        "is preferred over", model1, "\n")
  db <- paste(paste(paste("Clarke's test: it is not possible to discriminate between models:", 
                          model1), "and"), model2)
  if (b >= n/2) {
    pvalue <- 2 * (1 - pbinom(b - 1, n, 0.5))
    if (pvalue <= sig.lev) 
      cat("Clarke's test:", b, "p-value=", round(pvalue, 
                                                 4), model1, "is preferred over", model2, "\n")
    else cat("Clarke's test:", b, "p-value=", round(pvalue, 
                                                    4), "it is not possible to discriminate between models:", 
             model1, "and", model2, "\n")
  }
  if (b < n/2) {
    pvalue <- 2 * (pbinom(b, n, 0.5))
    if (pvalue <= sig.lev) 
      cat("Clarke's test:", b, "p-value=", round(pvalue, 
                                                 4), model2, "is preferred over", model1, "\n")
    else cat("Clarke's test:", b, "p-value=", round(pvalue, 
                                                    4), "it is not possible to discriminate between models:", 
             model1, "and", model2, "\n")
  }
}