# https://gist.github.com/richardbeare/b679c38dcb644ec50ea34ac061200504



predictdf.gam <- function(model, xseq, se, level) {
  olddata <- model.frame(model)
  if (is.null(olddata$randomid)) {
    newdata= tibble(x=xseq)
  } else {
    newdata = tibble(x=xseq, randomid=olddata$randomid[1])
  }
  pred <- predict(model, exclude="s(randomid)", newdata = newdata,
                  se.fit = se, level = level, interval = if (se)
                    "confidence"
                  else "none")
  if (se) {
    y = pred$fit
    ci <- pred$se.fit * 1.96
    ymin = y - ci
    ymax = y + ci
    tibble(x = xseq, y, ymin, ymax, se = pred$se.fit)
  }
  else {
    tibble(x = xseq, y = as.vector(pred))
  }
  
}
environment(predictdf.gam) <- environment(ggplot2:::predictdf.glm)