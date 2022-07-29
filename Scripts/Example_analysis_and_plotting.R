##ANALYSIS-----
#Urban
brm.habpref.resurban2<-brm(Urban ~ residuals, data = dataformcmc10,
                           cores=4,
                           family = gaussian, cov_ranef = list("Species" = A10),
                           control = list(adapt_delta = 0.99,max_treedepth=15))

brm.habpref.resurban2
pp_check(brm.habpref.resurban,nsamples=1000)
bayes_R2(brm.habpref.resurban2)
icc(brm.habpref.brainurban, re.form = NULL, typical = "mean",
    prob = 0.89, ppd = FALSE, adjusted = FALSE)

marginal_effects(brm.habpref.resurban2)

#Forest

brm.habpref.resforest2<-brm(Forests ~ residuals, data = dataformcmc10,
                            cores=4,
                            family = gaussian, cov_ranef = list("Species" = A10),
                            control = list(adapt_delta = 0.99,max_treedepth=15))

brm.habpref.resforest2
pp_check(brm.habpref.resforest,nsamples=1000)
bayes_R2(brm.habpref.resforest2)
icc(brm.habpref.resforest, re.form = NULL, typical = "mean",
    prob = 0.89, ppd = FALSE, adjusted = FALSE)

marginal_effects(brm.habpref.resforest2)

#FIGURES----

plot(Forests ~ residuals,data = dataformcmc10, las=1, main="Forest preference related to brain sizes\n Brain weight - Body size residuals (a)", ylab="Forest preference", xlab="Brain weight - body size residuals")
fit<-marginal_effects(brm.habpref.resforest2)
fits<-as.data.frame(fit$residuals)
lines(fits$residuals, fits$lower__, col = "grey30", lwd = 2)
lines(fits$residuals, fits$upper__, col = "grey30", lwd = 2)
polygon(c(fits$residuals, rev(fits$residuals)), c(fits$upper__, rev(fits$lower__)),
        col = "grey80", border = NA)
lines(c(-0.6,0.6),c(1.04,1.04), col = "black")
lines(c(-0.5,0.99),c(-0.04,-0.04), col = "black")

points(Forests ~ residuals,data = dataformcmc10)
lines(fits$residuals, fits$estimate__, lwd=2, col = "darkgreen")




plot(Urban ~ residuals,data = dataformcmc10, ylab="Urban preference",las=1, main="Urban preference related to brain sizes\n Brain weight - body size residuals (b)", xlab="Brain weight - body size residuals")
fit<-marginal_effects(brm.habpref.resurban2)
fits<-as.data.frame(fit$residuals)
lines(fits$residuals, fits$lower__, col = "grey30", lwd = 2)
lines(fits$residuals, fits$upper__, col = "grey30", lwd = 2)
polygon(c(fits$residuals, rev(fits$residuals)), c(fits$upper__, rev(fits$lower__)),
        col = "grey80", border = NA)
points(Urban ~ residuals,data = dataformcmc10)
lines(fits$residuals, fits$estimate__, lwd=2, col = "Blue")
lines(c(-0.5,0.99),c(1.04,1.04), col = "black")
lines(c(-0.5,0.7),c(-0.04,-0.04), col = "black")
























