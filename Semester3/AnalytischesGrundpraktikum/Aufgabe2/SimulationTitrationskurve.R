library(titrationCurves)

pk_w = 14

pka_1 = 6.35
pka_2 = 10.33

conc_base = 0.04508 # in mol/l
conc_acid = 0.05 # in mol/l
titer_acid = 0.9799

volume_base = 10 # in ml
volume_burette = 25 # in ml

titration_Curve = diwb_sa(conc.base = conc_base, conc.acid = conc_acid * titer_acid, pka1 = pka_1, pka2 = pka_2, pkw = pk_w,  vol.base = volume_base, overlay = TRUE)
titration_Curve_deriv = derivative(titration_Curve)
deriv = titration_Curve_deriv$first_deriv
deriv$y1 = - deriv$y1

intervall_1 = deriv$y1[1:(length(deriv$y1)/2)]
pos_eqp_1 = which(deriv$y1 == max(intervall_1))
eqp_1 = deriv$x1[[pos_eqp_1]]

intervall_2 = deriv$y1[(length(deriv$y1)/2):length(deriv$y1)]
pos_eqp_2 = which(deriv$y1 == max(intervall_2))
eqp_2 = deriv$x1[[pos_eqp_2]]
  
plot(titration_Curve, , xaxs = "i", xlim = c(0, volume_burette), yaxs = "i", ylim = c(0, 14), panel.first = grid(), type = "l", col = "red", lwd = 2, main="Acidimetrische Titration von Carbonat", xlab = "V(HCl) in ml", ylab = "pH")
lines(deriv, type = "l", col = "blue", lwd = 1)
abline(v = eqp_2, col = "red", lty = 2)
abline(v = eqp_1, col = "red", lty = 2)

cat("Äquivalenzpunkt 1 bei ", eqp_1, " ml")
cat("Äquivalenzpunkt 2 bei ", eqp_2, " ml")
