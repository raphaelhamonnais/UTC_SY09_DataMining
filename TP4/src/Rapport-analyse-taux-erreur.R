############## Analyse taux erreur fichiers synthétiques

synt = NULL
synt_log = NULL

synt[[1]] = c(3.43 , 4.52 , 4.22 , 3.67 , 3.25 , 4.73)
synt[[2]] = c(6.23 , 8 , 6.17 , 6.85 , 6.44 , 8.09)
synt[[3]] = c(4.23 , 4.16 , 5.45 , 4.2 , 4.37 , 5.93)


for (i in 1:3) {
    cat("synt", i, "\n")
    cat("mean", mean(synt[[i]]), "\n")
    cat("var", var(synt[[i]]), "\n")
    
    
    
    synt_ad = synt[[i]][1:3]
    synt_reg_log = synt[[i]][4:5]
    cat("mean synt_analyse_dis", round(mean(synt_ad), 2), "\n")
    cat("var synt_analyse_dis", round(var(synt_ad),2), "\n")
    cat("mean synt_reg_log", round(mean(synt_reg_log), 2), "\n")
    cat("var synt_reg_log", round(var(synt_reg_log),2), "\n")
    
    
    synt_lineaire = c(synt[[i]][2], synt[[i]][4])
    synt_quadratique = c(synt[[i]][1], synt[[i]][3], synt[[i]][5])
    cat("mean synt_lineaire", round(mean(synt_lineaire), 2), "\n")
    cat("var synt_lineaire", round(var(synt_lineaire),2), "\n")
    cat("mean synt_quadratique", round(mean(synt_quadratique), 2), "\n")
    cat("var synt_quadratique", round(var(synt_quadratique),2), "\n")
    
    writeLines("")
}
mean(synt[[1]])