try(setwd("jss"),silent=TRUE)
Sweave("ino_oelschlaeger_oetting_bauer.Rnw", encoding = "utf8")
tinytex::pdflatex("ino_oelschlaeger_oetting_bauer.tex")
Stangle("ino_oelschlaeger_oetting_bauer.Rnw", encoding = "utf8")
