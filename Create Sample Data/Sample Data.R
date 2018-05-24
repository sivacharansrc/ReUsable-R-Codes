x <- data.frame(Logical = c(TRUE, NA, FALSE, TRUE, FALSE),
                character = c("Siva", NA, "Amrita", "Siva", "Divya"),
                numeric = c(1.1,2.3,4.5,2,4),
                integer = c(1,2,3,6,3),
                date = c(as.Date("2018-12-01"), as.Date("2017-12-01"), as.Date("2019-05-28"),
                         as.Date("2018-07-24"), as.Date("2019-05-28")),
                factor = c("Supply Chain", "Marketing", "Supply Chain", "Production", "Planning"))

df <- data.frame(Name = c("Rob", "Michelle", "Siva", "Rob", "Prasath"),
                 Age = c(25,54,12,67,182),
                 DOB = c(as.Date("1993-12-01"), as.Date("1964-12-01"), as.Date("2006-05-28"),
                         NA, as.Date("2000-05-28")),
                 TestScore = c(72,82.5,NA,79.86,86),
                 Residence = as.factor(c(NA,NA,NA, "USA", "India")), stringsAsFactors = F)



