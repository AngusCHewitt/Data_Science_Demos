Capacity <-
  matrix(c(363, 168, 79, 226),
         nrow = 2,
         dimnames = list("Capacity 0-3 Months Prior" = c("Some", "None"),
                         "Capacity 24-27 Months Post" = c("Some", "None")))
Capacity

round(prop.table(Capacity,1),2)

mcnemar.test(Capacity)


Opioids <-
  matrix(c(363, 168, 79, 226),
         nrow = 2,
         dimnames = list("Opioids 0-3 Months Prior" = c("Some", "None"),
                         "Opioids 24-27 Months Post" = c("Some", "None")))
Opioids

round(prop.table(Opioids,1),2)

mcnemar.test(Opioids)
