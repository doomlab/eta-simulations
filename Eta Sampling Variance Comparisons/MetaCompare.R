
meta_large_nobias <- read.csv("~/John M.S. Experimental Psychology/Research and Articles/Statistics/ES Variance Comparison Project/meta_large_nobias.csv")
meta_large_pubbias <- read.csv("~/John M.S. Experimental Psychology/Research and Articles/Statistics/ES Variance Comparison Project/meta_large_pubbias.csv")
meta_medium_nobias <- read.csv("~/John M.S. Experimental Psychology/Research and Articles/Statistics/ES Variance Comparison Project/meta_medium_nobias.csv")
meta_medium_pubbias <- read.csv("~/John M.S. Experimental Psychology/Research and Articles/Statistics/ES Variance Comparison Project/meta_medium_pubbias.csv")
meta_small_nobias <- read.csv("~/John M.S. Experimental Psychology/Research and Articles/Statistics/ES Variance Comparison Project/meta_small_nobias.csv")
meta_small_pubbias <- read.csv("~/John M.S. Experimental Psychology/Research and Articles/Statistics/ES Variance Comparison Project/meta_small_pubbias.csv")


library(reshape)

long_small_nobias = melt(meta_small_nobias,
                         id = c("X", "sim"))
tapply(long_small_nobias$value, list(long_small_nobias$variable), mean, na.rm = T)
tapply(long_small_nobias$value, list(long_small_nobias$variable), sd, na.rm = T)


long_small_pubbias = melt(meta_small_pubbias,
                         id = c("X", "sim"))
tapply(long_small_pubbias$value, list(long_small_pubbias$variable), mean, na.rm = T)
tapply(long_small_pubbias$value, list(long_small_pubbias$variable), sd, na.rm = T)


long_medium_nobias = melt(meta_medium_nobias,
                         id = c("X", "sim"))
tapply(long_medium_nobias$value, list(long_medium_nobias$variable), mean, na.rm = T)
tapply(long_medium_nobias$value, list(long_medium_nobias$variable), sd, na.rm = T)


long_medium_pubbias = melt(meta_medium_pubbias,
                          id = c("X", "sim"))
tapply(long_medium_pubbias$value, list(long_medium_pubbias$variable), mean, na.rm = T)
tapply(long_medium_pubbias$value, list(long_medium_pubbias$variable), sd, na.rm = T)


long_large_nobias = melt(meta_large_nobias,
                          id = c("X", "sim"))
tapply(long_large_nobias$value, list(long_large_nobias$variable), mean, na.rm = T)
tapply(long_large_nobias$value, list(long_large_nobias$variable), sd, na.rm = T)


long_large_pubbias = melt(meta_large_pubbias,
                         id = c("X", "sim"))
tapply(long_large_pubbias$value, list(long_large_pubbias$variable), mean, na.rm = T)
tapply(long_large_pubbias$value, list(long_large_pubbias$variable), sd, na.rm = T)

