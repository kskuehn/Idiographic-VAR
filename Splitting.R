## Load Stacked Data
data<-read.csv("~/full.random.R")

## Split stacked data into person-specific files (saves to global environment)
pdat_split <- split(data, data$ID)

new_names<-as.character(c(unique(data$ID)))

for(i in seq_along(pdat_split)){
  new_names[i] <- paste0("pdat", i, sep="")
}

for (i in 1:length(pdat_split)) {
  assign(new_names[i], pdat_split[[i]])
}

write.csv(pdat2, file="pdat2.csv")