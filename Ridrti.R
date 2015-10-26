#
# Plot frequent organisms for this extract
#
common_organisms <- function(extract_dir){
  tests <- read.csv(sprintf("%s%s", extract_dir, "lab_test.csv"))
  organisms <- as.data.frame(table(tests$organism_details))
  names(organisms) <- c("Organism", "Frequency")
  
#   organisms <- organisms[organisms$Frequency > 3,]
  organisms <- organisms[organisms$Organism != "",]
  
  ggplot(organisms, aes(x=Organism, y=Frequency, fill=Organism)) + 
    geom_bar(stat="identity") +
    labs(title="Common Organisms") +
    guides(fill=FALSE) +
    coord_flip()
}