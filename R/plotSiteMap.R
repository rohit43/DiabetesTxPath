#Function to plot heat maps
plotSiteMap <- function(){
  file <- c("sites_Hb1c.csv")
  for(i in 1:length(file)){
    dat <- read.csv(file[i],header = TRUE, stringsAsFactors = FALSE)
    dat$drugComb <- paste(dat$Treatment,dat$Comparator,sep="-")
    dat <- subset(dat,select=c(drugComb,MtSinai,UTH))
    dat$drugComb <- factor(dat$drugComb,levels = rev(dat$drugComb))
    dat <- melt(dat)

    ggplot(data = dat, aes(x = variable, y = drugComb)) + geom_tile(aes(fill = value)) + scale_fill_viridis(option = "inferno",name="# Events", na.value = "white")
    #ggplot(data = dat, aes(x = variable, y = drugComb)) + geom_tile(aes(fill = value)) + scale_fill_viridis(discrete = TRUE,name="# Events", na.value = "white")

  }



}
