saveRelationsGraph <- function(data)
{
    for(i in 1:length(names(data)))
    {
        for(j in i+1:length(names(data)))
        {
            path = paste("img/rel/a", toString(i), "-", toString(j), ".jpeg", sep="")
            x = names(data)[i]
            y = names(data)[j]
            title = paste(x, "vs", y)
            jpeg(path)
            plot(data[,i], data[,j], xlab=x, ylab=y, main=title)
            dev.off()
        }
    }
}