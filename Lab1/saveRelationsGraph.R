saveRelationsGraph <- function(data)
{
    for(i in 1:length(names(data)))
    {
        dist = length(names(data)) - i
        for(j in 1:dist)
        {
            path = paste("img/rel/a", toString(i), "-", toString(j+i), ".jpeg", sep="")
            x = names(data)[i]
            y = names(data)[j+i]
            title = paste(x, "vs", y)
            jpeg(path)
            plot(data[,i], data[,j+i], xlab=x, ylab=y, main=title)
            dev.off()
        }
    }
}