library(ggplot2)
library(dplyr)
library(paletteer)

#Dataen. Skal ændres til dataens placering
df <- read.table("C:\\Users/jkbml/Desktop/transit vis/PKU_Transit_table.txt",header = TRUE,sep="\t")

måneder <- unique(df$month) #De mulige måneder

#Hiver de relevante data ud og sætter det op i et brugbart data.frame
func <- function(måned=måneder,dage=4,grønland=FALSE,sortering="1",antal=FALSE){
    
    if(!all(måned %in% måneder)){               #Tjekker om måned inputtet er muligt
        return("Ugyldig måned")
    }
    
    df <- if(!grønland) df[df$Region!="Greenland/Færøerne",] else df #fjerner grønland og færørerne
    df <- df[df$month %in% måned,]              #fjerner for de måneder der ikke er i input
    
    dage <- 1:dage                              #de rejsetider der kigges på
    datalist <- list()
    
    dfLoc <- split(df,df$Location)              #splitter dataframen op efter rekvirent
    
    for(frame in dfLoc){                        #loop over rekvirenterne
        data <- frame$Time.diff.days            #vector med alle rejsetiderne i dage
        x <- if(!antal) length(data) else 1     #hvis der ønskes procentvis data sættes x til antal af prøver ellers 1
            for(num in dage){                   #loop over de rejsetider der kigges på. datan samles i en list som tilføjes til en større liste
                result <- list("Rekvirent"=unique(frame$Location),"Dage"=toString(num),"percent"=sum(data==num)/x)
                datalist[[length(datalist)+1]] <- result
            }
                                                #for alle rejsertider over den givne mængde
            result <- list("Rekvirent"=unique(frame$Location),"Dage"=paste0(">",toString(tail(dage,1))),"percent"=sum(data>tail(dage,1))/x)
            datalist[[length(datalist)+1]] <- result
    }
    outcome <- do.call(rbind,datalist)          #listen af lists samles til et dataframe
    outcome <- as.data.frame(outcome)
    outcome$percent <- unlist(outcome$percent)
    outcome$Rekvirent <- unlist(outcome$Rekvirent)
    outcome$Dage <- unlist(outcome$Dage)
    
                                                #dataen rangeres
    key <- tapply(outcome$percent[outcome$Dage %in% sortering],
                  outcome$Rekvirent[outcome$Dage %in% sortering],
                  sum)
    outcome <- outcome[order(key[as.character(outcome$Rekvirent)]), ]
    
                                                #outcome er output dataen. antal er boolean om den er procentvis eller ej
    return(list("outcome"=outcome,"antal"=antal))
}


plot <- function(outcome){
    
    #dataen og boolean om den er procentvis eller ej
    dataframe <- outcome$outcome
    antal <- outcome$antal
    
    #tilføjer ny kolonne til labels på barene. NA hvis talene er for små til at der kan stå tekst
    dataframe <- if(antal) transform(dataframe,percent2=ifelse(percent>max(percent)/20,percent,NA)) else transform(dataframe,percent2=ifelse(percent>0.04,paste0(sprintf("%1.1f", percent*100),"%"),NA))
    
    #laver koloner til factors så de kommer i den rigtige rækkefølge
    dataframe$Dage <- factor(dataframe$Dage,levels = unique(dataframe$Dage))
    dataframe$Rekvirent <- factor(dataframe$Rekvirent, levels = unique(dataframe$Rekvirent))
    
    #labels til enden af barene hvis dataen ikke er procentvis
    totals <- if(antal) aggregate(percent ~ Rekvirent, data = dataframe, FUN = sum) else ""
    totals$label <- if (antal) totals$percent else ""
    
    ggplot(data=dataframe, aes(fill=Dage, x=Rekvirent, y=percent,label = percent2)) +
        #om grafen er percent stacked eller bare stacked
        {if(antal)geom_bar(position=position_stack(reverse=TRUE),stat="identity")} +
        {if(!antal)geom_bar(position="fill",stat="identity")} +
        {if(antal) scale_y_continuous(expand = expansion(mult = c(0, 0.1)))} +
        
        #farve og legend på barene
        scale_fill_manual(labels = unique(dataframe$Dage),
                          values = c(rev(paletteer_c("ggthemes::Red-Green-Gold Diverging", length(unique(dataframe$Dage))-1)),"black")) +
        
        #flipper x & y og vender y aksen
        coord_flip() +
        {if(!antal)scale_y_reverse()} +
        
        #legend
        scale_colour_continuous(guide = guide_legend(direction = "horizontal", title.position = "top",
                                                     label.position="bottom", label.hjust = 0.5, label.vjust = 0.5)) +
        labs(y="%",fill="Antal dage") +
        
        #teksten på barene
        geom_text(position=position_stack(vjust=0.5,reverse=antal),
                  colour="white",
                  fontface = "bold", 
                  size =5) +
        
        #teksten for enden af barene
        {if(antal)geom_text(data = totals,
                  aes(x = Rekvirent, y = percent, label = label, fill = NULL),
                  inherit.aes = FALSE,
                  hjust = if (antal) -0.2 else 1.2,
                  colour = "black", fontface = "bold", size = 5)} +
        
        #diverse
        theme(axis.text=element_text(size=16),
              axis.title=element_text(size=20,face="bold"),
              legend.title = element_text(size=18),
              legend.text = element_text(size=16),
              legend.direction = "horizontal",
              legend.title.position = "top",
              legend.position = "top","left",
              legend.byrow = TRUE)
}