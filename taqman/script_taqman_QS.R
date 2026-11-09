# Script til at omdanne Viia 7 output fil til pyro output format 
##### Dependencies and Treshholds #####
library(tidyr)
library(dplyr)
library(readr)
library(data.table)
library(stringr)
setwd("S:/Neonatal Genetik/3. NG Sample-Data/7a. Rutineanalyser/CF/data/Viia7_out")
# Cutoffs 
CRT.sample.fail <- 30
CRT.control.fail <- 35

#### definer blank linje til senere brug #MABH 09-04-2021
blanklinje <- ""

##### List Data og afled navngivning #####
# List filer der er analyseret
filename_result<-list.files (getwd(),pattern = "lts.txt$")
filename_sample<-list.files (getwd(),pattern = "up.txt$")

# Opret navn til Plade 1 output, L?ngden 13 er l?ngden p? k?rslens identifier
filename1 <- "1"
filename_plate_result <- substr(filename_result,1,13)
notename_plate1 <- paste(substr(filename_plate_result,1,13), substr(filename1,1,1), sep ="")  #mabh 23-03-2021 NAVN I NOTE SKAL V?RE UDEN .CSV
outputname_plate1 <- paste(substr(filename_plate_result,1,13), substr(filename1,1,1), ".csv", sep ="")

# Opret navn til Plade 2 output, L?ngden 13 er l?ngden p? k?rslens identifier
filename2 <- "2"
filename_plate_sample <-substr(filename_sample,1,13)
notename_plate2 <- paste(substr(filename_plate_result,1,13), substr(filename2,1,1), sep ="") #mabh 23-03-2021 NAVN I NOTE SKAL V?RE UDEN .CSV
outputname_plate2 <- paste(substr(filename_plate_result,1,13), substr(filename2,1,1), ".csv", sep ="")

# Opret tekstlinje som skal st? over tabellen i outputfilerne  #MABH 09-04-2021

linje_outpuplate1 <- paste("SNP Analysis Results Report, Run:", notename_plate1, sep=" ")
linje_outpuplate2 <- paste("SNP Analysis Results Report, Run:", notename_plate2, sep=" ")


##### possitioner 96 vs 384 format #####
# Importer link mellem f?rste 96 Br?nds plade position og ny 384 Br?nds plade position
well_format_plate1 <- read_delim("S:/Neonatal Genetik/3. NG Sample-Data/7a. Rutineanalyser/CF/Data/well_format_plate1.csv",
                                 ";", escape_double = FALSE, trim_ws = TRUE)

well_format_plate1<- well_format_plate1 %>%
                    rename (w=well)     %>%
                    rename (Well=well_384)
# Importer link mellem anden 96 Br?nds plade position og ny 384 Br?nds plade position
well_format_plate2 <- read_delim("S:/Neonatal Genetik/3. NG Sample-Data/7a. Rutineanalyser/CF/Data/well_format_plate2.csv", 
                                    ";", escape_double = FALSE, trim_ws = TRUE)
well_format_plate2<- well_format_plate2 %>%
                    rename (w=well)     %>%
                    rename (Well=well_384)

##### Valider sample og results #####
# Funktion til at tjekke at resultat filen og sample filen stammer fra samme k?rsel
tjek <- function(filename_plate_result,filename_plate_sample){
  if (!isTRUE(all.equal(filename_plate_result,filename_plate_sample))){
    mismatches <- paste(which(filename_plate_result != filename_plate_sample), collapse = ",")
    stop("error the A and B does not match at the following columns: ", mismatches )
  } else {
    message("alt er fint")
  }
}
# k?r funktion p? data
tjek(filename_plate_result,filename_plate_sample)

##### Load resultat og sample filer #####

## Import and manipulate Results
# Fread importerer alt efter linjen med [Results] hvor f?rste linje behandles som header, begr?nses til benyttet data
Result <- fread(filename_result, skip = "[Results]", header = T)%>%
  select(`Sample Name`, `Well Position`,`Allele1 Ct`,`Allele2 Ct`  ,`Quality(%)`, Call)
#Set undetermined to CRT==50, then determine minimum for filtering
Result$A1CRT <- ifelse(Result$`Allele1 Ct`=="Undetermined", 50, as.numeric(Result$`Allele1 Ct`))
Result$A2CRT <- ifelse(Result$`Allele2 Ct`=="Undetermined", 50, as.numeric(Result$`Allele2 Ct`))
Result <- Result %>% rowwise() %>% mutate(minCRT = min(`A1CRT`,`A2CRT`))

## Import Sample Setup 
# Fread importere alt efter linjen med [Sample Setup] hvor f?rste linje behandles som header, begr?nses til benyttet data
Sample_Setup <- fread(filename_sample, skip = "[Sample Setup]", sep = "\t", header = TRUE, na.strings = "NA") %>%  
  select(`Sample Name`, `Sample ID`) 
Sample_Setup[Sample_Setup==""]<- NA

# flet sample filen og resultatfilen
merge <- inner_join(Sample_Setup,Result, by="Sample Name", )  

##### Data manipulation #####

## fjern kontrollerne (ikke de blanke)

merge<- merge %>% 
  filter(str_detect(`Sample Name`,"^S"))

## Apply Cutoffs on samples and controls
merge$Call.filtered <- ifelse(merge$`Sample ID` == "Q1" & merge$minCRT >= CRT.control.fail, "Undetermined",
                              ifelse(merge$`Sample ID` == "Q1" & merge$minCRT <= CRT.control.fail,merge$Call,
                              ifelse(merge$minCRT <= CRT.sample.fail ,merge$Call,"Undetermined")))

# omdan genotypekald fra Viia 7 til pyro format
output1 <-mutate(merge, Genotype = ifelse (Call.filtered %in% "Homozygous CTT/CTT", "AAG/AAG", 
        ifelse (Call %in%"Heterozygous del508/CTT", "AAG/---",
        ifelse (Call%in% "Homozygous del508/del508", "-/-", "n.a.")))) 


# omd?b kvalitet af kald til pyro kvalitets nummenklatur
#transform(output1,`Quality(%)`=as.numeric(`Quality(%)`))
#(output1$`Quality(%)` >95, "Passed", "Failed")

output1$Quality <- ifelse(merge$`Sample ID` == "Q1" & merge$minCRT >= CRT.control.fail & output1$`Quality(%)` >95, "Passed",
                              ifelse(merge$`Sample ID` == "Q1" & merge$minCRT <= CRT.control.fail & output1$`Quality(%)` >95,"Failed",
                                     ifelse(merge$minCRT <= CRT.sample.fail ,"Passed","Failed")))


# tilf?j variable som findes i Pyrooutputtet, men ikke findes i Viia7 outputtet
output1<- output1 %>% mutate(Assay ="CF TaqMan", Note =" ", Position ="1", "Var. Pos" ="[AAG]", 
             Warnings=" ",`Quality Edited`="No",`Parameters Edited`="No")

#ret s? Note indeholder pladenavn ud fra 1 pr?ve i hver 96 plade
output1<- within(output1, Note[`Sample Name`=="S1"] <- notename_plate1)
output1<- within(output1, Note[`Sample Name`=="S97"] <- notename_plate2)

#fjern S i sample name og lav samplenummeret nummerisk         
output1 <-output1 %>% separate(`Sample Name`,sep="S",remove = FALSE, into = c("slet","sample"))
output1$slet <-NULL
output1<- output1 %>% transform(sample=as.numeric(sample))

# ret navne og genindf?r mellemrum

output1<-output1                              %>%
  rename (`Sample Name`=Sample.Name)        %>%
  rename (`Sample ID` =Sample.ID)           %>%
  rename (`Quality Edited`=Quality.Edited)  %>%
  rename (`Parameters Edited` =Parameters.Edited)%>%
  rename (`Var. Pos`=Var..Pos)                %>%
  rename (Well=`Well.Position`)

##### Export #####
## Lav outputpladerne (hvis der var to punchede plader, skal der v?re to outputplader)

# Filtrer plade1 og flet med format reference.
output_plate1 <- filter(output1,sample < 97) 
output_plate1_1<-  inner_join(output_plate1, well_format_plate1, by = "Well")                

# well er 384 format position, denne slettes af w der er 96 well pos, w omd?bes herefter
output_plate1_1$Well <-NULL            
output_plate1_1<-output_plate1_1 %>%
  rename (Well=w)

# v?lg og sorte de relevante variable og skriv til fil
output_plate1_1<- output_plate1_1[,c("Well","Assay","Sample ID","Note","Position","Var. Pos","Quality","Warnings", "Quality Edited", "Parameters Edited", "Genotype") ]
write.csv2(output_plate1_1, file =outputname_plate1, row.names=F,quote =FALSE, eol=";\n") #MABH 09-04-2021 tilf?jet eol=";\n"

# Tilf?j tekstlinje og blank linje til csv filen # MABH 09-04-2021

outputname_plate1 <- file(outputname_plate1)
csvindhold1 <- readLines(outputname_plate1)
writeLines(c(linje_outpuplate1, blanklinje, csvindhold1), outputname_plate1)

#plade2

# Filtrer plade2 og flet med format reference.
output_plate2 <- filter(output1,sample>96)
output_plate2_1<- inner_join(output_plate2, well_format_plate2, by = "Well")

# well er 384 format position, denne slettes af w der er 96 well pos, w omd?bes herefter
output_plate2_1$Well <-NULL            
output_plate2_1<-output_plate2_1 %>%
                rename (Well=w)
# v?lg og sorte de relevante variable og skriv til fil
output_plate2_1<- output_plate2_1[,c("Well","Assay","Sample ID","Note","Position","Var. Pos","Quality","Warnings", "Quality Edited", "Parameters Edited", "Genotype") ]
write.csv2(output_plate2_1, file =outputname_plate2, row.names=F,quote =FALSE, eol=";\n") # JOGR 9 Juli 2020, fejl i DF for output_plate2_1, hed 2_2 #MABH 09-04-2021 tilf?jet eol=";\n"

# Tilf?j tekstlinje og blank linje til csv filen # MABH 09-04-2021

outputname_plate2 <- file(outputname_plate2)
csvindhold2 <- readLines(outputname_plate2)
writeLines(c(linje_outpuplate2, blanklinje, csvindhold2), outputname_plate2)


shell("move *lts.txt .\\behandlede\\" )
shell("move *up.txt .\\behandlede\\" )
shell("move *1.csv .\\Screeningcenter_input\\" )
shell("move *2.csv .\\Screeningcenter_input\\" )

