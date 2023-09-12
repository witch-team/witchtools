#makes the iamc regional mapping to comon regions for WITCH yml file
#as in https://github.com/IAMconsortium/common-definitions/blob/main/mappings/REMIND_3.1.yml

#created by Lara on 12-09-2023

#use:
# model is the model version
# regdef which region definition. THE REGION MUST BE DEFINED IN WTCHTOOLS.


make_reg_yml <- function(model='WITCH 5.0',regdef='witch17'){
  
  library(stringr)
  library("stringi")
  library(yaml)
  library(foreach)
  library(rlist)  
  model='WITCH 5.0'
  regdef='witch17'
  
  #get the region descrition match to the n WITCH regions
  datr=merge(eval(parse(text=paste0("witchtools::region_descriptions$", regdef))),
             eval(parse(text=paste0("witchtools::region_mappings$", regdef))),
             by=regdef)
  
  #create concatenated column for the reporting format
  datr$descriptionmr = paste0(model,'|',datr$description) 
  
  #make the list structures (based on the common-definitions/definitions/region/native_regions/REMIND_3.1.yml)
  m <- unique(datr$witch17)
  ii=foreach(i=1:length(m)) %do% {
       oo=list(unique(datr[witch17 %in% m[i]]$descriptionmr))
       names(oo) = m[i]
       return(oo)
  }
  
  
  creg=list(
    World = unique(datr$witch17),
    `Asia (R5)` = c('china','india','indonesia','sasia','seasia'),
    `Latin America (R5)` = c('brazil','mexico','laca'),
    `Middle East & Africa (R5)`=c('mena','southafrica','ssa'),
    `OECD & EU (R5)`=c('canada','europe','jpnkor','oceania','usa'),
    `Reforming Economies (R5)` = c('te'),
    `Africa (R10)`=c('ssa','southafrica'),
    `China+ (R10)`=c('china'),
    `Europe (R10)`=c('europe'),
    `India+ (R10)`=c('india'),
    `Latin America (R10)`=c('laca','brazil','mexico'),
    `Middle East (R10)`=c('mena'),
    `North America (R10)`=c('usa','canada'),
    `Reforming Economies (R10)`=c('te'),
    `Rest of Asia (R10)`=c('sasia','seasia','jpnkor'),
    `Pacific OECD (R10)`=c('oceaina'),
    `Other (R10)`=c('indonesia')
  )
  

  ragreg=list(list(model=list(model)),
       list(native_regions=ii),
       list(common_regions=creg))
  
   la=as.yaml(ragreg)
  
  
  #write file
  write(la, paste0(str_replace(model,' ','_'),'.yml'))
  
  return(0)
  
}

