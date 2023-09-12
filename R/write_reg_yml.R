#makes the iamc region/native_regions WITCH yml file
#as in https://github.com/IAMconsortium/common-definitions/blob/main/definitions/region/native_regions/REMIND_3.1.yml

#created by Lara on 12-09-2023

#use:
# model is the model version
# regdef which region definition. THE REGION MUST BE DEFINED IN WTCHTOOLS.

make_reg_yml <- function(model='WITCH 5.0',regdef='witch17'){

library(stringr)
library("stringi")
library(yaml)
library(foreach)

model='WITCH 5.0'
regdef='witch17'

#get the region descrition match to the n WITCH regions
datr=merge(eval(parse(text=paste0("witchtools::region_descriptions$", regdef))),
           eval(parse(text=paste0("witchtools::region_mappings$", regdef))),
           by=regdef)

#make the list structures (based on the common-definitions/definitions/region/native_regions/REMIND_3.1.yml)
m <- unique(datr$description)
ii=foreach(i=1:length(m)) %do% {
  oo=list(desp=list(
    iso3_codes=datr[description %in% m[i]]$iso3
  ))
  
  names(oo) = paste0(model,'|',m[i])
  return(oo)
}

#add the primary ident structure
x =list(
  list(model=ii
  )
)

names(x[[1]]) = model

la=as.yaml(x)

#transform into yml file similar to
#common-definitions/definitions/region/native_regions/REMIND_3.1.yml may not be needed
#depending on the script to read it

la1=str_replace_all(la,'\n      - ',', ')
la2=str_replace_all(la1,'iso3_codes: ','iso3_codes: [')
la2=str_replace_all(la2,'iso3_codes:, ','iso3_codes: [')
la3=stri_replace_last(la2, fixed = "\n", "]")  
la4=str_replace_all(la3,"\n  - ","\n$  - ") 
la5=str_replace_all(la4,"\n[$]","]\n") 
la5=str_replace_all(la5,"- WITCH 5.0:]","- WITCH 5.0:") 

#write file
write(la5, paste0(str_replace(model,' ','_'),'.yml'))

return(0)

}

