get_param <- function(file_name,attribute_number){
  # renvoie la valeur des paramètres pour les noms de fichier de sortie de l'AnaWG (version à jour au 11/02/2022)
  regex <- ".*m([0-9]+)d([0-9]+)L([0-9]+).*cal([0-9].?[0-9]?)_TX([0-9].?[0-9]?)meth([0-9])-([0-9]*)-([^.]+)[.]Rdat"   #regex pattern for the file name
  att <- gsub(regex,paste("\\",attribute_number,sep=""),file_name) #retrieve attribute
  if (!grepl("\\D", att)) {return(as.numeric(att))}               #if the attribute obtained is a number, return it as a number and not a string
  else {return(att)}                                              # else return the string
}
