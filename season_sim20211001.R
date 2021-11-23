## # Weather generator using analogues
## # Simule des evenements: quelques jours par annee, et plusieurs annees
## # Demande d'avoir calcule les analogues sur SLP avec CASTf90
## # Pascal Yiou, LSCE, Dec. 2012, Mar. 2016
## # Modif Fevrier 2013, Avril 2013, Mai 2013 (parallelisation),
## Juin 2013 (Pareto), Avril 2014 (changement des options),
## Mars 2015 (correction d'un bug dans les declarations globales
## Juillet 2015: possibilite de perturber les parametres alpha1 et alpha2
## Ce code est fourni "as is" sans garantie de bon fonctionnement
##
## # Se lance en batch sur 12 processeurs par:
## qsub -l nodes=1:ppn=12 -q mediump /home/users/yiou/RStat/WEGE/simu_anaWEGE_E3P.sh
## Modification pour changer les options par defaut du batch
## Ce code sert pour le papier d'intercomparaison avec les WG d'Imperial College
## et EDF R&D

SI=Sys.info()
## Il faut adapter les chemins en fonction de la machine et de l'utilisateur
if(SI[[1]] == "Darwin"){
  Rsource="/Users/yiou/programmes/RStats/"
  NCEPdir="/Users/yiou/data/NCEP/"
  OUTdir=DATdir
  OUTdir="/Users/yiou/data/NCEP/"
}
if(SI[[1]] == "Linux"){
  Rsource <- "/home/users/ccadiou/Code/"
  NCEPdir <- "/home/users/ccadiou/Data/WEGE/Ana" # Pour la lecture des analogues
  ECAdir <-  "/home/users/ccadiou/Data/ERA5" # Pour la lecture des donnees de temperature
  OUTdir <- "/home/estimr3/ccadiou/WEGE/" # Pour les sorties de resultats
}

## Lecture des arguments d'entree
args=(commandArgs(TRUE))
print(args)
if(length(args)>0){
  fanalogs=args[1] # Analogues generaux
  fanalogsim=args[2] # Analogues de la cible dans les generaux
  Nd=as.numeric(args[3]) # Nombre de jours a simuler
  nsim=as.numeric(args[4]) # Nombre de simulations de Nd jours
  fout=args[5] # fichier de sortie
}

source(paste(Rsource,"WEGE/init_WG_seas_20210929.R",sep=""))
source(paste(Rsource,"WEGE/diag_WG.R",sep=""))

#-------------------------------------------------------------------------------
# Initialisations generales
## L'initialisation lit les fichiers d'analogues
#-------------------------------------------------------------------------------
setwd(paste(Rsource,"WEGE",sep=""))
if(!exists("fanalogs")) fanalogs=""
if(!exists("fanalogsim")) fanalogsim=""
if(!exists("season")) season="WINTER"
if(!exists("Nd")) Nd=90
if(!exists("nsim")) nsim=1000
if(!exists("fout")) fout=paste(OUTdir,"aWEGE_dyn",season,".Rdata",sep="")

init.awege(fanalogs=fanalogs,fanalogsim=fanalogsim)
#-------------------------------------------------------------------------------
#  Fin des Initialisations generales
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
# ------------ DEBUT DE LA SIMULATION
## La simulation se deroule en trois etapes:
## Simulation de dates
## calcul des temperatures analogues
## Simulations de GPD pour les depassements de seuil
#-------------------------------------------------------------------------------
##idyn=TRUE # simulations dynamiques ou statiques: idyn=TRUE or FALSE
nproc=3
if(SI[[4]] %in% paste("obelix",1:4,sep="")) nproc=2 # Pour les machines interactives
if(SI[[4]] %in% paste("obelix",5:50,sep="")){
  nproc=  as.numeric(Sys.getenv(c("NCPU"))) # Pour les machines BATCH
}

print(paste(detectCores(),"cores detected"))
ncpus = as.numeric(Sys.getenv(c("NCPU")))
ncpus = max(nproc,ncpus,na.rm=TRUE)
print(paste("Calcul sur",ncpus,"CPUs"))


print(paste(nsim,"simulations"))
r.year <<- c(1900:1950)
Nd=31
Nday <<- Nd
XX.wg=simu.awege.dates.dyn(seas="WINTER",Nd=90,alpha1=0.5,alpha2=4,nsim=nsim,
  npro=nproc,istoch=FALSE)


#-------------------------------------------------------------------------------
# ------------ FIN DE LA SIMULATION 
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
# Sauvegarde
#-------------------------------------------------------------------------------
setwd(OUTdir)
save(file=fout,XX.wg,season)

q("no")
