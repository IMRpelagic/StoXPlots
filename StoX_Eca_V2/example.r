
#-------------------------------------------------------------------------------#
# Remove everything from memory to avoide conflicts between runs
#-------------------------------------------------------------------------------#
rm(list=ls())


#-------------------------------------------------------------------------------#
# Set the working directory to location of the script. 
#-------------------------------------------------------------------------------#
setwd(dirname(rstudioapi::getSourceEditorContext()$path))


source('ECA_utils.r')

path <- "E:/Arbeid/Bestander/2021/Nordsjosild"


strata = read.csv('mainarea_fdir_from_2018_incl.txt',sep = '\t')
strata2 <-readStrataPolygons('C:/Users/sindrev/workspace/stox/reference/stratum/mainarea.txt')


#To avoid stuff to be remembered
tmp_baseline <- NULL



#Grab data from stox project
tmp_baseline <- Rstox::getBaseline(path)
closeProject(path)


# #grab data
file <- tmp_baseline$outputData$FilterLanding
biotic <- tmp_baseline$outputData$FilterBiotic


#plot age distribution per quarter and area
BioticCovData<-tmp_baseline$outputData$BioticCovData


#Total catch, check if this match the official report
sum(file$rundvekt)/1000


#Plot age distribution per estimation cell
ageDistribution(BioticCovData)


#Plot catch on map
CatchMap(file=file,strata = strata,strata2 = strata2,biotic=biotic)

#plot Catch development
CatchDevelopment(biotic=biotic,file = file)


#User input
minage = min(biotic$FilterBiotic_BioticData_individual.txt$age)
maxage <- max(biotic$FilterBiotic_BioticData_individual.txt$age)
plusage = 9
species='Nordsjosild'


#Run ECA
prepareRECA(path,maxage = maxage,minage = minage)
runRECA(path)


# load intermediate calculations to determine which locations are in the dta
l <- loadProjectData(path, var="prepareRECA")
ldata <- l$prepareRECA$StoxExport$landing
locationcodes <- sprintf("%02d-%02d", ldata[,"hovedomr\u00e5dekode"], ldata[,"lokasjonkode"])



customlocation <- convertToICESarea(file)
if(species=='Nordsjosild')customlocation <- convertToICESareaHerring(file)


#save outputs
saveDecomposedCatchMatrix(projectName = path,filename = paste0(path,'/output/r/report/caa_area.csv'),
                          addQuarterToDecomp = T, plusgr=plusage,customLocationGrouping = customlocation, decomposition = NULL)
saveDecomposedAgeGroupParameters(path, paste0(path,'/output/r/report/caa_area_wl.csv'), addQuarterToDecomp = T, 
                                 plusgr=plusage,customLocationGrouping = customlocation, decomposition = NULL)

saveDecomposedCatchAtLength(projectName = path,addQuarterToDecomp = T,customLocationGrouping = customlocation,decomposition = NULL,filename = paste0(path,'/output/r/report/length_per_A_Q.csv'))

saveDecomposedCatchAtLength(projectName = path,addQuarterToDecomp = T,customLocationGrouping = NULL,decomposition = NULL,filename = paste0(path,'/output/r/report/length_per_Q.csv'))


