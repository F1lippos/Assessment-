library("ProjectTemplate")
setwd("~/R/CSCFilProject")
load.project()

for (dataset in project.info$data)
{
  message(paste('Showing top 5 rows of', dataset))
  print(head(get(dataset)))
}

 

 summary(df3)
 summary(df2)
 
 
 