todaydate <- '170307'
model <- 'SVM1'
# pathStr = paste0('/Users/toumanabu/Box\ Sync/Data/', todaydate,
#                  '_R_', model, '/')
pathStr = paste0('/Users/dongshi/Box\ Sync/Data/', todaydate,
                 '_R_', model, '/')
fileNameStr = paste0(todaydate, '_Run_',
                     model, '_')
loopIndex = -1:7

for (i in loopIndex){
  fileName = paste0(pathStr, fileNameStr, as.character(i),'.data')
  con = file(fileName, 'rb')
  load(con)
  if (i==-1) all.data <- model.svm1$results
  else all.data <- rbind(all.data, model.svm1$results[-1,])
  close(con)
}

library(ggplot2)
p <- ggplot(all.data, aes(x = C, y = Accuracy)) + 
     coord_trans(x = 'log10') +
     geom_point() +
     scale_x_continuous(breaks = 10^(-1:7)) +
     ylim(0.6,1) +
     theme(plot.title = element_text(size = 20, hjust = 0.5)) +
     ggtitle('SVM with radial function basis')
print(p)


