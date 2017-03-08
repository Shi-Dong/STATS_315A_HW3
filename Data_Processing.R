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
    print(model.svm1)
    close(con)
}




