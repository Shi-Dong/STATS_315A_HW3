pathStr = '/Users/toumanabu/Box\ Sync/Data/170303_R_SVM2/'
fileNameStr = '170303_Run_SVM2_'
loopIndex = -1:7

for (i in loopIndex){
    fileName = paste0(pathStr, fileNameStr, as.character(i),'.data')
    con = file(fileName, 'rb')
    load(con)
    print(model.svm2)
    close(con)
 }


