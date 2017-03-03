pathStr = '/Users/toumanabu/Box\ Sync/Data/170302_R/'
fileNameStr = '170302_Run_SVM_'
loopIndex = 1:10

for (i in loopIndex){
    fileName = paste0(pathStr, fileNameStr, as.character(i),'.data')
    con = file(fileName, 'rb')
    load(con)
    print(model.svm0)
    close(con)
 }


