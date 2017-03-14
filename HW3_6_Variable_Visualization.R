featurePlot(x = data.train.normal[,-1],
            y = data.train.normal$default,
            plot = 'density',
            scales = list(x = list(relation="free"), 
                          y = list(relation="free")),
            layout = c(6,4))