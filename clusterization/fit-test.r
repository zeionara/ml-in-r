clusters = kmeans(x=read.csv('/home/dima/ml-in-r/clusterization/data.csv', header=T, sep=',')[,c('x','y')], iter.max=100, centers=3)
clusters