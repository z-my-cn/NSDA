# 第 4 章 网络的描述统计

## 准备工作 --------------------------------------------------------------------

# 加载相应R包
# 若没有，先用install.packages()命令安装
library(igraph)
library(igraphdata)
library(dplyr)
library(ergm)

## 网络密度 --------------------------------------------------------------------

# 读取边数据
largest_edge <- read.csv('./Data/edge_largest.csv')
# 由边数据构造无向网络
g <- graph_from_data_frame(largest_edge, directed = FALSE)

# 网络的节点数和边数
c(vcount(g), ecount(g))

# 网络密度
edge_density(g)

## 度分布直方图 ----------------------------------------------------------------

# 导入数据
data("karate")

# 网络的节点数、边数
c(vcount(karate), ecount(karate))

# 网络密度
edge_density(karate)

# 节点的度
d <- degree(karate)
# 度的最小值和最大值
c(min(d), max(d))

# 绘制度分布直方图
hist(d, xlab = "度", ylab = "频数", main = "")
