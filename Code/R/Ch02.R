# 第 2 章 认识网络结构数据

## 准备工作 -------------------------------------------------------------------

# 加载相应R包
# 若没有，先用install.packages()命令安装
library(igraph)

## 使用igraph包创建网络 --------------------------------------------------------

# 创建空网络
g <- make_empty_graph()
# 查看网络
g

# 将画布分成一行两列
par(mfrow=c(1,2))
# 设置随机种子
set.seed(42)

# 添加6个节点
g <- add_vertices(g, 6)
# 查看网络
g

# 绘制网络结构图
plot(g, main="添加节点")

# 添加连边
g <- add_edges(g, c(1,2, 2,3, 1,3, 3,4, 4,5, 1,5, 3,6, 2,6, 5,6))
# 查看网络
g

# 绘制网络结构图
plot(g, main="添加连边")

## 利用边数据构建（有向、无向）网络 --------------------------------------------

# 导入空手道俱乐部边数据
data <- read.table("./Data/karate.txt", head = FALSE)

# 设置随机种子
set.seed(42)
# 将画布分成一行两列
par(mfrow = c(1,2))

# 创建无向网络
g1 <- graph_from_edgelist(as.matrix(data), directed = FALSE)
# 查看网络
g1

# 绘制网络结构图
plot(g1, main = "无向网络图")

# 创建有向网络
g2 <- graph_from_data_frame(data, directed = TRUE)
# 查看网络
g2


# 绘制网络结构图
plot(g2, main = "有向网络图")


## 利用邻接矩阵构建（有向、无向）网络 ------------------------------------------

# 创建边数据
a <- data.frame(1:10, c(2,3,4,7,3,2,1,9,7,6))
# 根据边数据构造网络
g_a <- graph_from_data_frame(a, directed = FALSE)
# 将网络转换为邻接矩阵
adj <- as_adjacency_matrix(g_a, sparse = FALSE)
# 输出邻接矩阵
adj

# 设置随机种子
set.seed(42)
# 将画布分成一行两列
par(mfrow = c(1,2))
# 构建网络，默认为有向网络
g3 <- graph_from_adjacency_matrix(adj)
# 查看网络
g3

# 绘制网络结构图
plot(g3, main = "有向网络图")

# 构建无向网络
g4 <- graph_from_adjacency_matrix(adj, mode = "undirected")
# 查看网络
g4

# 绘制网络结构图
plot(g4, main = "无向网络图")
