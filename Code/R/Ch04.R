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

## 双对数度分布图 --------------------------------------------------------------

# 设置画布
par(mfrow = c(1, 2))
# 读取边数据
edge_core <- read.csv('./Data/edge_core.csv')
# 由边数据构造无向网络
g <- graph_from_data_frame(edge_core, directed = FALSE)

# 绘制度分布直方图
hist(degree(g), xlab = "度", ylab = "频数", main = " ")

# 绘制双对数度分布图
# 计数
data_freq <- data.frame(table(degree(g)))
data_freq$Var1 <- as.numeric(as.character(data_freq$Var1))
# 绘图
plot(
    log(as.numeric(data_freq$Var1)), log(data_freq$Freq),
    xlab = "对数-度", ylab = "对数-作者数"
)

## 二元结构 --------------------------------------------------------------------

# 导入数据
data("UKfaculty")
# 利用dyad.census()函数计算三种二元结构的数量
# dyad.census(UKfaculty)
dyad_census(UKfaculty)

# 利用邻接矩阵计算三种二元结构的数量
# 获得邻接矩阵
A <- as_adjacency_matrix(UKfaculty)
# 将"dgCMatrix"类型转化为"matrix"类型
A <- as.matrix(A)

# 对称的节点对
sum(diag(A%*%A))/2

# 非对称的节点对
sum(diag(A%*%t(A))) - sum(diag(A %*% A))

# 空节点对
n <- length(V(UKfaculty))
(n*(n-1))/2 - sum(diag(A %*% t(A))) + sum(diag(A %*% A))/2

## 三角形 ----------------------------------------------------------------------

# 导入数据
kite <- make_graph("Krackhardt_kite")
# 展示三角形结构，每一列为三角形的三个顶点
matrix(triangles(kite), nrow = 3)

# 每个节点是多少个三角形结构的顶点
count_triangles(kite)


# 设置随机种子
set.seed(42)
# 随机生成一个有向网络
g_sample <- sample_gnm(15, 45, directed = T)
# 网络当中的各种三元结构数量
# triad.census(g_sample)
triad_census(g_sample)


## 星形图、环形图、线图 --------------------------------------------------------

# 设置画布
par(mfrow = c(1, 3))
# 创建星状网络
g_1 <- make_star(7, mode = "undirected")
# 绘图
plot(
    g_1,
    layout = layout.star,
    vertex.label.cex = 4,
    vertex.label.color = 'black',
    vertex.label.dist  = 2
)

# 创建环形网络
g_2 <- make_ring(7)
# 绘图
plot(
    g_2,
    layout = layout.circle,
    vertex.label.cex = 4,
    vertex.label.color = 'black',
    vertex.label.dist  = 2
)

# 创建线形网络
g_3 <- make_lattice(7)
# 绘图
plot(
    g_3,
    vertex.label.cex = 4,
    vertex.label.color = 'black',
    vertex.label.dist  = 2
)

## 星形图、环形图、线图的距离和直径 --------------------------------------------

# 星形图的节点距离、网络直径
distances(g_1)

diameter(g_1)

# 环形图的节点距离、网络直径
distances(g_2)

diameter(g_2)

# 线形图的节点距离、网络直径
distances(g_3)

diameter(g_3)

## 星形图、环形图、线图的度中心性 ----------------------------------------------

# 星形图
degree(g_1, normalized = T)

# 环形图
degree(g_2, normalized = T)

# 线形图
degree(g_3, normalized = T)



