# 第 4 章 网络的描述统计

## 准备工作 --------------------------------------------------------------------

# 加载相应R包
# 若没有，先用install.packages()命令安装
library(igraph)
library(igraphdata)
library(dplyr)
library(ergm)
library(ggplot2)

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


## 星形图、环形图、线图的接近中心性 --------------------------------------------

# 星形图
closeness(g_1, normalized = T)

# 环形图
closeness(g_2, normalized = T)

# 线形图
closeness(g_3, normalized = T)


## 星形图、环形图、线图的中介中心性 --------------------------------------------

# 星形图
betweenness(g_1, normalized = T)

# 环形图
betweenness(g_2, normalized = T)

# 线形图
betweenness(g_3, normalized = T)


## 星状结构 --------------------------------------------------------------------

# 设置随机种子
set.seed(42)
# 设置画布
par(mfrow = c(1, 2))

# 星状结构2-star
g_4 <- make_star(3, mode = "undirected")
# 绘图
plot(
    g_4,
    vertex.label.cex = 1.5,
    vertex.label.color = 'black',
    vertex.label.dist  = 2
)

# 星状结构3-star
g_5 <- make_star(4, mode = "undirected")
# 绘图
plot(
    g_5,
    vertex.label.cex = 1.5,
    vertex.label.color = 'black',
    vertex.label.dist  = 2
)


# 星形图
# 生成邻接矩阵
A_1 <- as_adjacency_matrix(g_1)
g_1_net <- as.network(as.matrix(A_1), directed = F)

# 计算2-star与3-star数量
summary(formula(g_1_net ~ edges + kstar(2) + kstar(3)))

# 环形图
# 生成邻接矩阵
A_2 <- as_adjacency_matrix(g_2)
g_2_net <- as.network(as.matrix(A_2),directed = F)

# 计算2-star与3-star数量
summary(formula(g_2_net ~ edges + kstar(2) + kstar(3)))


# 线形图
# 生成邻接矩阵
A_3 <- as_adjacency_matrix(g_3)
g_3_net <- as.network(as.matrix(A_3),directed = F)

# 计算2-star与3-star数量
summary(formula(g_3_net ~ edges + kstar(2) + kstar(3)))


# 设置随机种子
set.seed(42)
# 设置画布
par(mfrow = c(1, 2))

# 星状结构2-star
g_4 <- make_star(3, mode = "in")
# 绘图
plot(
    g_4,
    vertex.label.cex = 1.5,
    vertex.label.color = 'black',
    vertex.label.dist  = 2
)

# 星状结构3-star
g_5 <- make_star(3, mode = "out")
# 绘图
plot(
    g_5,
    vertex.label.cex = 1.5,
    vertex.label.color = 'black',
    vertex.label.dist  = 2
)


## 共同邻居 --------------------------------------------------------------------

# 设置随机种子
set.seed(42)
par(mfrow = c(1, 1))

# 创建网络
g_6 <- make_empty_graph(n = 5, directed = FALSE)
# 添加连边
g_6 <- add_edges(g_6, c(1,2,2,3,4,5,1,4))
# 绘图
plot(
    g_6,
    layout = layout.star,
    vertex.label.cex = 1.5,
    vertex.label.color = 'black',
    vertex.label.dist  = 2
)

# 节点2的邻居
n2 <- neighbors(g_6, 2)
n2

# 节点4的邻居
n4 <- neighbors(g_6, 4)
n4

# 节点2与节点4的共同邻居
intersection(n2, n4)


## 案例：统计学科合作者网络 ----------------------------------------------------


# 读入数据并构建网络

# 读入数据
data <- read.csv("./Data/edge_largest.csv")

# 用graph_from_data_frame()函数创建无向网络
g <- graph_from_data_frame(data, directed = FALSE)

# 计算网络的节点数
print(paste0('网络的节点数为：', vcount(g)))

# 计算网络的边数
print(paste0('网络的边数为：', ecount(g)))

# 计算网络的密度
# print(paste0('网络的密度为：', graph.density(g)))
print(paste0('网络的密度为：', edge_density(g)))

绘制度分布直方图
hist(
    degree(g),
    ylab = "频数", xlab = "度",
    main = " ", col = "white",
    ylim = c(0,5000), xlim = c(0,120)
)


# 绘制双对数度分布图
# 计数
data_freq <- data.frame(table(degree(g)))
data_freq$Var1 <- as.numeric(as.character(data_freq$Var1))
# 绘图
plot(
    log(as.numeric(data_freq$Var1)), log(data_freq$Freq),
    xlab = "对数-度", ylab = "对数-作者数"
)


# 度
# 选择度前10的论文
# 计算节点的度并储存为数据框
degree <- data.frame(deg = degree(g))
# 将节点的name新增为一列
degree$name <- row.names(degree)
# 按照degree降序排列
degree <- degree[order(degree$deg, decreasing = T),]
# 读入重要的作者信息
author <- read.csv("./Data/author.csv")
# 匹配排名前10的作者信息
degree_top10 <- left_join(degree[1:10, ], author)

# 展示
head(degree_top10[ ,c(1:3)], 10)

# 计算路径与最短距离
# 使用all_shortest_paths()函数计算出第1个节点到第19个节点之间的所有最短路径

all_shortest_paths(
    g,
    from = V(g)[which(V(g)$name == "A7232-0")],
    to = V(g)[which(V(g)$name == "A18422-0")], mode = "all"
)$res

# 计算节点的中心性
# 计算节点的中介中心性并储存为数据框
betweenness <- data.frame(bet = betweenness(g, directed = F, normalized = T))
# 将节点的name新增为一列
betweenness$name <- row.names(betweenness)
# 降序排列
betweenness <- betweenness[order(betweenness$bet, decreasing = T),]
# 读入重要的作者信息
author <- read.csv("./Data/author.csv")
# 匹配作者信息
betweenness_top10 <- left_join(betweenness[1:10, ], author)

# 展示
head(betweenness_top10[ , c(1:3)], 10)
