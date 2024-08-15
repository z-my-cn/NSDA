# 第 5 章 网络结构数据的经典模型

## 准备工作 --------------------------------------------------------------------

# 加载相应R包
# 若没有，先用install.packages()命令安装
library(igraph)
library(ergm)
library(sand)


## 生成ER随机图 ----------------------------------------------------------------

# 设置随机种子
set.seed(42)
# 由ER随机图模型生成网络
g <- sample_gnp(n = 100, p = 0.01)
# 计算网络密度
# graph.density(g)
edge_density(g)

# 计算度均值
mean(degree(g))

# 设置画布
par(mfrow = c(1, 2))
# 绘制网络结构图
plot(g, layout = layout.circle, vertex.label = "")
# 绘制网络度分布直方图
hist(degree(g), breaks = 5, main = "", xlab = '度', ylab = '频数')


## 给定度分布生成网络 ----------------------------------------------------------

# 设置随机种子
set.seed(3)
# 从二项分布生成100个随机数
degree_dist <- rbinom(n = 100, size = 99, p = 0.01)
# 根据度分布生成网络
g <- sample_degseq(degree_dist)

# 设置画布
par(mfrow = c(1, 2))
# 绘制网络结构图
plot(g, layout = layout.circle, vertex.label = "")
# 绘制网络度分布直方图
hist(degree(g), breaks = 5, main = "", xlab = '度', ylab = '频数')


## fb数据的简单统计 ------------------------------------------------------------

# 读取边数据
fb_edge <- read.table(
    './Data/fb-pages-tvshow-edges.txt',
    sep = '\t', header = FALSE
)
# 由边数据生成网络
g.fb <- graph_from_edgelist(as.matrix(fb_edge),directed = FALSE)
# 计算网络密度
# graph.density(g.fb)
edge_density(g.fb)

# 生成邻接矩阵
A.fb <- as_adjacency_matrix(g.fb)
# 计算2-star与3-star数量
g.fb.net <- as.network(as.matrix(A.fb), directed = F)
summary(formula(g.fb.net ~ kstar(2) + kstar(3) + triangle))


## 模仿fb实际数据生成ER图 ------------------------------------------------------

# 设置随机种子
set.seed(42)
# 由ER随机图模型生成网络
g.er <- sample_gnp(n = 3892, p = 0.002)
# 生成邻接矩阵
A.er <- as_adjacency_matrix(g.er)
# 计算2-star与3-star数量
g.er.net <- as.network(as.matrix(A.er),directed = F)
summary(formula(g.er.net ~ kstar(2) + kstar(3) + triangle))

## 律师合作网络示例 ------------------------------------------------------------

# 数据集准备
# 加载sand包的lazega数据集
data(lazega)
# 生成邻接矩阵A、自变量x(数据框格式)
# A <- get.adjacency(lazega)
A <- as_adjacency_matrix(lazega, attr = NULL)
# x <- get.data.frame(lazega, what = "vertices")
x <- as_data_frame(lazega, what = "vertices")

# 建模所需网络格式lazeganet
lazeganet <- as.network(as.matrix(A), directed = F)
# 添加用于建模的节点属性Office
set.vertex.attribute(lazeganet, "Office", x$Office)


# ER模型

# 固定随机种子
set.seed(42)
# ER模型只有边统计量
model.er <- formula(lazeganet ~ edges)
# 模型汇总
summary(model.er)

# 模型估计
model.er.fit <- ergm(model.er)
summary(model.er.fit)


# Markov model

# 固定随机种子
set.seed(42)
# Markov模型
model.star <- formula(lazeganet ~ edges + kstar(2) + kstar(3)
                     + triangle)
summary(model.star)

# 模型的估计并不收敛
# model.star.fit <- ergm(model.star)


# 交替的k-star模型

# 固定随机种子
set.seed(42)
# 交替的k-star模型
model.alt <- formula(lazeganet ~ edges + gwesp(1, fixed = T))
summary(model.alt)

# 模型不收敛
# mode.alt.fit <- ergm(model.alt)


# 加入自变量的图模型
set.seed(42)
# 自变量考虑了：边数edges、gwesp、以及是否同属一个office
model.alt.x <- formula(lazeganet ~ edges + gwesp(1, fixed = T)
                      + match("Office"))
summary(model.alt.x)

# 模型的估计结果
model.alt.x.fit <- ergm(model.alt.x)

summary(model.alt.x.fit)


# 随机分块模型

# 构建块矩阵
pm <- cbind(c(0.3, 0.002), c(0.002, 0.08))
# 设置随机种子
set.seed(42)
par(mfrow = c(1, 1))
# 由简单随机分块模型生成网络
g <- sample_sbm(100, pref.matrix = pm, block.sizes = c(30,70))
# 设置节点组别
V(g)$group = c(rep(1, 30),rep(2, 70))
# 绘制网络结构图
plot(g,
     layout = layout.kamada.kawai,
     vertex.label = "",
     vertex.size = 4,
     vertex.color = V(g)$group)
