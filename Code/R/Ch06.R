# 第 6 章 网络结构数据的社区发现

## 准备工作 --------------------------------------------------------------------

# 加载相应R包
# 若没有，先用install.packages()命令安装
library(igraph)
library(igraphdata)


## 导入数据 --------------------------------------------------------------------

data("karate")


## edge betweenness算法 --------------------------------------------------------

# edge betweenness
set.seed(10)
ceb <- cluster_edge_betweenness(karate)
ceb

# 绘制社区发现结果图
plot(ceb, karate)

# 计算社区数量
length(ceb)

# 计算每个社区大小
sizes(ceb)

# 展示每个节点所属的社区。为节省空间，仅展示五个节点
membership(ceb)[1:5]

# 展示每个社区的节点。为节省空间，仅展示第一个社区
communities(ceb)[1]

# 计算ceb社区划分下的模块度
modularity(ceb)

# 给出ceb社区发现使用的算法
algorithm(ceb)

# 展示每条边是否是跨社区的边。如果这条边连接两个不同社区，则输出“TRUE”，否则输出“FALSE”。
crossing(ceb, karate)[1:2]  # 为节省空间，仅展示两条边


# 检查是否使用了分层算法来社区发现
is_hierarchical(ceb)

# 绘制层次社区结构的树状图，或dendPlot(ceb)
plot_dendrogram(ceb)


# 设置社区数量为2
ceb2 <- cut_at(ceb, no = 2)
V(karate)$Community = ceb2

#与真实标签作比较
V(karate)[Faction == 1]$shape <- 'square'
V(karate)[Faction == 2]$shape <- 'circle'
plot(karate, vertex.color = V(karate)$Community)


## fast greedy算法 -------------------------------------------------------------

# fastgreedy
set.seed(10)
cfg <- cluster_fast_greedy(karate)
plot(cfg, karate)

## leading eigenvector算法 -----------------------------------------------------

# leading eigenvector
set.seed(5)
cle <- cluster_leading_eigen(karate)
plot(cle, karate)

# 限定社区数量最多两个
set.seed(10)
cle2 <- cluster_leading_eigen(karate,step = 1)
plot(cle2, karate)


## informap算法 ----------------------------------------------------------------

# informap
set.seed(10)
ci <- cluster_infomap(karate)
plot(ci, karate)


## label proporgation算法 ------------------------------------------------------

# label proporgation
set.seed(10)
clp <- cluster_label_prop(karate)
plot(clp, karate)


## Multilevel算法 --------------------------------------------------------------

# Multilevel
set.seed(5)
cl <- cluster_louvain(karate)
plot(cl, karate)


## walktrap算法 ----------------------------------------------------------------

# walktrap
set.seed(1)
cw <- cluster_walktrap(karate)
plot(cw,karate)

## spinglass算法 ---------------------------------------------------------------

# spinglass
set.seed(1)
cs <- cluster_spinglass(karate)
plot(cs, karate)

## 社区结果评价 ----------------------------------------------------------------

# 获取真实社区划分
true_com <- make_clusters(karate, V(karate)$Faction)
membership(true_com)


# Multilevel算法进行社区发现
set.seed(5)
cl <- cluster_louvain(karate)
membership(cl)


# 计算兰德指数
compare(true_com, cl, method = "rand")

# 计算调整的兰德指数
compare(true_com, cl, method = "adjusted.rand")

# 计算分裂连接距离
compare(true_com, cl, method = "split.join")

# 计算信息变差
compare(true_com, cl, method = "vi")

# 计算标准化互信息
compare(true_com, cl, method = "nmi")

# 计算Multilevel算法得到的社区划分的模块度
modularity(karate, membership(cl))


# 读入边数据
data <- read.csv("./Data/edge_largest.csv")
# 用graph_from_data_frame()函数创建无向网络
g <- graph_from_data_frame(data[,c(1,2)],directed = FALSE)

W <- as_adjacency_matrix(g)            # 获得邻接矩阵
W <- as.matrix(W)                      # 将"dgCMatrix"类型转化为"matrix"类型
# 提取核心网络
converg <- FALSE
old.nrow <- nrow(W)
while(!converg){
    d = colSums(W)                       # 计算W矩阵的列和
    to.keep = which(d>=4)                # 保留列和大于等于4的列
    if(old.nrow == length(to.keep)){
        converg = TRUE
    }
    old.nrow = length(to.keep)
    W = W[to.keep,to.keep]
}
g_core <- graph_from_adjacency_matrix(W,mode = 'undirected')  # 由邻接矩阵构造无向网络

# 设置随机种子
set.seed(10)
# 使用cluster_louvain函数对核心网络进行社区划分，g_core是本书3.3.1节的结果
com <- cluster_louvain(g_core)
# 展示每个社区的大小
sizes(com)

# 查看A13633-1节点所属的社区
group_A13633_1 <- membership(com)['A13633-1']


# 提取对应社区的所有作者构成的子网络
# g_com41 <- induced_subgraph(g_core, groups(com)$`41`)
g_com_all <- induced_subgraph(g_core, which(membership(com) == group_A13633_1))
# 计算社区作者构成的合作网络的密度
# print(paste0('子网络的密度为：', graph.density(g_com41)))
print(paste0('子网络的密度为：', edge_density(g_com_all)))

# 绘制社区作者构成的子网络
# 设置节点大小与度成正比
# V(g_com41)$size = seq(2,6,length.out = max(degree(g_com41)))[degree(g_com41)]
V(g_com_all)$size = seq(2,6,length.out = max(degree(g_com_all)))[degree(g_com_all)]
# 设置ID为'A13633-1'的作者的标签为'Fan, Jianqing'
# V(g_com41)[V(g_com41)$name=='A13633-1']$label <- 'Fan, Jianqing'
V(g_com_all)[V(g_com_all)$name=='A13633-1']$label <- 'Fan, Jianqing'

set.seed(200)        # 设置随机种子，保证同一种布局画出来的图可以重复
par(mfrow=c(1,1), mar=c(0,2,0,2))
plot(
    # g_com41,
    g_com_all,
    layout = layout.fruchterman.reingold,           # 绘制力导向布局图
    vertex.label = V(g_com41)$label,                # 显示节点标签
    vertex.color = 'lightsteelblue2',               # 设置节点颜色
    edge.color = 'grey75'                           # 设置边的颜色
)
