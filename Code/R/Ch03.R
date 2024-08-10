# 第 3 章 网络数据结构可视化

## 准备工作 -------------------------------------------------------------------

# 加载相应R包
# 若没有，先用install.packages()命令安装
library(igraph)
library(igraphdata)
library(networkD3)

# 颜色
com_color <- c(
    "#FFCABC",'#85E0FF','#96FAA0','#FFC097','#A0BED2','#67FCFF',
    '#FFB5D0','#89C7B9','#C0EE75','#FFB4CF','#CDE0FF','#E6D554',
    '#C5ACBC','#52D3BF','#E7DEFE','#FF99AD','#FF9AAE','#F2AAAD',
    '#85E3FF','#53D6C1','#FFC5FF','#DFB56E','#FFD2FF'
)

## 布局方式 --------------------------------------------------------

# 导入karate数据
data("karate")
g <- karate

# 不设置节点颜色
V(g)$color = 1

# 设置随机种子
set.seed(42)
# 绘制网络结构图：随机布局
plot(g, layout = layout.random)


# 设置随机种子
set.seed(42)
# 设置画布
par(mfrow = c(1, 2))
# 环形布局
plot(g, layout = layout.circle)
# 星形布局
plot(g, layout = layout.star)


# 设置随机种子
set.seed(42)
# 设置画布
par(mfrow = c(1, 1))
# 球形布局
plot(g, layout = layout.sphere)


# 设置随机种子
set.seed(42)
# 设置画布
par(mfrow = c(1, 2))
# 生成树
g_tree <- make_tree(15, 3)
# 树形布局
plot(g_tree, layout = layout_as_tree)
# 树形布局 圆形排列
plot(g_tree, layout = layout_as_tree(g_tree, circular = TRUE))


# 设置随机种子
set.seed(42)
# 设置画布
par(mfrow = c(1, 2))
# 力导向布局Fruchterman Reingold算法
plot(g, layout = layout.fruchterman.reingold)
# 力导向布局Kamada Kawai算法
plot(g, layout = layout.kamada.kawai)

## 设置节点和连边属性 --------------------------------------------

set.seed(42)
par(mfrow = c(1, 1))
# 导入数据
data("karate")

plot(karate,
     # 力导向布局
     layout = layout.fruchterman.reingold,
     # 设置节点大小
     vertex.size = 4,
     # 设置节点形状
     vertex.shape = 'circle',
     # 设置节点颜色
     vertex.color = 'gold',
     # 设置节点标签
     vertex.label = V(karate)$name,
     # 设置节点标签大小
     vertex.label.cex = 0.8,
     # 设置节点和标签的距离，避免重叠
     vertex.label.dist = 1,
     # 设置节点标签颜色
     vertex.label.color = 'grey2',
     # 有向图中设置连边的箭头的大小,若为0即为无向图
     edge.arrow.size = 0,
     # 设置连边颜色
     edge.color = "darkgray",
     # 设置连边的粗细
     edge.width = 2,
     # 设置边权，此处直接使用数据集中的权重作为边权
     edge.label = E(karate)$weight,
     # 设置连边标签大小
     edge.label.cex = 0.6,
     # 设置连边标签颜色
     edge.label.color = 'red')


## 节点和连边分别反映节点和边的特性 ------------------------------------------

# 设置随机种子
set.seed(42)
# 导入数据
data("karate")
plot(karate,
     # 连边的粗细反映权重
     edge.width = E(karate)$weight,
     # 设置节点标签颜色为黑色
     vertex.label.color = 'black',
     # 设置节点标签大小
     vertex.label.cex = 1,
     # 节点大小反映节点的度的大小
     vertex.size = degree(karate),
     # 设置节点的颜色反映节点的阵营
     vertex.color = as.numeric(V(karate)$Faction))


## 用V(G)和E(G)设置节点和连边的属性 --------------------------------------------


# 设置随机种子
set.seed(42)


# 设置节点大小
V(karate)$size <- 5
# 设置节点颜色
V(karate)$color <- 'gold'
# 设置节点形状
V(karate)$shape <- 'circle'
# 设置节点标签大小
V(karate)$label.cex <- 0.8
# 设置节点与标签的距离
V(karate)$label.dist <- 1
# 设置节点标签的颜色
V(karate)$label.color <- 'grey2'
# 设置度最大的节点的颜色为蓝色
V(karate)[which.max(degree(karate))[[1]]]$color <- "blue"
# 设置度最大的节点的标签颜色为蓝色
V(karate)[which.max(degree(karate))[[1]]]$label.color <- "blue"


# 设置边的类型，2表示虚线
E(karate)$lty <- 2
# 设置边权标签大小
E(karate)$label.cex <- 0.6
# 设置连边颜色
E(karate)$color <- "grey"
# 设置权重最大的边的线为实线
E(karate)[E(karate)$weight == max(E(karate)$weight)]$lty <- 1
# 设置权重最大的边的颜色为黄色
E(karate)[E(karate)$weight == max(E(karate)$weight)]$color <- "grey3"
# 添加权重最大的3条边的标签
E(karate)[order(E(karate)$weight, decreasing = T)[1:3]]$label <- E(karate)[order(E(karate)$weight, decreasing = T)[1:3]]$weight
# 设置连边标签颜色
E(karate)$label.color <- 'red'

# 绘制网络图，图布局选取力导向布局
plot(karate, layout = layout.fruchterman.reingold)


## 提取核心子图 ----------------------------------------------------------------

# 设置画布
par(mfrow = c(1, 2))
# 设置随机种子
set.seed(42)


# 读取边数据
largest_edge <- read.csv('./Data/edge_largest.csv')
# 由边数据构造无向网络
g <- graph_from_data_frame(largest_edge, directed = FALSE)
# 计算网络的节点数
print(paste0('原始网络的节点数为：', vcount(g)))
# 计算网络的边数
print(paste0('原始网络的边数为：', ecount(g)))


# 不显示标签
V(g)$label = ""
plot(g,
     # 设置节点大小
     vertex.size = 5,
     # 设置节点颜色
     vertex.color = 'lightsteelblue2',
     # 设置节点边框颜色
     vertex.frame.color = 'skyblue4',
     # 不显示节点标签
     vertex.label = V(g)$label,
     # 力导向布局
     layout = layout.fruchterman.reingold)

# 获得邻接矩阵
W <- as_adjacency_matrix(g)
# 将"dgCMatrix"类型转化为"matrix"类型
W <- as.matrix(W)
# 提取核心网络
converg <- FALSE
old.nrow <- nrow(W)
while(!converg){
    # 计算W矩阵的列和
    d <- colSums(W)
    to.keep <- which(d >= 6)
    # 保留列和大于等于6的列
    if(old.nrow == length(to.keep)){
        converg <- TRUE
    }
    old.nrow <- length(to.keep)
    W <- W[to.keep,to.keep]
}
g_core <- graph_from_adjacency_matrix(W, mode = "undirected")
# 计算核心网络的节点数
print(paste0('核心网络的节点数为：', vcount(g_core)))
# 计算核心网络的边数
print(paste0('核心网络的边数为：', ecount(g_core)))


# 不显示标签
V(g_core)$label = ""
# 绘图
plot(g_core,
     # 设置节点大小
     vertex.size = 5,
     # 设置节点颜色
     vertex.color = 'lightsteelblue2',
     # 设置节点边框颜色
     vertex.frame.color = 'skyblue4',
     # 不显示节点标签
     vertex.label = V(g_core)$label,
     # 力导向布局
     layout = layout.fruchterman.reingold)


## 提取邻域子图 ----------------------------------------------------------------

# 设置随机种子
set.seed(42)
# 读取边数据
largest_edge <- read.csv('./Data/edge_largest.csv')
# 由边数据构造无向网络
g <- graph_from_data_frame(largest_edge, directed = FALSE)
# 提取节点邻域
# gn <- graph.neighborhood(g, order = 1)
gn <- make_ego_graph(g, order = 1, nodes = V(g))
# 展示前20个节点邻域的子图节点个数
sapply(gn, vcount)[1:20]


par(mfrow = c(1,2))
# 绘制节点A18738-0的1阶邻域子图
plot(gn[[which(V(g)$name == "A18738-0")]],
     # 设置节点大小
     vertex.size = 10,
     # 设置节点颜色
     vertex.color = 'lightsteelblue2',
     # 设置节点边框颜色
     vertex.frame.color = 'skyblue4',
     # 设置节点标签大小
     vertex.label.cex = 0.8,
     # 设置节点和标签的距离，便于错开重叠
     vertex.label.dist  = 2,
     # 力导向布局
     layout = layout.fruchterman.reingold)
# 绘制节点A17905-1的1阶邻域子图
plot(gn[[which(V(g)$name == "A17905-1")]],     # 设置节点大小
     vertex.size = 10,
     # 设置节点颜色
     vertex.color = 'lightsteelblue2',
     # 设置节点边框颜色
     vertex.frame.color = 'skyblue4',
     # 设置节点标签大小
     vertex.label.cex = 0.8,
     # 设置节点和标签的距离，便于错开重叠
     vertex.label.dist  = 2,
     # 力导向布局
     layout = layout.fruchterman.reingold)


## 网络社区发现 ----------------------------------------------------------------

# 设置随机种子
set.seed(42)
par(mfrow = c(1, 1))
# 使用multilevel.community函数对核心网络进行社区划分
# com <- multilevel.community(g_core)
com <- cluster_louvain(g_core)
# 展示每个社区的大小
table(com$membership)


# 为节点添加社区属性
V(g_core)$com <- com$membership
# 不显示标签
V(g_core)$label <- ""
# 绘制
plot(g_core,
     # 力导向布局
     layout = layout.fruchterman.reingold,
     # 设置节点大小
     vertex.size = 7,
     # 设置节点颜色
     vertex.color = com_color[V(g_core)$com],
     # 设置节点边框颜色
     vertex.frame.color = 'grey50')


## 简化网络 --------------------------------------------------------------------

# 设置随机种子
set.seed(42)
# 合并社区节点
gc <- contract(g_core, V(g_core)$com)
# 简化网络
gc <- simplify(gc)
# 不显示标签
V(gc)$label <- ""
# 绘图
plot(gc,
     # 设置节点大小
     vertex.size = sapply(V(gc)$name,length),
     # 设置节点颜色
     vertex.color = com_color[V(gc)],
     # 设置节点边框颜色
     vertex.frame.color = 'grey50',
     # 力导向布局
     layout = layout.fruchterman.reingold)


## 动态交互 --------------------------------------------------------------------

# 设置随机种子
set.seed(42)
ChaData <- read.csv("./Data/got-edges.csv")
# 由边数据构造无向网络
g <- graph_from_data_frame(ChaData, directed = FALSE)
# 使用cluster_infomap函数对网络进行社区划分
com <- cluster_infomap(g)
# 将网络转换为networkD3形式，并为节点添加社区属性
g_D3 <- igraph_to_networkD3(g, group = membership(com))
# 为节点添加度属性
g_D3$nodes$Nodesize <- degree(g)
# 为连边添加权重
g_D3$links$value <- E(g)$Weight
# 指定点击事件
script <- 'alert("name: " + d.name + ", group: " + d.group + ", size: " + d.nodesize)'
# 绘图
network <- forceNetwork(# 添加节点
    Nodes = g_D3$nodes,
    # 添加连边
    Links = g_D3$links,
    # 设置节点大小
    Nodesize = "Nodesize",
    # 设置节点标签
    NodeID = "name",
    # 设置节点社区
    Group = "group",
    # 设置节点的透明度
    opacity = 0.9,
    # 设置标签字体
    fontFamily = "宋体",
    # 设置标签字体大小
    fontSize = 25,
    # 设置连边颜色
    linkColour = "grey",
    # 设置颜色
    colourScale = JS("d3.scaleOrdinal(d3.schemeCategory20);"),
    # 设置节点半径计算方式
    radiusCalculation = "d.nodesize",
    # 设置点击事件
    clickAction = script,
    # 设置是否可以缩放
    zoom = TRUE)
# 展示动态交互网络
network


