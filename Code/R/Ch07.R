# 第 7 章 链路预测

## 准备工作 --------------------------------------------------------------------

# 加载相应R包
# 若没有，先用install.packages()命令安装
library(igraph)
library(sand)
library(pROC)
library(linkprediction)


## 自我中心网络 ----------------------------------------------------------------

# 使用fblog数据
data("fblog")
g <- fblog

# ego函数返回节点的自我中心网络(包含自身)
# ego函数的返回结果是list
ego(g, order = 1, nodes = 1)

ego(g, order = 1, nodes = c(1,3))


## 共同好友(CN)的计算

# 网络的规模
n <- vcount(g)
# 构建邻接矩阵A
A <- as_adjacency_matrix(g, sparse = F)
# 用向量记录共同邻居
cn <- c()
# 共同好友
for (i in (1:(n-1))) {
    #节点i的邻居
    ni <- ego(g, order = 1, nodes = i)
    #节点i+1到节点n的邻居
    nj <- ego(g, order = 1, nodes = (i+1):n)
    #节点i和i+1到n的共同邻居
    nei.ij <- mapply(intersect, ni, nj, SIMPLIFY = F)
    #共同邻居的数量(减去节点直接相连产生的计数)
    num <- unlist(lapply(nei.ij, length)) - 2*A[i, (i+1):n]
    #更新共同邻居指标
    cn <- c(cn, num)
}
# 展示部分的CN指标取值
cn[1:5]


# 取邻接矩阵的下三角阵
Avec <- A[lower.tri(A)]
# 分别绘制相连的节点对和不相连的节点对共同邻居数的箱线图
boxplot(cn[Avec == 0], cn[Avec == 1],
        varwidth = T,
        names = c("不相连", "相连"),
        ylab = "共同好友数")


## Salton余弦相似性 ------------------------------------------------------------

# 用向量记录Salton余弦相似性
sc <- c()
# SC
for (i in (1:(n-1))){
    #节点i的邻居
    ni <- ego(g, order = 1, nodes = i)
    #节点i+1到节点n的邻居
    nj <- ego(g, order = 1, nodes = (i+1):n)
    #节点i和i+1到n的共同邻居
    nei.ij <- mapply(intersect, ni, nj, SIMPLIFY = F)
    #共同邻居的数量(减去节点直接相连产生的计数)
    num1 <- unlist(lapply(nei.ij, length)) - 2*A[i, (i+1):n]
    #分母：节点i与i+1到n的度数相乘再开方
    num2 <- sqrt(degree(g)[i]*degree(g)[(i+1):n])
    #计算SC
    num <- num1/num2
    #更新SC指标
    sc <- c(sc, num)
}

# 展示部分SC指标取值
sc[1:5]


## PA指标 ----------------------------------------------------------------------

#节点度数的向量
d <- as.matrix(degree(g), n, 1)
#每个节点对之间度数相乘所得的矩阵
dm <- d %*% t(d)
#取下三角阵
pa <- dm[lower.tri(dm)]
# 展示部分指标取值
pa[1:5]


## Adamic-Adar(AA)系数 ---------------------------------------------------------

#利用similarity函数计算两个节点之间的相似性
aa_mat <- similarity(g, method = "invlogweighted")
#取下三角阵
aa <- aa_mat[lower.tri(aa_mat)]
# 展示部分AA指标取值
aa[1:5]


## 局部路径(LP) ----------------------------------------------------------------

alpha <- 0.01
lp_mat <- A %*% A + alpha * (A %*% A) %*% A
lp <- lp_mat[lower.tri(lp_mat)]

# 展示部分LP的指标取值
lp[1:5]

## 绘制后四个指标的分组箱线图 --------------------------------------------------

# 绘制后四个指标的分组箱线图
method <- c("SC","AA","PA","LP")
all <- list(sc, aa, pa, lp)
par(mfrow = c(2,2))
for(i in 1:4){
    boxplot(all[[i]][Avec == 0], all[[i]][Avec == 1],
            varwidth = T,
            names = c("不相连", "相连"),
            ylab = method[i])
}


## 五种方法预测效果比较 --------------------------------------------------------

r1 <- roc(Avec, cn)
r2 <- roc(Avec, sc)
r3 <- roc(Avec, aa)
r4 <- roc(Avec, pa)
r5 <- roc(Avec, lp)
r <- list(r1, r2, r3, r4, r5)
method <- c("CN", "SC", "AA", "PA", "LP")

# 绘制ROC曲线
par(mfrow = c(2,3))
for(i in 1:5){
    plot.roc(r[[i]], print.auc = T, main = method[i])
}

#AUC值
for(i in 1:5){
    print(r[[i]]$auc)
}

## linkprediction包能够计算相似性指标 ------------------------------------------

#计算相似性指标——LHN 返回一个矩阵
LHN <- proxfun(fblog, method = 'lhn_local')
print(LHN[1:3,1:3])


#计算相似性指标——RA,返回一个包含三列的数据框，前两列是边，第三列是指标值
RA <- proxfun(fblog, method = 'ra', value = 'edgelist')
head(RA,3)
