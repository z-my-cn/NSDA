# 第 1 章 概述

## 准备工作 --------------------------------------------------------------------

# 若没有，先用install.packages()命令安装
library(igraph)
library(igraphdata)

## karate数据示例 --------------------------------------------------------------

# 导入karate数据
data("karate")

# 展示网络基本情况
karate

# 设置随机种子
set.seed(42)
# 绘制网络结构图
plot(karate)

## igraphdata包中的数据 --------------------------------------------------------

# 查看数据列表
data(package="igraphdata")
# 导入Koenigsberg数据
data("Koenigsberg")

# 展示网络基本情况
Koenigsberg

# 设置随机种子
set.seed(42)
# 绘制网络结构图
plot(Koenigsberg)

