#' # 处理 WOS 原始数据



library(bibliometrix)
#' ## 全部的文献数据
# 如果是首次使用，需要处理原始文件。否则可以直接到下一个 Chunk 读取预存的数据。
all_record <- "data-raw/Gut_Microbiome_all.zip"

# 读取文件
content <- readFiles(all_record)
# 耗时2个小时读取完毕
M <- convert2df(content)

#' ## 标记高被引论文
highly_cited <- "data-raw/Highly_cited.txt"
content <- readFiles(highly_cited)
# 耗时2个小时读取完毕
highly_cited <- convert2df(content)


#' 高被引论文都包括在全部论文中

library(ggVennDiagram)
ggVennDiagram(list(all=rownames(M),high=rownames(highly_cited)))


#' 将高被引论文在 `M` 中做一个标记
M$HC <- FALSE
M[rownames(highly_cited),"HC"]  <- TRUE
summary(M$HC)



# 保存为 RDS
saveRDS(M,file = "data/M.RDS")

