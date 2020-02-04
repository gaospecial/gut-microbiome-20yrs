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

# 需要从 M 中提取作者、通讯作者、单位、国家等信息。
# 作者全名在 M$AF ，通讯作者在 M$RP（但是只有缩写）；
# 机构信息在 M$C1 中。
# 生成几个新字段
# M <- metaTagExtraction(M, Field="SR")  # short tag, 在引文列表中使用
# M <- metaTagExtraction(M, Field="CR_AU")  #	First Author of each cited reference
M <- metaTagExtraction(M, Field="AU_CO")  #	所有作者的所有机构的国家信息，与作者并非一一对应关系。
M <- metaTagExtraction(M, Field="AU_UN")  #	University of affiliation for each co-author and the corresponding author （同时生成的AU1_UN是一个通讯作者）
# M <- metaTagExtraction(M, Field="AU1_CO") #	Country of affiliation for the first author(仅为第一个作者，不包括共同第一作者)
M$AU_CO_NR <- unlist(lapply(strsplit(M$AU_CO,split = ";"),function(x) paste(unique(x),collapse = ";")))
M$AU_UN_NR <- unlist(lapply(strsplit(M$AU_UN,split = ";"),function(x) paste(unique(x),collapse = ";")))

# 加入影响因子数据（最新）
library(dplyr)
file <- "data-raw/2019_Impact_factor.xlsx"
journal_IF <- openxlsx::read.xlsx(file,startRow=3)  %>%
  select(SO,impact_factor) %>%
  mutate(SO=toupper(SO)) %>%
  unique()

# 按照影响因子分组
M <- M %>% left_join(journal_IF) %>%
  mutate(group=cut(impact_factor,
                   breaks = c(-Inf,3,5,10,20,Inf),
                   labels = c("<3",">3",">5",">10",">20")))

# 保存为 RDS
saveRDS(M,file = "data/M.RDS")


