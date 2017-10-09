# Network Analysis
# Table this until I can figure it out!
cats <- all_vids[,"categories"]
cats$categories <- str_replace_all(cats$categories, "Gay(, )?|Babe(, )?|HD(, )?|Verified Amateurs(, )?|Virtual Reality(, )?|Exclusive(, )?|Verified Models(, )?", "")
cats$categories <- str_replace_all(cats$categories, ", $", "")

cats <- str_split(cats$categories, ", ")
cats <- do.call(rbind,cats)

cats <- separate(data = cats, col = categories, into = c("V1", "V2", "V3", "V4", "V5"), sep = ", ")

# cats$categories <- str_split(cats$categories, ", ")
# head(combn(rowwise(cats), 2))



cat_table<- read.table(text=gsub(pattern = ", ", replacement = ",",cats),sep=",", stringsAsFactors = FALSE, fill = TRUE)
# Tables by values
v1v2tab <- as.data.frame(table(cat_table[,c(1,2)]))
v1v2tab <- v1v2tab[-which(v1v2tab$V2==""),]
v1v2tab <- reshape2::dcast(v1v2tab, V1 ~ V2, value.var = "Freq", fill=0)
rownames(v1v2tab) <- v1v2tab[,1]
v1v2tab <- v1v2tab[,2:ncol(v1v2tab)]
v1v3tab <- as.data.frame(table(cat_table[,c(1,3)]))
v1v3tab<-v1v3tab[-which(v1v3tab$V3==""),]
v1v3tab <- reshape2::dcast(v1v3tab, V1 ~ V3, value.var = "Freq", fill=0)
rownames(v1v3tab) <- v1v3tab[,1]
v1v3tab <- v1v3tab[,2:ncol(v1v3tab)]
v1v4tab <- as.data.frame(table(cat_table[,c(1,4)]))
v1v4tab<-v1v4tab[-which(v1v4tab$V4==""),]
v1v4tab <- reshape2::dcast(v1v4tab, V1 ~ V4, value.var = "Freq", fill=0)
rownames(v1v4tab) <- v1v4tab[,1]
v1v4tab <- v1v4tab[,2:ncol(v1v4tab)]
v2v3tab <- as.data.frame(table(cat_table[,c(2,3)]))
v2v3tab<-v2v3tab[-which(v2v3tab$V3==""),]
v2v3tab <- reshape2::dcast(v2v3tab, V2 ~ V3, value.var = "Freq", fill=0)
rownames(v2v3tab) <- v2v3tab[,1]
v2v3tab <- v2v3tab[,2:ncol(v2v3tab)]
v2v4tab <- as.data.frame(table(cat_table[,c(2,4)]))
v2v4tab<-v2v4tab[-which(v2v4tab$V4==""),]
v2v4tab <- reshape2::dcast(v2v4tab, V2 ~ V4, value.var = "Freq", fill=0)
rownames(v2v4tab) <- v2v4tab[,1]
v2v4tab <- v2v4tab[,2:ncol(v2v4tab)]
v3v4tab <- as.data.frame(table(cat_table[,c(3,4)]))
v3v4tab<-v3v4tab[-which(v3v4tab$V4==""),]
v3v4tab <- reshape2::dcast(v3v4tab, V3 ~ V4, value.var = "Freq", fill=0)
rownames(v3v4tab) <- v3v4tab[,1]
v3v4tab <- as.character(v3v4tab[,2:ncol(v3v4tab)])

adjmat <- v1v2tab
adjmatcol<-match(colnames(v1v3tab), colnames(v1v2tab))
adjmatrow<-match(rownames(v1v3tab), rownames(v1v2tab))
adjmat[adjmatrow, adjmatcol] <- adjmat[adjmatrow, adjmatcol] + v1v3tab

#,colnames(v1v4tab),colnames(v2v3tab),colnames(v2v4tab),colnames(v3v4tab))