library(ggplot2)
head(diamonds) 
head(mpg) # jidousya no nennpi
# ?mpg
# displ is haikiryo, hwy is  nennpi
ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy))
m1 <- ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy, colour = class))
ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy, shape = class))
ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy, colour = "red"))
ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy)) +
  facet_wrap(~class)
m2 <- ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy, colour = class)) +
  facet_wrap(~class)
m1
m2
ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy))
ggplot(data = mpg) +
  geom_smooth(mapping = aes(x = displ, y = hwy))
ggplot(data = mpg) +
  geom_smooth(mapping = aes(x = displ, y = hwy, colour = drv))
ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) +
  geom_point() +
  geom_smooth()
ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) +
  geom_point(aes(colour = class)) +
  geom_smooth()
# diamonds
ggplot(data = diamonds, mapping = aes(x = cut)) +
  geom_bar()

ggplot(data = diamonds, mapping = aes(x = cut, fill = cut)) +
  geom_bar()

ggplot(data = diamonds, mapping = aes(x = cut, y = price)) +
  geom_boxplot()
a <- ggplot(diamonds, aes(cut, price, fill = cut)) +
  geom_violin(trim = FALSE) +
  geom_boxplot(width = 0.08, fill = "white", outlier.size = FALSE) +
  theme_light()
a + coord_flip()
ggsave(a + coord_flip(), filename = "violinplot.jpg")





########
getwd()
list.files()
data <- readr::read_csv("data2_corrected.csv")
head(data)
class(data)
str(data)

summary(data)
summary(data$Food_A)
mean(data$Food_A) # 平均
sd(data$Food_A) # 標準偏差
var(data$Food_A) # 分散
class(data$Gender) # Rではデータの型が重要となる. Genderはcharacterなので、これをfactorに変換する必要がある
data$Gender <- factor(data$Gender, #factorに変更
                      levels = c("Male", "Female"))  # Maleを1, Femaleを2に指定
class(data$Gender)
summary(data)
summary(subset(data, Gender == 'Female')) #女性のみを抽出

p <- ggplot(data, aes(Age, Food_A)) +  # 解析対象の列を指定
  geom_point()         # 散布図なのでpointで作図することを指定
p

p +  geom_point(aes(color = Gender)) # デフォルト

p +  geom_point(aes(color = Gender)) + 
  scale_colour_manual(values = c(Male  = "black", Female  = "red")) # 色を指定した

p +  geom_point(size = 3, aes(shape = Gender)) # 丸と三角

p + stat_smooth(method = "lm") # 回帰直線を追加
summary(data)

p <- ggplot(data,     # データを指定
            aes(Height, Weight)) +  # x, y軸をそれぞれ指定
  geom_point(aes(color = Gender)) # 性差について色分け
p 

p <- ggplot(data, aes(x = Gender, y = BMI))
p + geom_boxplot() # 箱ひげ図
# 身長の分布
ggplot(data, aes(x = Height)) + geom_density()
ggplot(data, aes(x = Height)) + geom_density(aes(colour = Gender))
ggplot(data, aes(x = Height)) + geom_histogram(fill = "white", colour = "black") +
  facet_grid(Gender ~ .)



########
### 少しはバイオインフォマティクス的なプロットも
# MA-plotやvolcano plotを作図してみる
merged <- readr::read_delim("merged_annot.txt", delim = " ")
str(merged)
head(merged)
DE_sig <- dplyr::filter(merged, adj.P.Val <= 0.05, SYMBOL != "NA")
# FDRが0.05以下の遺伝子のみ抜き出す
ma <- ggplot(data = merged, aes(x = AveExpr, y = logFC)) + 
  geom_point(size = 0.1) +
  geom_point(data = DE_sig, colour = "red", size = 0.3) + 
  theme_classic()
ma + ggtitle("MA plot")
library(ggrepel)
ma + geom_text_repel(data = DE_sig, aes(label = SYMBOL))

vp <- ggplot(data = merged, aes(x = logFC, y = -log10(adj.P.Val))) + 
  geom_point(size = 0.1) +
  geom_point(data = DE_sig, colour = "red", size = 0.3) + 
  theme_classic()
vp
vp + ggtitle("volcano plot of microarray")
vp + geom_text_repel(data = DE_sig, aes(label = SYMBOL))

######
library(MotifDb)
library(seqLogo)
library(ggseqlogo)
# motif visualisation
PotentialIRF8 <- as.list(subset (MotifDb,
                                 tolower (geneSymbol)=="irf8"))
# seqlogo
PotentialIRF8[[2]]
seqLogo(PotentialIRF8[[2]]) # mouse
seqLogo(PotentialIRF8[[4]]) # human

# ggseqlogo
ggseqlogo(PotentialIRF8[[2]])
ggseqlogo(PotentialIRF8[[4]])

p1 <- ggseqlogo(PotentialIRF8[[2]], method = "bits")
p2 <- ggseqlogo(PotentialIRF8[[2]], method = "prob")
gridExtra::grid.arrange(p1, p2)

p3 <- ggplot() + geom_logo(PotentialIRF8[[2]]) + theme_classic() + 
  scale_y_continuous(expand = c(0,0), limits = c(0, 2.0)) + 
  scale_x_continuous(expand = c(0,0), breaks = seq(1,15,1))
gridExtra::grid.arrange(p1, p3)

# add annotation
ggplot() + 
  annotate('rect', xmin = 2.5, xmax = 6.5, ymin = -0.05, ymax = 1.9, alpha = .1, col='black', fill='yellow') +
  annotate('rect', xmin = 8.5, xmax = 12.5, ymin = -0.05, ymax = 1.9, alpha = .1, col='black', fill='yellow') +
  geom_logo(PotentialIRF8[[2]], stack_width = 0.90) + 
  annotate('text', x=4.5, y=2, label='IRF or ETS') + 
  annotate('text', x=10.5, y=2, label='IRF motif') + 
  theme_logo()


