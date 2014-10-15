## === 第一次用R需安裝套件 ===
list.of.packages <- c("ggplot2", "xlsx", "FactoMineR","showtext",
                      "Cairo", "ggthemes","directlabels","dplyr")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)


# 內建圖
corrPlot <- function (df,label=c("印象","品牌"),textSize=10,
                      x_limits=c(-0.5,0.6),.alpha=0.7,
                      x=substitute(Dim1),y=substitute(Dim2)) {
  require(ggplot2)
  require(ggthemes)
  xName  <- substitute(x); yName  <- substitute(y)
  p <- ggplot(df, aes_q(x=xName, y=yName, label=quote(rownames(df))))
  p <-  p + geom_text(aes(label=rownames(df),
                          colour=factor(type),family="華康新特明體"),
                      alpha=0.7,size=textSize) +
    scale_size_continuous(range = c(6, 14),guide=FALSE)+
    scale_colour_discrete(name=" ",breaks=1:2,labels=label)+
    scale_x_continuous(limits = c(-0.5,0.6)) + # 調整x刻度
    theme_solarized_2() +
    theme(legend.position="top",legend.text=element_text(size=15))
  
  if (exists(deparse(substitute(gradient)))) {
    p <-  p + aes(size=gradient)
  }
  return(p)
}


## ======== 點點版 ==============
corrPlot2 <- function (df,label=c("印象","品牌"),textSize=1,
                       x_limits=c(-0.5,0.6),.alpha=0.7,
                       x=substitute(Dim1),y=substitute(Dim2)) {
  require(ggplot2)
  require(ggthemes)
  require("directlabels")
  xName  <- substitute(x); yName  <- substitute(y)
  p <- ggplot(df, aes_q(x=xName, y=yName, colour=quote(factor(type)))) + 
    geom_point(shape=1,size=2)
  p <-  p + geom_dl(aes(label=rownames(df)),
                    method=list("extreme.grid",fontfamily="華康新特明體",alpha=.alpha,
                                cex=textSize)
  )+
    scale_size_continuous(range = c(6, 14),guide=FALSE)+
    scale_colour_discrete(name="對應變數",breaks=1:2,labels=label)+
    scale_x_continuous(limits = x_limits) + # 調整x刻度
    #     theme_solarized_2() +
    theme(legend.position="top",legend.text=element_text(size=15))  
  return(p)
}


# ==== 圖片輸出 ====
picOutput <- function (pic, PicName="corrPlot.png",width=800,height=600) {
  require(showtext)
  require(Cairo)
  try(font.add("華康新特明體", "新特明體.ttc"))
  CairoPNG(PicName, width, height)
  showtext.begin();
  plot(pic)
  showtext.end();
  dev.off();
}
