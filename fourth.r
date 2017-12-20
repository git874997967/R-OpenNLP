 library(quantmod)
 getSymbols(c("QQQ","SPY"),src="yahoo") # 从Yahoo Finance下载QQQ与SPY这两个ETF（交易所交易基金）的数据。QQQ与SPY分别是纳斯达克100指数与标准普尔500指数。
 
 chartSeries(QQQ,subset="2014-08::2015-08",theme=chartTheme("balck"),TA="addVo();addBBands();addATR();addCMF()") # 画QQQ的数据，从2014年8月至2015年8月，白色背景，TA是显示的技术指标。
 
 chartSeries(SPY,subset="2014-08::2015-08",theme=chartTheme("black"),TA="addVo();addBBands();addATR();addCMF()") # 画SPY的数据，其他参数同上。
 getDividends("CHL")