# 对武汉和上海地区做RD分析
library(pheatmap)
library(tidyverse)
library(stargazer)
library(car)
library(rdrobust)
library(rddensity)
library(ggsci)
library(openxlsx)
library(readxl)

dt_sh = readxl::read_excel('output/dt_sh.xlsx')
dt_wh = readxl::read_excel('output/dt_wh.xlsx')

dt_sh$date = as.Date(dt_sh$date)
dt_wh$date = as.Date(dt_wh$date)


# 设置字体
library(showtext)
font_add('timesbd', '/home/lzh/personal/fonts/timesbd.ttf')
showtext_auto()
myfonts = 'timesbd'

# 将图像中的中文日期设置为英文格式
Sys.setlocale("LC_TIME", "English") 



# 重新标一下days
dt_sh %>% 
  mutate(phase = if_else(date < sh_time[2], 's1', if_else(date < sh_time[3], 's2', if_else(date < sh_time[4], 's3', 's4')))) -> dt_sh

dt_sh$days = 1:nrow(dt_sh)

sh_time <- c('2022-02-28', '2022-03-28', '2022-04-21','2022-05-16')
sh_time <- as.Date(sh_time)


# t1
c = filter(dt_sh, date == sh_time[2])$days

dt_sh_t1 = dt_sh %>% 
  filter(date < sh_time[3])

rdplot(dt_sh_t1$emo_score, dt_sh_t1$days, c = c, p = 2, 
       nbins = 20, kernel = 'tri')

covs = dt_sh_t1 %>% 
  select(new_confirmed_log, E1_Income_support,
         C6M_Stay_at_home_requirements,  H6M_Facial_Coverings)


rdrobust(dt_sh_t1$emo_score, dt_sh_t1$days, c = c, p = 2, 
         kernel = 'tri', bwselect = 'msetwo') %>% 
  summary()

# t2
c = filter(dt_sh, date == sh_time[3])$days

dt_sh_t2 = dt_sh %>% 
  filter(date < sh_time[4], date >= sh_time[2])

t2 = rdplot(dt_sh_t2$emo_score, dt_sh_t2$days, c = c, p = 2, 
       nbins = 20, kernel = 'tri')

covs = dt_sh_t2 %>% 
  select(new_confirmed_log, E1_Income_support,
         C6M_Stay_at_home_requirements,  H6M_Facial_Coverings)


rdrobust(dt_sh_t2$emo_score, dt_sh_t2$days, c = c, p = 1, covs = covs,
         kernel = 'tri', bwselect = 'msetwo') %>% 
  summary()



# t3
c = filter(dt_sh, date == sh_time[4])$days

dt_sh_t3 = dt_sh %>% 
  filter(date >= sh_time[3])

rdplot(dt_sh_t3$emo_score, dt_sh_t3$days, c = c, p = 2, 
       nbins = 20, kernel = 'tri')

covs = dt_sh_t3 %>% 
  select(new_confirmed_log, E1_Income_support,
         C6M_Stay_at_home_requirements,  H6M_Facial_Coverings)


rdrobust(dt_sh_t3$emo_score, dt_sh_t3$days, c = c, p = 1, covs = covs,
         kernel = 'tri', bwselect = 'msetwo') %>% 
  summary()


## 武汉疫情的rd分析
# 重新标一下days
dt_wh$days = 1:nrow(dt_wh)

wh_time <- c('2019-12-01', '2020-01-24','2020-02-16',  '2020-03-24')
wh_time <- as.Date(wh_time)


# t1
c = filter(dt_wh, date == wh_time[2])$days

dt_wh_t1 = dt_wh %>% 
  filter(date <= wh_time[3])

rdplot(dt_wh_t1$emo_score, dt_wh_t1$days, c = c, p = 2, 
       nbins = 20, kernel = 'tri')

covs = dt_wh_t1 %>% 
  select(new_cured_log, C1M_School_closing, C2M_Workplace_closing,  C3M_Cancel_public_events,C4M_Restrictions_on_gatherings ,C5M_Close_public_transport,
         C6M_Stay_at_home_requirements, E1_Income_support,  H6M_Facial_Coverings, H8M_Protection_of_elderly_people)


rdrobust(dt_wh_t1$emo_score, dt_wh_t1$days, c = c, p = 3, 
         kernel = 'tri', bwselect = 'msetwo') %>% 
  summary()


# RDD data生成
rd_data = rdd_data(y = dt_wh_t1$emo_score, 
                   x = dt_wh_t1$days, 
                   cutpoint = c, covar = covs)
#RDD参数估计
rd_model = rd_data %>% 
  rdd_reg_lm(slope = "separate", order=2) 
summary(rd_model)


# t2
# 国务院联防联控机制2月16日举行新闻发布会。
# 国家卫健委新闻发言人米锋介绍，截至2月15日24时，湖北、全国其他省份重症病例占确诊病例的比例均明显下降。
# 
# 据湖北省政府网站消息，2月16日，湖北发布进一步强化新冠肺炎疫情防控的通告。
# 其中指出，城乡所有村组、社区、小区、居民点实行24小时最严格的封闭式管理。
# 严管外来车辆，非必需不进出;严管外来人员，非必要不入内；
# 严管住户外出，药品和必需生活物品等可采取集中采购配送等方式进行。

c = filter(dt_wh, date == wh_time[3])$days

dt_wh_t2 = dt_wh %>% 
  filter(date < wh_time[4], date > wh_time[2])

rdplot(dt_wh_t2$emo_score, dt_wh_t2$days, c = c, p = 2, 
       nbins = 20, kernel = 'tri')

covs = dt_wh_t2 %>% 
  select(new_cured_log, C1M_School_closing,  C3M_Cancel_public_events, C5M_Close_public_transport,
         C6M_Stay_at_home_requirements,  H6M_Facial_Coverings)


rdrobust(dt_wh_t2$emo_score, dt_wh_t2$days, c = c, p = 2, covs = covs,
         kernel = 'tri', bwselect = 'msetwo') %>% 
  summary()

# t3
c = filter(dt_wh, date == wh_time[4])$days

dt_wh_t3 = dt_wh %>% 
  filter(date >= wh_time[3])

rdplot(dt_wh_t3$emo_score, dt_wh_t3$days, c = c, p = 2, nbins = 20,
       kernel = 'uni', x.label = 'Days', y.label = 'Sentiment', title = 'The third cutoff (2022-05-17)')

covs = dt_wh_t3 %>% 
  select(new_cured_log, C1M_School_closing, C2M_Workplace_closing, C3M_Cancel_public_events, C5M_Close_public_transport,
         C6M_Stay_at_home_requirements, E1_Income_support, H6M_Facial_Coverings, H8M_Protection_of_elderly_people)

rdrobust(dt_wh_t3$emo_score, dt_wh_t3$days, c = c, p = 2, covs = covs, 
         kernel = 'uni', bwselect = 'msetwo') %>% 
  summary()


# 调色盘
cols = pal_npg(palette = c("nrc"), alpha = 1)(8)

p1 = dt_sh_t1 %>%
  ggplot(aes(x = date, y = emo_score, color = phase))+ 
  geom_point(size = 2.5, ) + 
  geom_smooth(size = 2.5, method = "lm", se=T, formula = y ~ x + I(x ^ 2))+ 
  scale_color_manual(values = cols[1:2]) +
  theme_grey(base_size = 22)+
  geom_vline(xintercept =  sh_time[2], 
             linewidth=1, linetype="dashed") +
  theme_bw() + 
  theme(
    # LABLES APPEARANCE
    plot.title = element_text(size=24, face= "bold", colour= "black" , family = myfonts),
    axis.title.x = element_blank(),    
    axis.title.y = element_text(size=26, face="bold", colour = "black", family = myfonts),    
    axis.text.x = element_text(angle = 45,hjust = 1,size=22, face="bold", colour = "black", family = myfonts), 
    # axis.text.y = element_text(size=12,  colour = "black"), # unbold
    axis.text.y = element_text(size=22, face="bold", colour = "black", family = myfonts), # bold
    strip.text.x = element_text(size = 22, face="bold", colour = "black" , family = myfonts),
    strip.text.y = element_text(size = 22, face="bold", colour = "black", family = myfonts),
    axis.line.x = element_line(color="black", size = 0.3),
    axis.line.y = element_line(color="black", size = 0.3),
    panel.border = element_rect(colour = "black", fill=NA, size=0.3), 
    legend.position="bottom",
    legend.title=element_text(size=22, family = myfonts), 
    legend.text = element_text(size=20, family = myfonts)
  ) +
  scale_x_date(breaks = '2 weeks', date_labels = '%b %d')

p2 = dt_sh_t2 %>%
  ggplot(aes(x = date, y = emo_score, color = phase))+ 
  geom_point(size = 2.5) + 
  geom_smooth(size = 2.5, method = "lm", se=T, formula = y ~ x + I(x^2))+ 
  geom_vline(xintercept =  sh_time[3], 
             linewidth=1, linetype="dashed") +
  scale_color_manual(values = cols[2:3]) +
  theme_grey(base_size = 22)+
  #scale_y_continuous(limits = c(-6, 3)) +
  theme_bw() + 
  theme(
    # LABLES APPEARANCE
    plot.title = element_text(size=24, face= "bold", colour= "black" , family = myfonts),
    axis.title.x = element_blank(),    
    axis.title.y = element_blank(),    
    axis.text.x = element_text(angle = 45,hjust = 1,size=22, face="bold", colour = "black", family = myfonts), 
    # axis.text.y = element_text(size=12,  colour = "black"), # unbold
    axis.text.y = element_text(size=22, face="bold", colour = "black", family = myfonts), # bold
    strip.text.x = element_text(size = 22, face="bold", colour = "black" , family = myfonts),
    strip.text.y = element_text(size = 22, face="bold", colour = "black", family = myfonts),
    axis.line.x = element_line(color="black", size = 0.3),
    axis.line.y = element_line(color="black", size = 0.3),
    panel.border = element_rect(colour = "black", fill=NA, size=0.3), 
    legend.position="bottom",
    legend.title=element_text(size=22, family = myfonts), 
    legend.text = element_text(size=20, family = myfonts)
  )   +
  scale_x_date(breaks = '2 weeks', date_labels = '%b %d')

p3 = dt_sh_t3 %>%
  ggplot(aes(x = date, y = emo_score, color = phase))+ 
  geom_point(size = 2.5) + 
  geom_smooth(size = 2.5, method = "lm", se=T, formula = y ~ x + I(x ^ 2))+ 
  geom_vline(xintercept =  sh_time[4], 
             linewidth=1, linetype="dashed") +
  scale_color_manual(values = cols[3:4]) +
  theme_grey(base_size = 22)+
  #scale_y_continuous(limits = c(-6, 3)) +
  theme_bw() + 
  theme(
    # LABLES APPEARANCE
    plot.title = element_text(size=24, face= "bold", colour= "black" , family = myfonts),
    axis.title.x = element_blank(),    
    axis.title.y = element_blank(),    
    axis.text.x = element_text(angle = 45,hjust = 1, size=22, face="bold", colour = "black", family = myfonts), 
    # axis.text.y = element_text(size=12,  colour = "black"), # unbold
    axis.text.y = element_text(size=22, face="bold", colour = "black", family = myfonts), # bold
    strip.text.x = element_text(size = 22, face="bold", colour = "black" , family = myfonts),
    strip.text.y = element_text(size = 22, face="bold", colour = "black", family = myfonts),
    axis.line.x = element_line(color="black", size = 0.3),
    axis.line.y = element_line(color="black", size = 0.3),
    panel.border = element_rect(colour = "black", fill=NA, size=0.3), 
    legend.position="bottom",
    legend.title=element_text(size=22, family = myfonts), 
    legend.text = element_text(size=20, family = myfonts)
  )   +
  scale_x_date(breaks = '2 weeks', date_labels = '%b %d')

library(ggpubr)
ggarrange(p1, p2, p3, nrow = 1)
