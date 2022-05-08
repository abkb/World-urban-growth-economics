

setwd("C:/Users/akibria/Desktop/Kib-UofA/Arizona/Collaboration & myone/Papers/MYONE/Urban growth")


library(readxl)
library(tidyverse)
library(dplyr)

library(ggplot2)
library(car)        # multiple linear regression

library(pastecs)
library(psych)
library(cowplot)    # combining the plots 

dataBG= read_excel("GHS_STAT_V1_2_BGD.xlsx")

# Filter data file
dtafil= filter(dataBG, XC_NM_LST== "Bangladesh" & QA2_1V==1)
dtafil
lapply(dtafil, as.numeric)

#data frame 

dt_frm= data.frame(dtafil$UC_NM_MN,
                   dtafil$NTL_AV, 
                   dtafil$E_GR_AH14,dtafil$E_GR_AM14, dtafil$E_GR_AL14, dtafil$E_GR_AT14,
                   dtafil$E_GR_AH00,dtafil$E_GR_AM00, dtafil$E_GR_AL00, dtafil$E_GR_AT00,
                   
                   dtafil$E_EC2E_R15,dtafil$E_EC2E_E15, dtafil$E_EC2E_I15,dtafil$E_EC2E_A15,dtafil$E_EC2E_T15, 
                   dtafil$E_EC2E_R00,dtafil$E_EC2E_E00, dtafil$E_EC2E_I00,dtafil$E_EC2E_A00,dtafil$E_EC2E_T00,
                   
                   dtafil$EX_FD_AREA, dtafil$EX_FD_B15, dtafil$EX_FD_P15, 
                   dtafil$EX_FD_B00, dtafil$EX_FD_P00, 
                   
                   dtafil$EX_SS_AREA, dtafil$EX_SS_B15, dtafil$EX_SS_P15,
                   dtafil$EX_SS_B00, dtafil$EX_SS_P00,
                   
                   dtafil$E_CPM2_T14,
                   dtafil$E_CPM2_T00,
                   
                   dtafil$EX_HW_IDX, 
                   
                   dtafil$GDP15_SM,  dtafil$B15)

dt_frm 
options(max.print = 9999)
summary(dtafil$P00)
# transform variable

pgr= ((dtafil$P15) - (dtafil$P00))+ 12304.1 # not used 
pgr
log_pgr= log(pgr)
log_pgr

log_gdp= log(dtafil$GDP15_SM)
log_gdp
log_b= log(dtafil$B15)

b_diff= (dtafil$B15- dtafil$B00)/15    # built-area growth rate per yr
b_diff
log_b_diff= log(b_diff)
log_b_diff

       

lu_eff
summary(lu_eff)
lu_eff_postv= lu_eff+(6.752431e-06)
lu_eff_postv
log_lu_eff= log(lu_eff_postv)
log_lu_eff

pgrt= log(dtafil$P15/dtafil$P00)/15       # population growth rate 2000-2015
pgrt

summary(pgrt)
summary(dtafil$B15, dtf)


lcrt= log(dtafil$B15/dtafil$B00)/15       # land consumption rate 2000-2015
lcrt
summary(lcrt)
summary(dtafil$P15)

lu_effic= lcrt/pgrt                       # land use efficiency calculation
lu_effic

describe(lu_effic)                        # descriptive statistics of land use efficiency 2015

plot(lu_effic)

log_lu_effic= log(lu_effic)

log_ag= log(dtafil$SDG_A2G14)

gr_ah15_num= as.numeric(as.character(dtafil$E_GR_AH14))

# graph for land use efficiency 
dt_f_lu_eff= data.frame(lu_effic, lcrt, pgrt)
library(ggthemes)
ggplot(dt_f_lu_eff, aes(x= pgrt, y=lcrt, size= lu_effic,  color= lu_effic)) +
  geom_point(alpha=0.7)+
  ylab("LCR") +
  xlab("PGR") +
  theme_bw()
  

# maping on bgd 


ggplot(dt_f_lu_eff,aes(x= pgrt, y=lcrt, size = lu_effic, color= as.factor(lu_effic))) +
  geom_point(alpha=0.5, shape=21, color="black") +
  scale_size(range = c(.1, 24), name="Population (M)") +
  
  theme_ipsum() +
  theme(legend.position="bottom") +
  
  theme(legend.position = "none")



# Combine transformed data with data frame 
dtf_com= cbind(dt_frm, log_gdp, log_pgr, log_b, log_lu_effic, log_ag, gr_ah15_num)
options(max.print = 9999)
dtf_com

dtafil_com= cbind(dtafil, log_gdp, log_pgr, log_b, log_lu_effic, log_ag, gr_ah15_num)

# ## correlation plot


dt_frm_corr= data.frame(dtafil$E_CPM2_T14, dtafil$E_EC2E_I15, dtafil$E_EC2E_R15,
                        dtafil$E_GR_AM14, dtafil$E_GR_AL14, dtafil$E_GR_AT14,
                        dtafil$EX_FD_B15, dtafil$EX_FD_P15, 
                        dtafil$EX_SS_B15, dtafil$EX_SS_P15, 
                        dtafil$P15, dtafil$B15, dtafil$NTL_AV)
dt_frm_corr        # need dollar sign to import column 

dtf_urb_cor= subset(dtafil_com, select= c( E_CPM2_T14,  E_EC2E_I15,  E_EC2E_R15,
                               gr_ah15_num, E_GR_AM14,  E_GR_AL14,  
                               EX_FD_B15,  EX_FD_P15, 
                               EX_SS_B15,  EX_SS_P15, 
                               P15,  B15,  NTL_AV))         # selecting variables with subset()

dtf_urb_cor        # directly select the column name 


dtr_cor_rnm= rename(dtf_urb_cor,
  PMC= E_CPM2_T14, CO2I= E_EC2E_I15,CO2R= E_EC2E_R15,
  HGR= gr_ah15_num, MGR= E_GR_AM14, LGR= E_GR_AL14, 
  FDB= EX_FD_B15, FDP= EX_FD_P15, 
  SSB= EX_SS_B15, SSP= EX_SS_P15, 
  POP= P15, BA=B15, NTL= NTL_AV) 

dtf_urb_rnm= subset(dtr_cor_rnm, select= c(PMC, CO2I,CO2R,
                    HGR, MGR, LGR, 
                    FDB, FDP, 
                    SSB, SSP, 
                    POP, BA, NTL))
##### correlation matrix in different methods 

library(PerformanceAnalytics) # matrix method 
corr_mtx= chart.Correlation(dtr_cor_rnm, histogram = F)

library(GGally) # matrix method 
ggpairs(dt_frm_corr, aes(alpha = 0.5),displayGrid = FALSE,
        upper = list(continuous = wrap("cor", size = 2.5)))


rs_u= round(cor(dtf_urb_rnm),2)    # correlation matrix 
rs_u
upper
upper[upper.tri(rs_u)]= ""        # remove upper values of correlation matrix 
upper= as.data.frame(upper)
upper



col<- colorRampPalette(c('white',  "darkgreen"))(10)              # matrix method by heatmap 

ht_mp=heatmap(x = rs_u, col = col, symm = TRUE)
colors()

library(gplots)
ht_map= heatmap.2(x = rs_u, scale = "row", col = bluered(100), 
                  trace = "none", density.info = "none")

library(psych) # matrix method 

pairs.panels(dt_frm_corr,
             smooth = TRUE,      # If TRUE, draws loess smooths
             scale = FALSE,      # If TRUE, scales the correlation text font
             density = TRUE,     # If TRUE, adds density plots and histograms
             ellipses = TRUE,    # If TRUE, draws ellipses
             method = "pearson", # Correlation method (also "spearman" or "kendall")
             pch = 21,           # pch symbol
             lm = FALSE,         # If TRUE, plots linear fit rather than the LOESS (smoothed) fit
             cor = TRUE,         # If TRUE, reports correlations
             jiggle = FALSE,     # If TRUE, data points are jittered
             factor = 2,         # Jittering factor
             hist.col = 4,       # Histograms color
             stars = TRUE,       # If TRUE, adds significance level with stars
             ci = TRUE)          # If TRUE, adds confidence intervals

## arranging different graphs in one 

library(gridExtra)

grid.arrange(ht_mp,corr_mtx)

#### Descriptive stat
as.numeric(dtafil$E_EC2E_E15)
summary(dtf_com)


#### boxplot with p value for GDP AND POPU.
library(dplyr)
library(ggplot2)
library(ggpubr)

dtf_urb_boxp= subset(dtafil, select= c(P15, P00,  B00,  B15, GDP00_SM, GDP15_SM))
dtf_urb_boxp

Po= subset(dtafil, select= c(P00, P15)) # needed for stacking 
head(Po)
Ba= subset(dtafil, select= c(B00,  B15)) # needed for stacking
Gd= subset(dtafil, select= c(GDP00_SM, GDP15_SM)) # needed for stacking

POP= stack(Po)                # stacking pop. 00 and 15
pop_df= data.frame(POP)
head(POP)
BA= stack(Ba)                 # stacking ba. 00 and 15
ba_df= data.frame(BA)
GDP= stack(Gd)                # stacking gdp. 00 and 15
gdp_df= data.frame(GDP)
Yr_cut= c(1:170)
Yr= cut(Yr_cut, breaks = c(0, 85, 170),
labels = c("2000", "2015"), include.lowest = T)

dt_com_bx= cbind(POP, BA,GDP, Yr )
dt_com_bx



library(writexl)
# write_xlsx(as.data.frame(dt_com_bx), path= "box_pl_dt.xlsx")
dt_bx_p= read_excel("box_pl_dt.xlsx")

bxplt1= ggpaired(dt_bx_p, x = "Yr", y = "POP",
                color = "Yr", line.color = "gray", line.size = 0.4,
                palette = "jco")+
                stat_compare_means(paired = TRUE, label.y = 24900000, size= 3)+
                xlab("Year")+ ylab("Population (No.)")+
                theme(axis.title = element_text(size = 10),
                  axis.text = element_text(size = 8), legend.position = "None",
                  panel.background = element_rect(fill = NA   ))
                

bxplt1

bxplt2= ggpaired(dt_bx_p, x = "Yr", y = "BA",
                 color = "Yr", line.color = "gray", line.size = 0.4,
                 palette = "jco")+
                  stat_compare_means(paired = TRUE , label.y = 900, size= 3)+
                 xlab("")+ ylab("BA (sq. km)")+
                theme(axis.title = element_text(size = 10),
                  axis.text = element_text(size = 8), legend.position = "None",
                  panel.background = element_rect(fill = NA   ))

bxplt2

bxplt3= ggpaired(dt_bx_p, x = "Yr", y = "GDP",
                 color = "Yr", line.color = "gray", line.size = 0.4,
                 palette = "jco")+
                stat_compare_means(paired = TRUE, label.y = 101000000000, size= 3)+
                xlab("")+ ylab("GDP (US$)")+
                                theme(axis.title = element_text(size = 10),
                      axis.text = element_text(size = 8), legend.position = "None",
                      panel.background = element_rect(fill = NA ))
  
 
bxplt3

library(cowplot)

plot_grid(bxplt3, bxplt1, bxplt2, ncol=3, nrow=1, labels = c('A', 'B', 'C'), label_size = 10)

library(gridExtra)
library(grid)

grid.arrange(bxplt3, bxplt1, bxplt2, ncol=3, nrow=1)                               # combining the plots
grid.rect(width = 1, height = 1, gp = gpar(lwd = 1, col = "grey50", fill = NA))    # plot boundary color 

ggarrange(bxplt3, bxplt1, bxplt2, ncol=3, nrow=1, labels = (c('A', 'B', 'C')))     # with Fig number labels



library(latticeExtra)
c.trellis (bxplt1, bxplt2, bxplt3, ncol=3)

library(devtools)
library(lemon)
nt= theme(legend.position = 'none')

grid_arrange_shared_legend (bxplt1, arrangeGrob( bxplt2+nt, bxplt3+nt, ncol=2), nrow=1,  ncol=3)


colours()
# with pastecs library
stat.desc(dtafil)

#with psych library
library(psych)
options(max.print = 9999)
d_st= describe(dt_frm)
d_st

d_stdt= describe(dtafil)
d_stdt

mean(dt_frm$dtafil.E_GR_AH14)
summary(dt_frm$dtafil.E_GR_AM14)

num_co2a= as.numeric(as.character(dtafil$E_EC2E_A15))
describe(num_co2a)

# % of descriptive statistics 
# high green
# medium green
# low green 





## export out put in excel
install.packages("xlsx")
library(xlsx)
write.xlsx(describe(describe(dt_frm)), "tabl_res1.xlsx") # an option to export but not used 

library(writexl)
write_xlsx(as.data.frame(d_st), path= "des_results.xlsx")


####### gdp vs population growth  
model_g_pg= lm(log_gdp~ log_pgr , data = dtf_com)

summary(model_g_pg)



pl1= plot(log_pgr, log_gdp)
pl1+abline(model_g_pg)

library(ggpubr)# display the r-sq valeu and reg. equation
library(plotly)

p4 = ggplot(dtf_com, aes(x = log_pgr, y = log_gdp)) + 
  xlab("logPG") + 
  ylab("LogGDP")+
  geom_point()+
  geom_smooth(method= 'lm', color="red")+
  stat_regline_equation(label.y = 11, aes(label= ..eq.label..))+
  stat_regline_equation(label.y = 11.3, aes(label= ..rr.label..))

p4

## display text in graph 
p11 = ggplot(dtf_com, aes(x = log_pgr, y = log_gdp)) + 
            xlab("logPG") + 
            ylab("LogGDP")+
            geom_point()+
            geom_smooth(method= 'lm', color="red")+
            geom_text(aes(label=dtafil.UC_NM_MN),nudge_x = 0.1, nudge_y = 0.1, check_overlap = T)+
            stat_regline_equation(label.y = 11, aes(label= ..eq.label..))+
            stat_regline_equation(label.y = 10.7, aes(label= ..rr.label..))
            
p11

########### gdp vs built up ######
model_g_ba= lm(log_gdp~ log_b, data = dtf_com)

summary(model_g_ba)

p2 = ggplot(dtf_com, aes(x = log_gdp, y = log_b)) + 
  xlab("logBA") + 
  ylab("LogGDP")+
  geom_point()+
  geom_smooth(method= 'lm', color="red")+
  xlim(7.5, 10)+ ylim(0, 3)+
  stat_regline_equation(label.y = 3, aes(label= ..eq.label..))+
  stat_regline_equation(label.y = 2.8, aes(label= ..rr.label..))

p2

###### gdp vs land use efficiency 
model_g_lua= lm(log_gdp~ log_lu_effic, data = dtf_com)

summary(model_g_lua)

p3 = ggplot(dtf_com, aes(x = log_gdp, y = log_lu_effic)) + 
  xlab("logLUE") + 
  ylab("LogGDP")+
  geom_point()+
  geom_smooth(method= 'lm', color="red")+
  xlim(-7.25, 8)+ ylim(-4, 7)+
  stat_regline_equation(label.y = 6.8, aes(label= ..eq.label..))+
  stat_regline_equation(label.y = 6.5, aes(label= ..rr.label..))

p3



### gdp vs access to green 
model_g_ag= lm(log_gdp~ log_ag, data = dtf_com)

summary(model_g_ag)

p5 = ggplot(dtf_com, aes(x = log_gdp, y = log_ag)) + 
  xlab("logAG") + 
  ylab("logGDP")+
  geom_point()+
  geom_smooth(method= 'lm', color="red")+
  xlim(7.25, 20)+ ylim(4, 20)+
  stat_regline_equation(label.y = 6.8, aes(label= ..eq.label..))+
  stat_regline_equation(label.y = 6.5, aes(label= ..rr.label..))

p5


##### land use efficiency 
#90-00
pg_9000= log(dtafil$P00/dtafil$P90)/10

pg_9000

lc_rt_9000= log(dtafil$B00/dtafil$B90)/10
lc_rt_9000

lnd_effic_9000= lc_rt_9000/pg_9000
lnd_effic_9000

# range for summary statistics 
po15 = cut(dtafil$P15, breaks = seq(50000, 30000000, by = 1000000), include.lowest=TRUE)
po00 = cut(dtafil$P00, breaks = seq(50000, 30000000, by = 1000000), include.lowest=TRUE)

describe(po00)   # proportion offers value of percentage 
describe(po15)


b15 = cut(dtafil$B15, breaks = seq(0.1, 900, by = 50), include.lowest=TRUE)
b15
describe(b15)

b00 = cut(dtafil$B00, breaks = seq(0.1, 900, by = 49.9), include.lowest=TRUE)
b00
describe(b00)


# 00-15
pg_0015= log(dtafil$P15/dtafil$P00)/15

pg_0015

mean(pg_0015)
lc_rt_0015= log(dtafil$B15/dtafil$B00)/15
lc_rt_0015

lnd_effic_0015= lc_rt_0015/pg_0015
lnd_effic_0015

### t-test
as.numeric(dtafil$NTL_AV, 
           dtafil$E_GR_AH14,dtafil$E_GR_AM14, dtafil$E_GR_AL14, dtafil$E_GR_AT14,
           dtafil$E_GR_AH14,dtafil$E_GR_AM00, dtafil$E_GR_AL00, dtafil$E_GR_AT00,
           
           dtafil$E_EC2E_R15,dtafil$E_EC2E_E15, dtafil$E_EC2E_I15,dtafil$E_EC2E_A15,dtafil$E_EC2E_T15, 
           dtafil$E_EC2E_R00,dtafil$E_EC2E_E00, dtafil$E_EC2E_I00,dtafil$E_EC2E_A00,dtafil$E_EC2E_T00,
           
           dtafil$EX_FD_AREA, dtafil$EX_FD_B15, dtafil$EX_FD_P15, 
           dtafil$EX_FD_B00, dtafil$EX_FD_P00, 
           
           dtafil$EX_SS_AREA, dtafil$EX_SS_B15, dtafil$EX_SS_P15,
           dtafil$EX_SS_B00, dtafil$EX_SS_P00,
           
           dtafil$E_CPM2_T14,
           dtafil$E_CPM2_T00,)  

as.numeric(dtafil$NTL_AV, dtafil$E_GR_AH14, dtafil$E_GR_AM14) # for removing error in non-numeric data 
as.numeric (dtafil$E_EC2E_E00,dtafil$E_EC2E_E15)

#####Kruskal tests### since the normality assimption is not required  
gr_ah00_num= as.numeric(as.character(dtafil$E_GR_AH00)) # convert to numeric 
gr_ah15_num= as.numeric(as.character(dtafil$E_GR_AH14)) # convert to numeric 
wilcox.test (gr_ah00_num, gr_ah15_num, paired = T) # high green (dense veg.)

describe(gr_ah00_num)
describe(gr_ah15_num)
view(gr_ah15_num)
gr_av_num= as.numeric(as.character(dtafil$E_GR_AV14))
cor(gr_av_num, dtafil$P15, method = "pearson")

#
wilcox.test(dtafil$E_GR_AM14,dtafil$E_GR_AM00, paired = T) #  medium green
wilcox.test(dtafil$E_GR_AL00, dtafil$E_GR_AL14, paired = T) # low green 
wilcox.test(dtafil$E_GR_AT14,dtafil$E_GR_AT00, paired = T) # total green

describe(dtafil$E_GR_AM14)
describe(dtafil$E_GR_AT00)

#
wilcox.test(dtafil$E_EC2E_R00,dtafil$E_EC2E_R15, paired = T) # co2 emission from residential area

describe(dtafil$E_EC2E_R15)
#
co2_e00_num= as.numeric(as.character(dtafil$E_EC2E_E00)) # convert to numeric  
co2_e15_num= as.numeric(as.character(dtafil$E_EC2E_E15)) # convert to numeric 
wilcox.test(co2_e00_num,co2_e15_num, paired = T) # co2 emission from energy 
describe(co2_e00_num)
describe(co2_e15_num)
#
EC2E_A00_num = as.numeric(as.character(dtafil$E_EC2E_A00)) # convert to numeric 
EC2E_A15_num= as.numeric(as.character(dtafil$E_EC2E_A15)) # convert to numeric 
wilcox.test(EC2E_A00_num,EC2E_A15_num, paired = T) # co2 emission from agri sector

describe(EC2E_A00_num)
describe(EC2E_A15_num)
#
wilcox.test(dtafil$E_EC2E_I00,dtafil$E_EC2E_I15, paired = T) # co2 emission from Industry

#
co2_t00_num= as.numeric(as.character(dtafil$E_EC2E_T00)) # convert to numeric  
co2_t15_num= as.numeric(as.character(dtafil$E_EC2E_T15)) # convert to numeric 
wilcox.test(co2_t00_num,co2_t15_num, paired = T) # co2 emission from transport  


describe(co2_t00_num)
describe(co2_t15_num)
#
wilcox.test(dtafil$E_CPM2_T00, dtafil$E_CPM2_T14, paired = T) # concentration of pm2.5 

#
wilcox.test(dtafil$EX_FD_P15,dtafil$EX_FD_P00, paired = T) # population flood exposure
wilcox.test(dtafil$EX_FD_B00,dtafil$EX_FD_B15, paired = T) # built area flood exposure

describe(dtafil$EX_FD_P15)
describe(dtafil$EX_FD_B00)
#
wilcox.test(dtafil$EX_SS_B15,dtafil$EX_SS_B00, paired = T) # built area storm surge exposure 
wilcox.test(dtafil$EX_SS_P15,dtafil$EX_SS_P00, paired = T) # population storm surge exposure 
describe(dtafil$EX_SS_B15)

#### effects size ############## Cliff Delta is used for non-nomal data effects size calculation 
library(effsize)

gr_ah00_num= as.numeric(as.character(dtafil$E_GR_AH00)) # convert to numeric 
gr_ah15_num= as.numeric(as.character(dtafil$E_GR_AH14)) # convert to numeric 
cliff.delta(gr_ah00_num, gr_ah15_num)# high green (dense veg.)
#
cliff.delta(dtafil$E_GR_AM14,dtafil$E_GR_AM00) #  medium green
cliff.delta(dtafil$E_GR_AL00, dtafil$E_GR_AL14) # low green 
cliff.delta(dtafil$E_GR_AT14,dtafil$E_GR_AT00) # total green 

#
cliff.delta(dtafil$E_EC2E_R00,dtafil$E_EC2E_R15) # co2 emission from residential area

#
co2_e00_num= as.numeric(as.character(dtafil$E_EC2E_E00)) # convert to numeric  
co2_e15_num= as.numeric(as.character(dtafil$E_EC2E_E15)) # convert to numeric 
cliff.delta(co2_e00_num,co2_e15_num) # co2 emission from energy 

#
EC2E_A00_num = as.numeric(as.character(dtafil$E_EC2E_A00)) # convert to numeric 
EC2E_A15_num= as.numeric(as.character(dtafil$E_EC2E_A15)) # convert to numeric 
cliff.delta(EC2E_A00_num,EC2E_A15_num) # co2 emission from agri sector


#
cliff.delta(dtafil$E_EC2E_I00,dtafil$E_EC2E_I15) # co2 emission from Industry

#
co2_t00_num= as.numeric(as.character(dtafil$E_EC2E_T00)) # convert to numeric  
co2_t15_num= as.numeric(as.character(dtafil$E_EC2E_T15)) # convert to numeric 
cliff.delta(co2_t00_num,co2_t15_num) # co2 emission from transport  

#
cliff.delta(dtafil$E_CPM2_T00, dtafil$E_CPM2_T14) # concentration of pm2.5 

#
cliff.delta(dtafil$EX_FD_P15,dtafil$EX_FD_P00) # population flood exposure
cliff.delta(dtafil$EX_FD_B00,dtafil$EX_FD_B15) # built area flood exposure

#
cliff.delta(dtafil$EX_SS_B15,dtafil$EX_SS_B00) # built area storm surge exposure 
cliff.delta(dtafil$EX_SS_P15,dtafil$EX_SS_P00) # population storm surge exposure 





#### t- tests ########### (abandoned as the data were not normally distributed)-
#
gr_ah00_num= as.numeric(as.character(dtafil$E_GR_AH00)) # convert to numeric 
gr_ah15_num= as.numeric(as.character(dtafil$E_GR_AH14)) # convert to numeric 
t.test(gr_ah00_num, gr_ah15_num, paired = T) # high green (dense veg.)

#
t.test(dtafil$E_GR_AM14,dtafil$E_GR_AM00, paired = T) #  medium green
t.test(dtafil$E_GR_AL00, dtafil$E_GR_AL14, paired = T) # low green 
t.test(dtafil$E_GR_AT14,dtafil$E_GR_AT00, paired = T) # total green

#
t.test(dtafil$E_EC2E_R00,dtafil$E_EC2E_R15, paired = T) # co2 emission from residential area
lnr00= log(dtafil$E_EC2E_R00) # log transformation 
lnr15= log(dtafil$E_EC2E_R15) # log transformation
shapiro.test(lnr00)
ggqqplot(lnr00)
t.test(lnr00, lnr15, paired = T) # t-test after log transformation

#
co2_e00_num= as.numeric(as.character(dtafil$E_EC2E_E00)) # convert to numeric  
co2_e15_num= as.numeric(as.character(dtafil$E_EC2E_E15)) # convert to numeric 
t.test(co2_e00_num,co2_e15_num, paired = T) # co2 emission from industry 

#
EC2E_A00_num = as.numeric(as.character(dtafil$E_EC2E_A00)) # convert to numeric 
EC2E_A15_num= as.numeric(as.character(dtafil$E_EC2E_A15)) # convert to numeric 
t.test(EC2E_A00_num,EC2E_A15_num, paired = T) # co2 emission from agri sector

describe(EC2E_A00_num)
describe(EC2E_A15_num)
#
t.test(dtafil$E_EC2E_I00,dtafil$E_EC2E_I15, paired = T) # co2 emission from energy 

#
co2_t00_num= as.numeric(as.character(dtafil$E_EC2E_T00)) # convert to numeric  
co2_t15_num= as.numeric(as.character(dtafil$E_EC2E_T15)) # convert to numeric 
t.test(co2_t00_num,co2_t15_num, paired = T) # co2 emission from transport  

#
t.test(dtafil$E_CPM2_T00, dtafil$E_CPM2_T14, paired = T) # concentration of pm2.5 

#
t.test(dtafil$EX_FD_P15,dtafil$EX_FD_P00, paired = T) # population flood exposure
t.test(dtafil$EX_FD_B00,dtafil$EX_FD_B15, paired = T) # built area flood exposure

#
t.test(dtafil$EX_SS_B15,dtafil$EX_SS_B00, paired = T) # built area storm surge exposure 
t.test(dtafil$EX_SS_P15,dtafil$EX_SS_P00, paired = T) # population storm surge exposure 
ggqqplot(dtafil$EX_SS_P15)


##### effect size calculation ########### cohen's d is used for effect size of nomal distribution 
library(effsize)

cohen.d(dtafil$E_EC2E_I00,dtafil$E_EC2E_I15) # effect size industry co2 emission
cohen.d(gr_ah00_num, gr_ah15_num, paired = T) # high green (dense veg.)

#
cohen.d(dtafil$E_GR_AM14,dtafil$E_GR_AM00, paired = T) #  medium green
cohen.d(dtafil$E_GR_AL00, dtafil$E_GR_AL14, paired = T) # low green 
cohen.d(dtafil$E_GR_AT14,dtafil$E_GR_AT00, paired = T) # total green

#
cohen.d(dtafil$E_EC2E_R00,dtafil$E_EC2E_R15, paired = T) # co2 emission from residential area

#
co2_e00_num= as.numeric(as.character(dtafil$E_EC2E_E00)) # convert to numeric  
co2_e15_num= as.numeric(as.character(dtafil$E_EC2E_E15)) # convert to numeric
is.na(co2_e00_num)
is.na(co2_e15_num)
e00= na.omit(co2_e00_num)
e15=na.omit(co2_e15_num)
cohen.d(e00,e15) # co2 emission from energy 
shapiro.test(e00)
library(ggpubr)
ggqqplot(e00)

#
EC2E_A00_num = as.numeric(as.character(dtafil$E_EC2E_A00)) # convert to numeric 
E_EC2E_A15_num= as.numeric(as.character(dtafil$E_EC2E_A15)) # convert to numeric 
cohen.d(EC2E_A00_num,E_EC2E_A15_num, paired = T) # co2 emission from agri sector

#
cohen.d(dtafil$E_EC2E_I00,dtafil$E_EC2E_I15, paired = T) # co2 emission from industry 

#
co2_t00_num= as.numeric(as.character(dtafil$E_EC2E_T00)) # convert to numeric  
co2_t15_num= as.numeric(as.character(dtafil$E_EC2E_T15)) # convert to numeric 
cohen.d(co2_t00_num,co2_t15_num, paired = T) # co2 emission from transport  

#
cohen.d(dtafil$E_CPM2_T00, dtafil$E_CPM2_T14, paired = T) # concentration of pm2.5 

#
cohen.d(dtafil$EX_FD_P15,dtafil$EX_FD_P00, paired = T) # population flood exposure
cohen.d(dtafil$EX_FD_B00,dtafil$EX_FD_B15, paired = T) # built area flood exposure

#
cohen.d(dtafil$EX_SS_B15,dtafil$EX_SS_B00, paired = T) # built area storm surge exposure 
cohen.d(dtafil$EX_SS_P15,dtafil$EX_SS_P00, paired = T) # population storm surge exposure 


#### multiple graphs mixing 
library(gridExtra)

grid.arrange(p2,p4, ncol=2)






msf= st_as_sf(urb_dtf, coords = c('dtafil.GCPNT_LON', 'dtafil.GCPNT_LAT'))
msf
ggplot(msf) + 
  geom_sf(aes(color = lu_effic))


ggplot() +  geom_point(data=urb_dtf, aes(x=dtafil.GCPNT_LON, y=dtafil.GCPNT_LAT), color="red")
 + ggplot()+ geom_sf(data = shp_dt)

ggplot(shp_dt)+ geom_sf()


dfi= fortify(shp_dt)
head(dfi)

names(urb_dtf)
ggplot()+ geom_point(data = urb_dtf, aes(x= dtafil.GCPNT_LON, y= dtafil.GCPNT_LAT, group= lu_effic))+
  geom_polygon(lu_effic)+ 
  geom_path()

lucls= cut(urb_dtf$lu_effic, breaks = c(-3,0, 10,20,Inf), 
           labels = c( ' -3 -0', '0-10', '10-20','>20' ))
library(tidyverse)

sep_coord= shp_dt %>%
  mutate(lat = unlist(map(shp_dt$geometry ,1)),
         long = unlist(map(shp_dt$geometry,2)))


sep_coordn=as.data.frame(st_coordinates(shp_dt))
sep_coordn




ggplot()+ geom_sf(data = shp_dt)+ 
            geom_point(data=urb_dtf, aes(x=dtafil.GCPNT_LON, y=dtafil.GCPNT_LAT), color="red")+
  geom_polygon(data = urb_dtf, aes(dtafil.GCPNT_LON, dtafil.GCPNT_LAT, group = lu_effic, fill = lu_effic))

ggplot()+ geom_sf(data = shp_dt)+ 
    geom_polygon(data = urb_dtf, aes(dtafil.GCPNT_LON, dtafil.GCPNT_LAT, group = lu_effic, fill = lu_effic))


ggplot()+ geom_sf(data = shp_dt, aes(fill= lu_effic))



describe(urb_dtf$lu_effic)






##
g1= ggplot(data= dtf_com, aes(x=log_pgr, y=log_gdp)+ xlab("logPG")+ ylab("logGDP")+ geom_point())
g1
# mean calculation

nud=as.numeric(dataBG$E_EC2E_A15)
mean(nud, na.rm = T)

summary(nud, na.rm=T)

# add new varibale
dataBG1 = mutate(dataBG, Den_15= P15/AREA)
dataBG2 = mutate(dataBG1, Den_cls= case_when(Den_15<3000~ 'small',
                                             Den_15<5000~ 'med',
                                             Den_15>5000~ 'large'))

summary(dataBG1$Den_15)

manwhit=  data.frame( 
  group = rep(c("90", "15"), each=60),
  weight = c(dtafil$P90,  dtafil$P15)
)

# Normality test
# GGPlot 

f1 = ggplot(data = dataBG2, mapping = aes(x = Den_15, y = GDP15_SM, colors())) +
  geom_point(aes(col= Den_cls)) + xlab("Den_15") + ylab("GDP 2015") 

f1

# non parametric test for two samples 
wilcox.test(dtafil$P90,  dtafil$P15)
############ rough 











