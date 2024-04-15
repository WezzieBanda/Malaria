library("ggplot2")
#head(`data_for_plot (1)`)
#source('plotting_functions.R')
library(readxl) 
library(readr)
EHTdata <- read_csv("Wezzie-Tom/EHTdata.csv")
View(EHTdata)

# Load synthetic data. This could be replaced with a real dataset of interest
# Data is for a 7-arm trial, with one full rotation (343 data points)
#df <- read_excel('EHTdata.xlsx')
#read_excel("path/to/your/excel/file.xlsx")
# In addition to the untreated control, trial contains 3 ITNs, N1, N2, N3. 
# Both Washed ('w') & Unwashed ('u') ITNs are included in the trial, making 7 arms in total
table(EHTdata$treatment)

# I prefer to have a common range on the y axis for all 3 of these panels. This is done by selecting the value of 'mx' 
#mx <- max(df[df$treatment=='C'|df$treatment=='N1u'|df$treatment=='N1w',]$total)

mx <- max(EHTdata[EHTdata$treatment=='IG1'|EHTdata$treatment=='1G2'|EHTdata$treatment=='OP'|EHTdata$treatment=='P2'|EHTdata$treatment=='RG'|EHTdata$treatment=='UT',]$total)


pnel1 <- function(EHTdata, arm = 'IG2', arm_title = 'ITN', mx = mx,
                  leg_end = 0, legX = 0.8, legY = 0.8, pie = 1, pieX = 0.1, pieY = 0.56){
  dataCB5 <- dplyr::select(EHTdata, c('day','treatment','unf_live','unf_dead','bf_live','bf_dead'))
  
  minn <- min(dataCB5$day) - 0.5
  maxx <- max(dataCB5$day) + 0.5
  
  dataCB5m <- melt(dataCB5, id.vars = c('day','treatment'))
  
  levels(dataCB5m$variable) <- c('Unfed Alive','Unfed Dead','Fed Alive','Fed Dead')
  
  dataCB5m$variable <- ordered(dataCB5m$variable, 
                               levels = c('Unfed Dead','Fed Dead', 'Unfed Alive','Fed Alive'))
  }





pnel1(EHTdata, arm = 'IG1', arm_title = 'ITN', mx=mx, pieX = 0.6)
pnel1(dataa = df, arm = 'N1u', arm_title = 'ITN (Unwashed)', mx=mx, pieX = 0.6)
pnel1(dataa = df, arm = 'N1w', arm_title = 'ITN (Washed)', mx=mx, pieX = 0.6)

pnel1(EHTdata, arm = 'IG1', arm_title = 'ITN', mx=mx, pieX = 0.6)
pnel1(EHTdata, arm = 'IG2', arm_title = 'ITN', mx=mx, pieX = 0.6)
pnel1(EHTdata, arm = 'OP', arm_title = 'ITN', mx=mx, pieX = 0.6)



error_bar_prop(dataa = df, arm = 'IG1', arm_title = 'ITN (Unwashed)')
error_bar_prop(dataa = df, arm = 'IG2', arm_title = 'ITN (Washed)')

#Is there evidence for deterrence?
tapply(df$total, df$treatment, mean)

bfi(dataa = df, arm1 = 'C', arm2 = 'N1u', arm3 = 'N1w',
    arm_label1 = 'Control', arm_label2 = 'ITN Unwashed', arm_label3 = 'ITN Washed')
#Note how the percentages change, once deterrence is turned on (denominator changes)
bfi(dataa = df, deterr = 1, arm1 = 'C', arm2 = 'N1u', arm3 = 'N1w',
    arm_label1 = 'Control', arm_label2 = 'ITN Unwashed', arm_label3 = 'ITN Washed')

#Note: if deterr = 1, you'd need a legend ('leg_end = 1') in the top row (I often choose top right panel)
#Here, ITN isn't specified in the labels. But could change to e.g. 'IG2 (Unwashed)'
plot_grid(pnel1(dataa = df, arm = 'C', arm_title = 'Control', mx=mx, pieX = 0.56),
          pnel1(dataa = df, arm = 'N1u', arm_title = 'ITN (Unwashed)', mx=mx, pieX = 0.56),
          pnel1(dataa = df, arm = 'N1w', arm_title = 'ITN (Washed)', mx=mx, leg_end = 0, pieX = 0.56),
          bfi(dataa = df, deterr = 1, arm1 = 'C', arm2 = 'N1u', arm3 = 'N1w', 
              arm_label1 = 'Control', arm_label2 = 'ITN (Unwashed)', arm_label3 = 'ITN (Washed)'),
          error_bar_prop(dataa = df, arm = 'N1u', arm_title = 'ITN (Unwashed)'),
          error_bar_prop(dataa = df, arm = 'N1w', arm_title = 'ITN (Washed)'),
          nrow=2, labels = c('a','b','c','d','e','f'))
# Saved figure may need to be quite big, to fit everything in!!
#ggsave('Six_panel_figure2.pdf',height = 12.0, width = 17.0)