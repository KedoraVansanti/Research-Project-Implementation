#Reading in the dataset
setwd("F:/Studium/MSc.Paleobiology/2.Semester/Research Project Implementation/MergeCSV/data")#DesktopPC
setwd("C:/Users/pixie/MSc.Paleobiology/2.Semester/Research Project Implementation/MergeCSV/data")#Laptop
Proj <- read.csv("AllData.csv") #read in collected data
#change coordinate unit from pixel to micrometer
Proj$Y <- Proj$Y * (1.8/32)
Proj$X <- Proj$X *(1.8/32)

#Subsetting data
#Only length (without width)
length_dat <- Proj[which(Proj$Task == "Length"),]
#cemented vs non-cemented
Cement1 <- length_dat[which(length_dat$Cement..1.No.2.Yes. == "1"),] #subsetting to non-cemented cells
Cement2 <- length_dat[which(length_dat$Cement..1.No.2.Yes. == "2"),] #subsetting to cemented cells

nrow(Cement1) #count how many cells are not cemented
nrow(Cement2) #count how many cells are cemented

# getting the mean length of both categories
mean(Cement1$Length)
mean(Cement2$Length)

#test for normal distribution of length

hist(Cement1$Length)
hist(Cement2$Length)

shapiro.test(Cement1$Length) #p-value: 0.1827
shapiro.test(Cement2$Length) #p-value:0.02854
# non-cemented cell length is not normally distributed

#wilcoxon test to check if differences in length between the two cell types are significant
wilcox.test(Length ~ Cement..1.No.2.Yes., data = length_dat)
#p-value<2.2e-16 -> differences between cemented and non-cemented cell lengths are significant

#boxplot
max(Cement1$Length)
max(Cement2$Length)
min(Cement1$Length)
min(Cement2$Length)
median(Cement1$Length)
median(Cement2$Length)


Cement1$type <- "non-cemented"
Cement2$type <- "cemented"
# Combine both types into one data frame, now with a type column to differentiate
combined_data <- rbind(Cement1, Cement2)
boxplot(Length ~ type,
        data = combined_data,
        col = c("#D81B60", "#1E88E5"),
        xlab = "cell type",
        ylab = "cell length (µm)",
        main = "cell lengths by type",
        notch=FALSE )
legend("topright",
       legend = c("cemented", "non-cemented"),
       fill = c("#D81B60", "#1E88E5"),
       title = "cell type")


#scatterplot: X and Y- coordinates plotted per cell type
x11()
plot(Proj$X, Proj$Y,
     col = ifelse(Proj$Cement..1.No.2.Yes. == "1", "#D81B60", "#1E88E5"),
     pch = 16,        
     xlab = "X coordinate (µm)", ylab = "Y coordinate (µm)",
     main = "cell coordinates by type"
     #,xlim=c(-10000, 15000)
)

legend("topleft", legend = c("non-cemented", "cemented"),
       col = c("#D81B60", "#1E88E5"), pch = 16)

#plotting length per y-coordinate
install.packages("zoo")
library("zoo") #package is needed for the "rollmean" - function

#read in chronological data
chrono_dat <- read.csv("NZ_142-2-Chronology.csv")
chrono_dat$Length <- cumsum(chrono_dat$Length) #previous length gets added to current length


x11()
length <- length_dat[order(length_dat$Y),]
plot(length$Y, length$Length,
     col= "lightgrey",
     pch = 16,
     xlab = "Y coordinate", ylab= "cell length (µm)",
     main = "length per Y coordinate"
    
)
#add rolling mean
lines(length$Y, rollmean(length$Length, k=7, fill=NA),
      col = "#07afaf", lwd = 2
)
#add chronological markers     
abline(v = chrono_dat$Length,
           col = "#FFC107",
           lty = 2, lwd =2
       
)
# Add year labels to chronological markers
text(x = chrono_dat$Length,
     y = 19.5,  #y-position of the labels
     labels = chrono_dat$Year,
     srt = 90,            # rotate text 90 degrees
     pos = 4,             # position to the right of the line
     cex = 0.8,           # text size
     col = "#FFC107"
)


legend("topright",
        legend = c("cell length", "rolling mean (k=7)", "chronological marker"),
        col = c("lightgrey", "#07afaf", "#FFC107"),
        pch = c(16, NA, NA),
        lty = c(NA, 1, 2),
        lwd = c(NA, 2, 2)
       
)

   
#calculating the area of each cell with length*width
#subsetting data
#only width (without length)
width_dat <- Proj[which(Proj$Task == "Width"),]

#Creating data frame with all relevant information (Y, length, width, cell-nr.)
df <- data.frame(1:1254) #empty dataframe with same row number as cell number
df$Cell.Nr<- length_dat$Cell.Nr
df$Y <- length_dat$Y
df$Length <- length_dat$Length
df$Width<- width_dat$Length
df$Area <- df$Length*df$Width

#plot area with y-coordinate
x11()
area <- df[order(df$Y),]
plot(area$Y, area$Area,
     col= "lightgrey",
     pch = 16, 
     xlab = "Y coordinate", ylab= "cell area (μm²)",
     main = "area per Y coordinate"
)

lines(area$Y, rollmean(area$Area, k=7, fill=NA),
      col = "#07afaf", lwd = 2
)

abline(v = chrono_dat$Length,
       col = "#FFC107",
       lty = 2,
       lwd =2
)

text(x = chrono_dat$Length,
     y = 19.5,  
     labels = chrono_dat$Year,
     srt = 90,            
     pos = 4,             
     cex = 0.8,           
     col = "#FFC107"
)

legend("topright",
       legend = c("cell area", "rolling mean (k=7)", "chronological marker"),
       col = c("lightgrey", "#07afaf", "#FFC107"),
       pch = c(16, NA, NA),
       lty = c(NA, 1, 2),
       lwd = c(NA, 2, 2)
)

######################
meanLengthperYear <- (max(chrono_dat$Length)- min(chrono_dat$Length))/30