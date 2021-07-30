#If God is for us, who can be against us

#Libraries
library(psych)
library(RColorBrewer)
library(VIM)
library(tree)
library(randomForest)
library(caret)
library(ROCR)
library(rpart)
library(rpart.plot)
library(party)

#Loading the data
mush = read.csv("C:/Users/tonyk/Documents/DSP 34/Task/DecisionTree/mushrooms.csv",
                stringsAsFactors = TRUE)

#Getting the dimensions of the data
dim(mush)
'8124   23'

#Checking is there any missing values
sapply(mush, function(x) sum(is.na(x)))
sapply(mush, function(x) sum(is.null(x)))
"No Missing values"

#Getting the variable names
colnames(mush)
'[1] "class"                    "cap.shape"               
 [3] "cap.surface"              "cap.color"               
 [5] "bruises"                  "odor"                    
 [7] "gill.attachment"          "gill.spacing"            
 [9] "gill.size"                "gill.color"              
[11] "stalk.shape"              "stalk.root"              
[13] "stalk.surface.above.ring" "stalk.surface.below.ring"
[15] "stalk.color.above.ring"   "stalk.color.below.ring"  
[17] "veil.type"                "veil.color"              
[19] "ring.number"              "ring.type"               
[21] "spore.print.color"        "population"              
[23] "habitat"'

#Target variable - class
'edible=e, poisonous=p'
str(mush$class)
'Factor w/ 2 levels "e","p": 2 1 1 2 1 1 1 1 2 1 ...'

cl_tab = table(mush$class)
print(cl_tab)
'   e    p 
 4208 3916 '

4208+3916

0.47*8124

3818

8124-3818

3916-3818

98/4208

#Barplot
barplot(cl_tab,
        names.arg = c('Edible', 'Poisonous'),
        col = c('forestgreen','firebrick'),
        main = 'Class of a Mushroom')

#cap.shape  
'bell=b,conical=c,convex=x,flat=f, knobbed=k,sunken=s'
str(mush$cap.shape)
'Factor w/ 6 levels "b","c","f","k",..: 6 6 1 6 6 6 1 1 6 1 ...'

sh_tab = table(mush$cap.shape)
print(sh_tab)
'   b    c    f    k    s    x 
 452    4 3152  828   32 3656 '

#Barplot
barplot(sh_tab,
        names.arg = c('Bell', 'Conical', 'Flat', 'Knobbed', 'Sunken', 'Convex'),
        col = brewer.pal(6, 'Accent'),
        main = "Barplot of Mushroom's Cap Shape")

#Dividing the data as per class
mush_p = mush[mush$class=='p',]
mush_e = mush[mush$class=='e',]

#Dividing plot areas `1 row 2 columns`
par(mfrow = c(1,2))

print(table(mush_p$cap.shape))
'   b    c    f    k    s    x 
  48    4 1556  600    0 1708 '

barplot(table(mush_p$cap.shape),
        names.arg = c('Bell', 'Conical', 'Flat', 'Knobbed', 'Sunken', 'Convex'),
        col = brewer.pal(6, 'Dark2'),
        main = 'Poisonous Mushroom', las = 2)

print(table(mush_e$cap.shape))
'   b    c    f    k    s    x 
 404    0 1596  228   32 1948 '

barplot(table(mush_e$cap.shape),
        names.arg = c('Bell', 'Conical', 'Flat', 'Knobbed', 'Sunken', 'Convex'),
        col = brewer.pal(6, 'Dark2'),
        main = 'Edible Mushroom', las = 2)

par(mfrow = c(1,1))

#cap.surface
'fibrous=f,grooves=g,scaly=y,smooth=s'

str(mush$cap.surface)
'Factor w/ 4 levels "f","g","s","y": 3 3 3 4 3 4 3 4 4 3 ...'

#Barplot
print(table(mush$cap.surface))
'   f    g    s    y 
 2320    4 2556 3244 '

barplot(table(mush$cap.surface),
        names.arg = c('Fibrous','Grooves','Smooth', 'Scaly'),
        col = brewer.pal(4, 'YlOrRd'),
        main = "Barplot of Mushroom's Cap Shape")

#Dividing plot areas `1 row 2 columns`
par(mfrow = c(1,2))

print(table(mush_p$cap.surface))
'   f    g    s    y 
  760    4 1412 1740 '

barplot(table(mush_p$cap.surface),
        names.arg = c('Fibrous','Grooves','Smooth', 'Scaly'),
        col = brewer.pal(4, 'YlOrRd'),
        main = 'Poisonous Mushroom', las = 2)

print(table(mush_e$cap.surface))
'   f    g    s    y 
 1560    0 1144 1504 '

barplot(table(mush_e$cap.surface),
        names.arg = c('Fibrous','Grooves','Smooth', 'Scaly'),
        col = brewer.pal(4, 'YlOrRd'),
        main = 'Edible Mushroom', las = 2)

par(mfrow = c(1,1))

#cap.color
'brown=n,buff=b,cinnamon=c,gray=g,green=r,pink=p,purple=u,red=e,white=w,yellow=y'

str(mush$cap.color)
'Factor w/ 10 levels "b","c","e","g",..: 5 10 9 9 4 10 9 9 9 10 ...'

#Barplot
print(table(mush$cap.color))
'   b    c    e    g    n    p    r    u    w    y 
  168   44 1500 1840 2284  144   16   16 1040 1072 '

barplot(table(mush$cap.color),
        names.arg = c('buff','cinnamon','red','gray','brown','pink','green','purple','white','yellow'),
        col = c('lightgoldenrod','sienna','red','gray','brown','pink','green','purple','white','yellow'),
        main = "Barplot of Mushroom's Cap Color", las = 2)

#Dividing plot areas `1 row 2 columns`
par(mfrow = c(1,2))

print(table(mush_p$cap.color))
'   b    c    e    g    n    p    r    u    w    y 
  120   12  876  808 1020   88    0    0  320  672 '

barplot(table(mush_p$cap.color),
        names.arg = c('buff','cinnamon','red','gray','brown','pink','green','purple','white','yellow'),
        col = c('lightgoldenrod','sienna','red','gray','brown','pink','green','purple','white','yellow'),
        main = 'Poisonous Mushroom', las = 2)

print(table(mush_e$cap.color))
'   b    c    e    g    n    p    r    u    w    y 
   48   32  624 1032 1264   56   16   16  720  400 '

barplot(table(mush_e$cap.color),
        names.arg = c('buff','cinnamon','red','gray','brown','pink','green','purple','white','yellow'),
        col = c('lightgoldenrod','sienna','red','gray','brown','pink','green','purple','white','yellow'),
        main = 'Edible Mushroom', las = 2)

par(mfrow = c(1,1))

#bruises
'bruises=t,no=f'
str(mush$bruises)
'Factor w/ 2 levels "f","t": 2 2 2 2 1 2 2 2 2 2 ...'

#Barplot
print(table(mush$bruises))
'   f    t 
 4748 3376'

barplot(table(mush$bruises),
        names.arg = c('No Bruises','Bruises'),
        col = c('steelblue', 'tan3'),
        main = 'Barplot of Mushroom Bruises')

#Dividing plot areas `1 row 2 columns`
par(mfrow = c(1,2))

print(table(mush_p$bruises))
'   f    t 
 3292  624 '

barplot(table(mush_p$bruises),
        names.arg = c('No Bruises','Bruises'),
        col = c('steelblue', 'tan3'),
        main = 'Poisonous Mushroom')

print(table(mush_e$bruises))
'   f    t 
 1456 2752'

barplot(table(mush_e$bruises),
        names.arg = c('No Bruises','Bruises'),
        col = c('steelblue', 'tan3'),
        main = 'Edible Mushroom')

par(mfrow = c(1,1))

#odor
'almond=a,anise=l,creosote=c,fishy=y,foul=f,musty=m,none=n,pungent=p,spicy=s
'
str(mush$odor)
'Factor w/ 9 levels "a","c","f","l",..: 7 1 4 7 6 1 1 4 7 1 ...'

#Barplot
print(table(mush$odor))
'   a    c    f    l    m    n    p    s    y 
  400  192 2160  400   36 3528  256  576  576 '

barplot(table(mush$odor),
        names.arg = c('Almond','Creosote','Foul','Anise','Musty','None',
                      'Pungent','Spicy','Fishy'),
        col = brewer.pal(10,'Dark2'),
        main = 'Barplot of Mushroom odor', las=2)

#Dividing plot areas `1 row 2 columns`
par(mfrow = c(1,2))

print(table(mush_p$odor))
'   a    c    f    l    m    n    p    s    y 
    0  192 2160    0   36  120  256  576  576 '

barplot(table(mush_p$odor),
        names.arg = c('Almond','Creosote','Foul','Anise','Musty','None',
                      'Pungent','Spicy','Fishy'),
        col = brewer.pal(9,'Set1'),
        main = 'Poisonous Mushroom', las=2)

print(table(mush_e$odor))
'   a    c    f    l    m    n    p    s    y 
  400    0    0  400    0 3408    0    0    0 '

barplot(table(mush_e$odor),
        names.arg = c('Almond','Creosote','Foul','Anise','Musty','None',
                      'Pungent','Spicy','Fishy'),
        col = brewer.pal(9,'Set1'),
        main = 'Edible Mushroom', las=2)

par(mfrow = c(1,1))

#gill.attachment
'attached=a,descending=d,free=f,notched=n'

str(mush$gill.attachment)
'Factor w/ 2 levels "a","f": 2 2 2 2 2 2 2 2 2 2 ...'

#Barplot
print(table(mush$gill.attachment))
'   a    f 
  210 7914 '

barplot(table(mush$gill.attachment),
        names.arg = c('Attached','Free'),
        col = c('orangered', 'orchid3'),
        main = 'Barplot of Mushroom Gill Attachment')

#Dividing plot areas `1 row 2 columns`
par(mfrow = c(1,2))

print(table(mush_p$gill.attachment))
'   a    f 
   18 3898'

barplot(table(mush_p$gill.attachment),
        names.arg = c('Attached','Free'),
        col = c('orangered', 'orchid3'),
        main = 'Poisonous Mushroom')

print(table(mush_e$gill.attachment))
'   a    f 
  192 4016'

barplot(table(mush_e$gill.attachment),
        names.arg = c('Attached','Free'),
        col = c('orangered', 'orchid3'),
        main = 'Edible Mushroom')

par(mfrow = c(1,1))

#gill.spacing
'close=c,crowded=w,distant=d'

str(mush$gill.spacing)
'Factor w/ 2 levels "c","w": 1 1 1 1 2 1 1 1 1 1 ...'

#Barplot
print(table(mush$gill.spacing))
'   c    w 
 6812 1312'

barplot(table(mush$gill.spacing),
        names.arg = c('Close', 'Crowded'),
        col = c('red3','slateblue'),
        main = 'Barplot of Mushroom Gill Spacing')

#Dividing plot areas `1 row 2 columns`
par(mfrow = c(1,2))

print(table(mush_p$gill.spacing))
'   c    w 
 3804  112 '

barplot(table(mush_p$gill.spacing),
        names.arg = c('Close', 'Crowded'),
        col = c('red3','slateblue'),
        main = 'Poisonous Mushroom')

print(table(mush_e$gill.spacing))
'   c    w 
 3008 1200'

barplot(table(mush_e$gill.spacing),
        names.arg = c('Close', 'Crowded'),
        col = c('red3','slateblue'),
        main = 'Edible Mushroom')

par(mfrow = c(1,1))

#gill.size
'broad=b,narrow=n'

str(mush$gill.size)
'Factor w/ 2 levels "b","n": 2 1 1 2 1 1 1 1 2 1 ...'

#Barplot
print(table(mush$gill.size))
'   b    n 
 5612 2512'

barplot(table(mush$gill.size),
        names.arg = c('Broad', 'Narrow'),
        col = c('royalblue4','seagreen4'),
        main = 'Barplot of Mushroom Gill Size')

#Dividing plot areas `1 row 2 columns`
par(mfrow = c(1,2))

print(table(mush_p$gill.size))
'   b    n 
 1692 2224'

barplot(table(mush_p$gill.size),
        names.arg = c('Broad', 'Narrow'),
        col = c('royalblue4','seagreen4'),
        main = 'Poisonous Mushroom')

print(table(mush_e$gill.size))
'   b    n 
3920  288'

barplot(table(mush_e$gill.size),
        names.arg = c('Broad', 'Narrow'),
        col = c('royalblue4','seagreen4'),
        main = 'Edible Mushroom')

par(mfrow = c(1,1))

#gill.color
'black=k,brown=n,buff=b,chocolate=h,gray=g, green=r,orange=o,pink=p,purple=u,
red=e,white=w,yellow=y'

str(mush$gill.color)
'Factor w/ 12 levels "b","e","g","h",..: 5 5 6 6 5 6 3 6 8 3 ...'

#Barplot
print(table(mush$gill.color))
'   b    e    g    h    k    n    o    p    r    u    w    y 
1728   96  752  732  408 1048   64 1492   24  492 1202   86'

barplot(table(mush$gill.color),
        names.arg = c('Buff', 'Red','Gray','Chocolate','Black','Brown',
                      'Orange','Pink','Green','Purple','White','Yellow'),
        col = c('lightgoldenrod', 'red','gray','chocolate','black','brown',
                'orange','pink','green','purple','white','yellow'),
        main = 'Barplot of Mushroom Gill Color', las=2)

#Dividing plot areas `1 row 2 columns`
par(mfrow = c(1,2))

print(table(mush_p$gill.color))
'   b    e    g    h    k    n    o    p    r    u    w    y 
 1728    0  504  528   64  112    0  640   24   48  246   22'

barplot(table(mush_p$gill.color),
        names.arg = c('Buff', 'Red','Gray','Chocolate','Black','Brown',
                      'Orange','Pink','Green','Purple','White','Yellow'),
        col = c('lightgoldenrod', 'red','gray','chocolate','black','brown',
                'orange','pink','green','purple','white','yellow'),
        main = 'Poisonous Mushroom', las=2)

print(table(mush_e$gill.color))
'  b   e   g   h   k   n   o   p   r   u   w   y 
   0  96 248 204 344 936  64 852   0 444 956  64 '

barplot(table(mush_e$gill.color),
        names.arg = c('Buff', 'Red','Gray','Chocolate','Black','Brown',
                      'Orange','Pink','Green','Purple','White','Yellow'),
        col = c('lightgoldenrod', 'red','gray','chocolate','black','brown',
                'orange','pink','green','purple','white','yellow'),
        main = 'Edible Mushroom',las=2)

par(mfrow = c(1,1))

#stalk.shape
'enlarging=e,tapering=t'

str(mush$stalk.shape)
'Factor w/ 2 levels "e","t": 1 1 1 1 2 1 1 1 1 1 ...'

#Barplot
print(table(mush$stalk.shape))
'   e    t 
3516 4608 '

barplot(table(mush$stalk.shape),
        names.arg = c('Enlarging', 'Tapering'),
        col = c('cyan4', 'chocolate4'),
        main = 'Barplot of Mushroom Stalk Shape')

#Dividing plot areas `1 row 2 columns`
par(mfrow = c(1,2))

print(table(mush_p$stalk.shape))
'   e    t 
 1900 2016 '

barplot(table(mush_p$stalk.shape),
        names.arg = c('Enlarging', 'Tapering'),
        col = c('cyan4', 'chocolate4'),
        main = 'Poisonous Mushroom')

print(table(mush_e$stalk.shape))
'   e    t 
 1616 2592 '

barplot(table(mush_e$stalk.shape),
        names.arg = c('Enlarging', 'Tapering'),
        col = c('cyan4', 'chocolate4'),
        main = 'Edible Mushroom')

par(mfrow = c(1,1))

#stalk.root
'bulbous=b,club=c,cup=u,equal=e,rhizomorphs=z,rooted=r,missing=?'

str(mush$stalk.root)
'Factor w/ 5 levels "?","b","c","e",..: 4 3 3 4 4 3 3 3 4 3 ...'

#Barplot
print(table(mush$stalk.root))
'   ?    b    c    e    r 
 2480 3776  556 1120  192'

#Converting ? to NA
mush$stalk.root[mush$stalk.root == '?'] = ''

#Earlier NA is not identified being the symbol ? is used
sum(is.na(mush$stalk.root)) 
'2480'

#Filling the missing values using KNN
mush = kNN(mush)

#KNN Creates duplicate variables -removing them
mush = mush[1:23]

#Checking the missing values
sum(is.na(mush$stalk.root))
'0 missing values'

#Dropping levels/ factors not used (? is not used)
mush$stalk.root = factor(mush$stalk.root)

#Reduced from 5 levels to 4 being ? is not in use
str(mush$stalk.root)
'Factor w/ 4 levels "b","c","e","r": 3 2 2 3 3 2 2 2 3 2 ...'

print(table(mush$stalk.root))
'   b    c    e    r 
 5387  663 1882  192 '

barplot(table(mush$stalk.root),
        names.arg = c('Bulbous', 'Club', 'Equal', 'Rooted'),
        col = brewer.pal(4, 'RdYlGn'),
        main = 'Barplot of Mushroom Stalk root')

#Dividing plot areas `1 row 2 columns`
par(mfrow = c(1,2))

#Updating our subsets being we have replaced the missing values in stalk.root 
#variable and also adjusted the levels
#Dividing the data as per class
mush_p = mush[mush$class=='p',]
mush_e = mush[mush$class=='e',]

print(table(mush_p$stalk.root))
'   b    c    e    r 
 3234   45  637    0 '

barplot(table(mush_p$stalk.root),
        names.arg = c('Bulbous', 'Club', 'Equal', 'Rooted'),
        col = brewer.pal(4, 'RdYlGn'),
        main = 'Poisonous Mushroom')

print(table(mush_e$stalk.root))
'   b    c    e    r 
 2153  618 1245  192 '

barplot(table(mush_e$stalk.root),
        names.arg = c('Bulbous', 'Club', 'Equal', 'Rooted'),
        col = brewer.pal(4, 'RdYlBu'),
        main = 'Edible Mushroom')

par(mfrow = c(1,1))

#stalk.surface.above.ring
'fibrous=f,scaly=y,silky=k,smooth=s'

str(mush$stalk.surface.above.ring)
'Factor w/ 4 levels "f","k","s","y": 3 3 3 3 3 3 3 3 3 3 ...'

#Barplot
print(table(mush$stalk.surface.above.ring))
'   f    k    s    y 
  552 2372 5176   24'

barplot(table(mush$stalk.surface.above.ring),
        names.arg = c('Fibrous', 'Silky', 'Smooth', 'Scaly'),
        col = brewer.pal(4,'Paired'),
        main = 'Barplot of Mushroom Stalk surface above ring')

#Dividing plot areas `1 row 2 columns`
par(mfrow = c(1,2))

print(table(mush_p$stalk.surface.above.ring))
'   f    k    s    y 
 144 2228 1536    8 '

barplot(table(mush_p$stalk.surface.above.ring),
        names.arg = c('Fibrous', 'Silky', 'Smooth', 'Scaly'),
        col = brewer.pal(4,'Paired'),
        main = 'Poisonous Mushroom')

print(table(mush_e$stalk.surface.above.ring))
'   f    k    s    y 
 408  144 3640   16'

barplot(table(mush_e$stalk.surface.above.ring),
        names.arg = c('Fibrous', 'Silky', 'Smooth', 'Scaly'),
        col = brewer.pal(4,'Accent'),
        main = 'Edible Mushroom')

par(mfrow = c(1,1))
                  
#stalk.surface.below.ring
'fibrous=f,scaly=y,silky=k,smooth=s'

str(mush$stalk.surface.below.ring)
'Factor w/ 4 levels "f","k","s","y": 3 3 3 3 3 3 3 3 3 3 ...'

#Barplot
print(table(mush$stalk.surface.below.ring))
'   f    k    s    y 
 600 2304 4936  284 '

barplot(table(mush$stalk.surface.below.ring),
        names.arg = c('Fibrous', 'Silky', 'Smooth', 'Scaly'),
        col = brewer.pal(4,'Spectral'),
        main = 'Barplot of Mushroom Stalk surface below ring')

#Dividing plot areas `1 row 2 columns`
par(mfrow = c(1,2))

print(table(mush_p$stalk.surface.below.ring))
'   f    k    s    y 
  144 2160 1536   76 '

barplot(table(mush_p$stalk.surface.below.ring),
        names.arg = c('Fibrous', 'Silky', 'Smooth', 'Scaly'),
        col = brewer.pal(4,'Set1'),
        main = 'Poisonous Mushroom')

print(table(mush_e$stalk.surface.below.ring))
'   f    k    s    y 
  456  144 3400  208'

barplot(table(mush_e$stalk.surface.below.ring),
        names.arg = c('Fibrous', 'Silky', 'Smooth', 'Scaly'),
        col = brewer.pal(4,'Set1'),
        main = 'Edible Mushroom')

par(mfrow = c(1,1))    

#stalk.color.above.ring
'brown=n,buff=b,cinnamon=c,gray=g,orange=o,pink=p,red=e,white=w,yellow=y'

str(mush$stalk.color.above.ring)
'Factor w/ 9 levels "b","c","e","g",..: 8 8 8 8 8 8 8 8 8 8 ...'

#Barplot
print(table(mush$stalk.color.above.ring))
'   b    c    e    g    n    o    p    w    y 
  432   36   96  576  448  192 1872 4464    8 '

barplot(table(mush$stalk.color.above.ring),
        names.arg = c('buff','cinnamon','red','gray','brown','orange','pink','white','yellow'),
        col = c('lightgoldenrod','sienna','red','gray','brown','orange','pink','white','yellow'),
        main = 'Barplot of Mushroom Stalk color above ring')

#Dividing plot areas `1 row 2 columns`
par(mfrow = c(1,2))

print(table(mush_p$stalk.color.above.ring))
'   b    c    e    g    n    o    p    w    y 
  432   36    0    0  432    0 1296 1712    8 '

barplot(table(mush_p$stalk.color.above.ring),
        names.arg = c('buff','cinnamon','red','gray','brown','orange','pink','white','yellow'),
        col = c('lightgoldenrod','sienna','red','gray','brown','orange','pink','white','yellow'),
        main = 'Poisonous Mushroom',las =2)

print(table(mush_e$stalk.color.above.ring))
'   b    c    e    g    n    o    p    w    y 
    0    0   96  576   16  192  576 2752    0 '

barplot(table(mush_e$stalk.color.above.ring),
        names.arg = c('buff','cinnamon','red','gray','brown','orange','pink','white','yellow'),
        col = c('lightgoldenrod','sienna','red','gray','brown','orange','pink','white','yellow'),
        main = 'Edible Mushroom', las=2)

par(mfrow = c(1,1))    
          
#stalk.color.below.ring
'brown=n,buff=b,cinnamon=c,gray=g,orange=o,pink=p,red=e,white=w,yellow=y'

str(mush$stalk.color.below.ring)
'Factor w/ 9 levels "b","c","e","g",..: 8 8 8 8 8 8 8 8 8 8 ...'

#Barplot
print(table(mush$stalk.color.below.ring))
'   b    c    e    g    n    o    p    w    y 
  432   36   96  576  512  192 1872 4384   24'

barplot(table(mush$stalk.color.below.ring),
        names.arg = c('buff','cinnamon','red','gray','brown','orange','pink','white','yellow'),
        col = c('lightgoldenrod','sienna','red','gray','brown','orange','pink','white','yellow'),
        main = 'Barplot of Mushroom Stalk color below ring')

#Dividing plot areas `1 row 2 columns`
par(mfrow = c(1,2))

print(table(mush_p$stalk.color.below.ring))
'   b    c    e    g    n    o    p    w    y 
  432   36    0    0  448    0 1296 1680   24 '

barplot(table(mush_p$stalk.color.below.ring),
        names.arg = c('buff','cinnamon','red','gray','brown','orange','pink','white','yellow'),
        col = c('lightgoldenrod','sienna','red','gray','brown','orange','pink','white','yellow'),
        main = 'Poisonous Mushroom',las =2)

print(table(mush_e$stalk.color.below.ring))
'   b    c    e    g    n    o    p    w    y 
    0    0   96  576   64  192  576 2704    0'

barplot(table(mush_e$stalk.color.below.ring),
        names.arg = c('buff','cinnamon','red','gray','brown','orange','pink','white','yellow'),
        col = c('lightgoldenrod','sienna','red','gray','brown','orange','pink','white','yellow'),
        main = 'Edible Mushroom', las=2)

par(mfrow = c(1,1))   

#veil.type
'partial=p,universal=u'

str(mush$veil.type)
'Factor w/ 1 level "p": 1 1 1 1 1 1 1 1 1 1 ...'

print(table(mush$veil.type))
'   p 
 8124'

'Data is having only 1 type'

#veil.color
'brown=n,orange=o,white=w,yellow=y'

str(mush$veil.color)
'Factor w/ 4 levels "n","o","w","y": 3 3 3 3 3 3 3 3 3 3 ...'

#Barplot
print(table(mush$veil.color))
'   n    o    w    y 
   96   96 7924    8'

barplot(table(mush$veil.color),
        names.arg = c('brown','orange','white','yellow'),
        col = c('brown','orange','white','yellow'),
        ylim = c(0,8000),
        main = 'Barplot of Mushroom veil color')

#Dividing plot areas `1 row 2 columns`
par(mfrow = c(1,2))

print(table(mush_p$veil.color))
'   n    o    w    y 
    0    0 3908    8'

barplot(table(mush_p$veil.color),
        names.arg = c('brown','orange','white','yellow'),
        col = c('brown','orange','white','yellow'),
        ylim = c(0,4000),
        main = 'Poisonous Mushroom',las =2)

print(table(mush_e$veil.color))
'   n    o    w    y 
   96   96 4016    0'

barplot(table(mush_e$veil.color),
        names.arg = c('brown','orange','white','yellow'),
        col = c('brown','orange','white','yellow'),
        main = 'Edible Mushroom', las=2)

par(mfrow = c(1,1)) 

#ring.number
'none=n,one=o,two=t'

str(mush$ring.number)
'Factor w/ 3 levels "n","o","t": 2 2 2 2 2 2 2 2 2 2 ...'

#Barplot
print(table(mush$ring.number))
'   n    o    t 
   36 7488  600 '

barplot(table(mush$ring.number),
        names.arg = c('none','one','two'),
        col = brewer.pal(3,'YlGnBu'),
        main = 'Barplot of Mushroom veil number')

#Dividing plot areas `1 row 2 columns`
par(mfrow = c(1,2))

print(table(mush_p$ring.number))
'   n    o    t 
   36 3808   72'

barplot(table(mush_p$ring.number),
        names.arg = c('none','one','two'),
        col = brewer.pal(3,'YlGnBu'),
        main = 'Poisonous Mushroom')

print(table(mush_e$ring.number))
'   n    o    t 
    0 3680  528'

barplot(table(mush_e$ring.number),
        names.arg = c('none','one','two'),
        col = brewer.pal(3,'YlGnBu'),
        main = 'Edible Mushroom')

par(mfrow = c(1,1))         

#ring.type
'cobwebby=c,evanescent=e,flaring=f,large=l,none=n,pendant=p,sheathing=s,zone=z'

str(mush$ring.type)
'Factor w/ 5 levels "e","f","l","n",..: 5 5 5 5 1 5 5 5 5 5 ...'

#Barplot
print(table(mush$ring.type))
'   e    f    l    n    p 
 2776   48 1296   36 3968 '

barplot(table(mush$ring.type),
        names.arg = c('evanescent','flaring','large','none','pendant'),
        col = brewer.pal(5,'Dark2'),
        main = 'Barplot of Mushroom veil type')

#Dividing plot areas `1 row 2 columns`
par(mfrow = c(1,2))

print(table(mush_p$ring.type))
'   e    f    l    n    p 
 1768    0 1296   36  816 '

barplot(table(mush_p$ring.type),
        names.arg = c('evanescent','flaring','large','none','pendant'),
        col = brewer.pal(5,'Set1'),
        main = 'Poisonous Mushroom', las=2)

print(table(mush_e$ring.type))
'   e    f    l    n    p 
 1008   48    0    0 3152 '

barplot(table(mush_e$ring.type),
        names.arg = c('evanescent','flaring','large','none','pendant'),
        col = brewer.pal(5,'Set1'),
        main = 'Edible Mushroom', las=2)

par(mfrow = c(1,1))

#spore.print.color
'black=k,brown=n,buff=b,chocolate=h,green=r,orange=o,purple=u,white=w,yellow=y'

str(mush$spore.print.color)
'Factor w/ 9 levels "b","h","k","n",..: 3 4 4 3 4 3 3 4 3 3 ...'

#Barplot
print(table(mush$spore.print.color))
'   b    h    k    n    o    r    u    w    y 
   48 1632 1872 1968   48   72   48 2388   48 '

barplot(table(mush$spore.print.color),
        names.arg = c('buff','chocolate','black','brown','orange','green','purple','white','yellow'),
        col = c('lightgoldenrod','chocolate','black','brown','orange','green','purple','white','yellow'),
        main = 'Barplot of Mushroom Spore print color', las=2)

#Dividing plot areas `1 row 2 columns`
par(mfrow = c(1,2))

print(table(mush_p$spore.print.color))
'   b    h    k    n    o    r    u    w    y 
    0 1584  224  224    0   72    0 1812    0 '

barplot(table(mush_p$spore.print.color),
        names.arg = c('buff','chocolate','black','brown','orange','green','purple','white','yellow'),
        col = c('lightgoldenrod','chocolate','black','brown','orange','green','purple','white','yellow'),
        main = 'Poisonous Mushroom', las=2)

print(table(mush_e$spore.print.color))
'   b    h    k    n    o    r    u    w    y 
   48   48 1648 1744   48    0   48  576   48'

barplot(table(mush_e$spore.print.color),
        names.arg = c('buff','chocolate','black','brown','orange','green','purple','white','yellow'),
        col = c('lightgoldenrod','chocolate','black','brown','orange','green','purple','white','yellow'),
        main = 'Edible Mushroom', las=2)

par(mfrow = c(1,1))

#population
'abundant=a,clustered=c,numerous=n,scattered=s,several=v,solitary=y'

str(mush$population)
'Factor w/ 6 levels "a","c","n","s",..: 4 3 3 4 1 3 3 4 5 4 ...'

#Barplot
print(table(mush$population))
'   a    c    n    s    v    y 
  384  340  400 1248 4040 1712 '

barplot(table(mush$population),
        names.arg = c('abundant','clustered','numerous','scattered','several','solitary'),
        col = brewer.pal(6,'Set1'),
        main = 'Barplot of Mushroom Population', las=2)

#Dividing plot areas `1 row 2 columns`
par(mfrow = c(1,2))

print(table(mush_p$population))
'   a    c    n    s    v    y 
    0   52    0  368 2848  648 '

barplot(table(mush_p$population),
        names.arg = c('abundant','clustered','numerous','scattered','several','solitary'),
        col = brewer.pal(6,'Dark2'),
        main = 'Poisonous Mushroom', las=2)

print(table(mush_e$population))
'   a    c    n    s    v    y 
  384  288  400  880 1192 1064 '

barplot(table(mush_e$population),
        names.arg = c('abundant','clustered','numerous','scattered','several','solitary'),
        col = brewer.pal(6,'Dark2'),
        main = 'Edible Mushroom', las=2)

par(mfrow = c(1,1)) 

#habitat
'grasses=g,leaves=l,meadows=m,paths=p,urban=u,waste=w,woods=d'

str(mush$habitat)
'Factor w/ 7 levels "d","g","l","m",..: 6 2 4 6 2 2 4 4 2 4 ...'

#Barplot
print(table(mush$habitat))
'   d    g    l    m    p    u    w 
 3148 2148  832  292 1144  368  192'

barplot(table(mush$habitat),
        names.arg = c('woods','grasses','leaves','meadows','paths','urban','waste'),
        col = brewer.pal(7,'Paired'),
        main = 'Barplot of Mushroom habitat', las=2)

#Dividing plot areas `1 row 2 columns`
par(mfrow = c(1,2))

print(table(mush_p$habitat))
'   d    g    l    m    p    u    w 
 1268  740  592   36 1008  272    0 '

barplot(table(mush_p$habitat),
        names.arg = c('woods','grasses','leaves','meadows','paths','urban','waste'),
        col = brewer.pal(7,'Set1'),
        main = 'Poisonous Mushroom', las=2)

print(table(mush_e$habitat))
'   d    g    l    m    p    u    w 
 1880 1408  240  256  136   96  192'

barplot(table(mush_e$habitat),
        names.arg = c('woods','grasses','leaves','meadows','paths','urban','waste'),
        col = brewer.pal(7,'Set1'),
        main = 'Edible Mushroom', las=2)

par(mfrow = c(1,1))           

#Splitting the data into train & test
set.seed(121)
select_rows_80 = sample(1:nrow(mush), round(0.8*nrow(mush)),replace = FALSE)         
mush_train = mush[select_rows_80,]
mush_test = mush[-select_rows_80,]

#Building the Decision Tree
dt_model = tree(class~.-class, data = mush_train)

plot(dt_model, type = 'uniform')
text(dt_model,pretty = 0, cex=0.6)
summary(dt_model)

#Prediction on Train Data
pred_train = predict(dt_model, mush_train, type = 'class')
confusionMatrix(pred_train, mush_train$class)
"Confusion Matrix and Statistics

          Reference
Prediction    e    p
         e 3373    8
         p    0 3118
                                          
               Accuracy : 0.9988          
                 95% CI : (0.9976, 0.9995)
    No Information Rate : 0.519           
    P-Value [Acc > NIR] : < 2e-16         
                                          
                  Kappa : 0.9975          
 Mcnemar's Test P-Value : 0.01333         

Sensitivity : 1.0000          
Specificity : 0.9974          
Pos Pred Value : 0.9976          
Neg Pred Value : 1.0000          
Prevalence : 0.5190          
Detection Rate : 0.5190          
Detection Prevalence : 0.5202          
Balanced Accuracy : 0.9987          

'Positive' Class : e "


#Prediction on Test Data
pred_test = predict(dt_model,mush_test, type = 'class')
confusionMatrix(pred_test,mush_test$class)
"Confusion Matrix and Statistics

          Reference
Prediction   e   p
         e 835   0
         p   0 790
                                     
               Accuracy : 1          
                 95% CI : (0.9977, 1)
    No Information Rate : 0.5138     
    P-Value [Acc > NIR] : < 2.2e-16  
                                     
                  Kappa : 1          
 Mcnemar's Test P-Value : NA         
                                     
            Sensitivity : 1.0000     
            Specificity : 1.0000     
         Pos Pred Value : 1.0000     
         Neg Pred Value : 1.0000     
             Prevalence : 0.5138     
         Detection Rate : 0.5138     
   Detection Prevalence : 0.5138     
      Balanced Accuracy : 1.0000     
                                     
       'Positive' Class : e"

#Building the tree using rpart library
fit = rpart(class~.-class, data = mush_train, method = 'class')
summary(fit)

#Plotting the tree
rpart.plot(fit)

fit_predict1 = predict(fit, newdata = mush_train, type = 'class')

confusionMatrix(fit_predict, mush_test$class)

#Predicting on test data
fit_predict = predict(fit, newdata = mush_test, type = 'class')
confusionMatrix(fit_predict, mush_test$class)
"
Confusion Matrix and Statistics

          Reference
Prediction   e   p
         e 835  11
         p   0 779
                                          
               Accuracy : 0.9932          
                 95% CI : (0.9879, 0.9966)
    No Information Rate : 0.5138          
    P-Value [Acc > NIR] : < 2.2e-16       
                                          
                  Kappa : 0.9864          
 Mcnemar's Test P-Value : 0.002569        
                                          
            Sensitivity : 1.0000          
            Specificity : 0.9861          
         Pos Pred Value : 0.9870          
         Neg Pred Value : 1.0000          
             Prevalence : 0.5138          
         Detection Rate : 0.5138          
   Detection Prevalence : 0.5206          
      Balanced Accuracy : 0.9930          
                                          
       'Positive' Class : e"

#ROC Curve
fit_pred1 = prediction(predict(fit, newdata = mush_test, type = 'prob')[,2], mush_test$class)
pred_roc = performance(fit_pred1, measure = 'tpr', x.measure = 'fpr')

plot(performance(fit_pred1, "tpr", "fpr"), col="blue", main="ROC Kyphosis, using library ROCR")
abline(0, 1, lty=2)

auc <- performance(fit_pred1, "auc")
auc@y.values
'0.993038'

#Building Tree model using party library
model_par = ctree(class~.-class, data = mush_train)
summary(model_par)

plot(model_par)

#Prediction on Test data
pred_par = predict(model_par,mush_test)
confusionMatrix(pred_par, mush_test$class)
"
Confusion Matrix and Statistics

          Reference
Prediction   e   p
         e 835   0
         p   0 790
                                     
               Accuracy : 1          
                 95% CI : (0.9977, 1)
    No Information Rate : 0.5138     
    P-Value [Acc > NIR] : < 2.2e-16  
                                     
                  Kappa : 1          
 Mcnemar's Test P-Value : NA         
                                     
            Sensitivity : 1.0000     
            Specificity : 1.0000     
         Pos Pred Value : 1.0000     
         Neg Pred Value : 1.0000     
             Prevalence : 0.5138     
         Detection Rate : 0.5138     
   Detection Prevalence : 0.5138     
      Balanced Accuracy : 1.0000     
                                     
       'Positive' Class : e "

#ROC Curve
pred_par1 = prediction(predict(model_par, mush_test, type = 'prob')[,2], mush_test$class)
pred_roc = performance(fit_pred1, measure = 'tpr', x.measure = 'fpr')

plot(performance(fit_pred1, "tpr", "fpr"), col="blue", main="ROC Kyphosis, using library ROCR")
abline(0, 1, lty=2)

auc <- performance(fit_pred1, "auc")
auc@y.values

#Tree vs Rpart
"The key difference between these two packages is the way they treat missing 
values in the splitting and scoring processes. With R.Tree, an observation with 
a missing value for the primary split rule is not sent further down the tree. 
On the other hand, with R.Rpart, users may choose the way to handle missing 
values, including using surrogates, by setting up the "usesurrogate" parameter 
in the rpart.control option

Rpart offers more flexibility when growing trees. 9 parameters are offered for 
setting up the tree modeling process, including the usage of surrogates. 
R.Tree only offers 3 parameters to control the modeling process 
(mincut, minsize and mindev).

http://www.rohitschauhan.com/index.php/2018/06/13/a-comparison-on-using-r-tree-vs-r-rpart/"

#Rpart vs Ctree
"both rpart and ctree recursively perform univariate splits of the dependent 
variable based on values on a set of covariates. rpart and related algorithms 
usually employ information measures (such as the Gini coefficient) for 
selecting the current covariate.

ctree, according to its authors (see chl's comments) avoids the following 
variable selection bias of rpart (and related methods): They tend to select 
variables that have many possible splits or many missing values. Unlike the 
others, ctree uses a significance test procedure in order to select variables 
instead of selecting the variable that maximizes an information measure 
(e.g. Gini coefficient)."
