library(ggplot2)
df <- mpg
df$manufacturer <- as.factor(df$manufacturer)
df$model <- as.factor(df$model)
df$trans <- as.factor(df$trans)
df$drv <- as.factor(df$drv)
str(df)

## basic scatter plot
qplot(displ, hwy, data = df)

## with color
qplot(displ, hwy, data = df, color = drv)

## with a statistic geom
qplot(displ, hwy, data = df, geom = c("point", "smooth"))

## a histogram
qplot(hwy, data = df, fill = drv)

## using facets
qplot(displ, hwy, data = df, facets = .~drv)

qplot(hwy, data = df, 
      facets = drv~., binwidth = 2)

## qplot example
df1 <- mpg[which(mpg$drv!="r"),]
df1 <- select(df1, c("displ", "hwy", "drv"))
df1$drv <- as.factor(df1$drv)
levels(df1$drv) <- c("four-wheel drive", "two-wheel drive")
str(df1)

qplot(displ, hwy, data = df1, facets = .~drv, 
      geom = c("point", "smooth"), method = "lm")

g <- ggplot(df1, aes(displ, hwy))
summary(g)

g <- ggplot(df1, aes(displ, hwy))
p <- g + geom_point()
p

g + geom_point() + geom_smooth()

g + geom_point() + geom_smooth(method = "lm")

g + geom_point() + geom_smooth(method = "lm") + facet_grid(.~drv)

