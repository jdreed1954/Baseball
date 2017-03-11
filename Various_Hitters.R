## Roger Maris
#Let's lookup Maris' playerID from the master table and generate a table with his lifetime statistics.
```{r}
maris.info <- subset(Master, nameFirst == "Roger" & nameLast =="Maris")
maris.id   <- as.character(maris.info$playerID)

Maris <- get_stats(maris.id)
Maris

```

```{r}
ggplot(data = Maris, aes(Age,H/AB*1000)) + geom_point() + geom_smooth() + geom_hline(yintercept=300) +
  ggtitle("Roger Maris' Batting Average") + labs(x = "Year", y = "Batting Average")
```
```{r}
ggplot(data = Maris, aes(Age,OBP)) + 
  geom_point(aes(x = Age, y = OBP, colour ="OBP")) + geom_smooth(aes(y = OBP, colour = "OBP")) +
  geom_point(aes(x = Age, y = SLG, colour = "SLG")) + geom_smooth(aes(y = SLG, colour = "SLG")) +
  geom_point(aes(x = Age, y = OPS, colour = "OPS")) + geom_smooth(aes(y = OPS, colour = "OPS")) +
  ggtitle("Roger Maris' OBP, SLG, SLG+PBP") + labs(x = "Age", y = "OBP, SLG & OPS")
```

Compare Maris' and Mantle's OPS

```{r}
ggplot(data = Maris, aes(Age,OPS)) + 
  geom_point(aes(x = Age, y = OPS, colour ="Maris OPS")) + geom_smooth(data = Maris, aes(y = OPS, colour = "Maris OPS")) +
  geom_point(data = Mantle, aes(Age, OPS, colour = "Mantle OPS")) + geom_smooth(data = Mantle, aes(y = OPS, colour = "Mantle OPS")) +
  ggtitle("Maris versus Mantle OPS") + labs(x = "Age", y = "OPS")
```



## Rogers Hornsby
Let's lookup Hornsby's' playerID from the master table and generate a table with his lifetime statistics.
```{r}
hornsby.info <- subset(Master, nameFirst == "Rogers" & nameLast =="Hornsby")
hornsby.id   <- as.character(hornsby.info$playerID)

Hornsby <- get_stats(hornsby.id)
Hornsby

```



```{r}
ggplot(data = Hornsby, aes(Age,H/AB*1000)) + geom_point() + geom_smooth() + geom_hline(yintercept=300) +
ggtitle("Rogers Hornsby's Batting Average") + labs(x = "Year", y = "Batting Average")
```

```{r}
ggplot(data = Hornsby, aes(Age,OBP)) + 
geom_point(aes(x = Age, y = OBP, colour ="OBP")) + geom_smooth(aes(y = OBP, colour = "OBP")) +
geom_point(aes(x = Age, y = SLG, colour = "SLG")) + geom_smooth(aes(y = SLG, colour = "SLG")) +
geom_point(aes(x = Age, y = OPS, colour = "OPS")) + geom_smooth(aes(y = OPS, colour = "OPS")) +
ggtitle("Rogers Hornsby's OBP, SLG, OPS") + labs(x = "Age", y = "OBP, SLG & OPS")
```


## Ted Williams
Let's lookup Ted Williams' playerID from the master table and generate a table with his lifetime statistics.


```{r}
williams.info <- subset(Master, nameFirst == "Ted" & nameLast =="Williams")
Williams.id   <- as.character(williams.info$playerID)

Williams <- get_stats(williams.id)
Williams
```

```{r}
ggplot(data = Williams, aes(Age,OBP)) + 
geom_point(aes(x = Age, y = OBP, colour ="OBP")) + geom_smooth(aes(y = OBP, colour = "OBP")) +
geom_point(aes(x = Age, y = SLG, colour = "SLG")) + geom_smooth(aes(y = SLG, colour = "SLG")) +
geom_point(aes(x = Age, y = OPS, colour = "OPS")) + geom_smooth(aes(y = OPS, colour = "OPS")) +
ggtitle("Ted Williams' OBP, SLG, OPS") + labs(x = "Age", y = "OBP, SLG & OPS")
```


#Compare Williams, Mantle, Maris and Hornsby OPS

```{r}
ggplot(data = Maris, aes(Age,OPS)) + 
geom_point(aes(x = Age, y = OPS, colour ="Williams OPS")) + geom_smooth(data = Williams, aes(y = OPS, colour = "Williams OPS")) +
geom_point(aes(x = Age, y = OPS, colour ="Maris OPS")) + geom_smooth(data = Maris, aes(y = OPS, colour = "Maris OPS")) +
geom_point(data = Mantle, aes(Age, OPS, colour = "Mantle OPS")) + geom_smooth(data = Mantle, aes(y = OPS, colour = "Mantle OPS")) +
geom_point(data = Hornsby, aes(Age, OPS, colour = "Hornsby OPS")) + 
geom_smooth(data = Hornsby, aes(y = OPS, colour = "Hornsby OPS")) +
ggtitle("Hornsby, Mantle, Maris, & Williams OPS") + labs(x = "Age", y = "OPS")
```



## Ty Cobb
Let's lookup Ty Cobb's playerID from the master table and generate a table with his lifetime statistics.


```{r}
cobb.info <- subset(Master, nameFirst == "Ty" & nameLast =="Cobb")
cobb.id   <- as.character(cobb.info$playerID)

Cobb <- get_stats(cobb.id)
Cobb
```

```{r}
ggplot(data = Cobb, aes(Age,OBP)) + 
geom_point(aes(x = Age, y = OBP, colour ="OBP")) + geom_smooth(aes(y = OBP, colour = "OBP")) +
geom_point(aes(x = Age, y = SLG, colour = "SLG")) + geom_smooth(aes(y = SLG, colour = "SLG")) +
geom_point(aes(x = Age, y = OPS, colour = "OPS")) + geom_smooth(aes(y = OPS, colour = "OPS")) +
ggtitle("Ty Cobb OBP, SLG & OPS") + labs(x = "Age", y = "OBP, SLG & OPS")
```