library(plotly)

# P(Sex|Class)
dist <- mydata %>% 
  count(Class, Sex, .drop = FALSE) %>% 
  group_by(Class) %>% 
  mutate(freq_per_class = n/sum(n))
# volcano is a numeric matrix that ships with R
fig <- plot_ly(data = dist, x = ~Class, y = ~Sex, z = ~n) %>% add_surface()
fig
plot_ly(x = )


data(Titanic)
titanic <- as.data.frame(Titanic)
titanic$Survived <- factor(titanic$Survived, levels=c("Yes", "No"))


ggplot(data=titanic) +
  geom_mosaic(aes(weight=Freq, x=product(Class), fill=Survived))
# good practice: use the 'dependent' variable (or most important variable)
# as fill variable

ggplot(data=titanic) +
  geom_mosaic(aes(weight=Freq, x=product(Class, Age), fill=Survived))

ggplot(data=titanic) +
  geom_mosaic(aes(weight=Freq, x=product(Class), conds=product(Age), fill=Survived))
ggplot(data=titanic) +
  geom_mosaic(aes(weight=Freq, x=product(Survived, Class), fill=Age))

# Just excluded for timing. Examples are included in testing to make sure they work
## Not run: 
data(happy, package="productplots")

ggplot(data = happy) + geom_mosaic(aes(x=product(happy)), divider="hbar")
ggplot(data = happy) + geom_mosaic(aes(x=product(happy))) +
  coord_flip()
# weighting is important
ggplot(data = happy) +
  geom_mosaic(aes(weight=wtssall, x=product(happy)))
ggplot(data = happy) + geom_mosaic(aes(weight=wtssall, x=product(health), fill=happy)) +
  theme(axis.text.x=element_text(angle=35))
ggplot(data = happy) +
  geom_mosaic(aes(weight=wtssall, x=product(health), fill=happy), na.rm=TRUE)
ggplot(data = happy) +
  geom_mosaic(aes(weight=wtssall, x=product(health, sex, degree), fill=happy),
              na.rm=TRUE)

# here is where a bit more control over the spacing of the bars is helpful:
# set labels manually:
ggplot(data = happy) +
  geom_mosaic(aes(weight=wtssall, x=product(age), fill=happy), na.rm=TRUE, offset=0) +
  scale_x_productlist("Age", labels=c(17+1:72))
# thin out labels manually:
labels <- c(17+1:72)
labels[labels %% 5 != 0] <- ""
ggplot(data = happy) +
  geom_mosaic(aes(weight=wtssall, x=product(age), fill=happy), na.rm=TRUE, offset=0) +
  scale_x_productlist("Age", labels=labels)
ggplot(data = happy) +
  geom_mosaic(aes(weight=wtssall, x=product(age), fill=happy, conds = product(sex)),
              divider=mosaic("v"), na.rm=TRUE, offset=0.001) +
  scale_x_productlist("Age", labels=labels)
# facetting works!!!!
ggplot(data = happy) +
  geom_mosaic(aes(weight=wtssall, x=product(age), fill=happy), na.rm=TRUE, offset = 0) +
  facet_grid(sex~.) +
  scale_x_productlist("Age", labels=labels)

ggplot(data = happy) +
  geom_mosaic(aes(weight = wtssall, x = product(happy, finrela, health)),
              divider=mosaic("h"))
ggplot(data = happy) +
  geom_mosaic(aes(weight = wtssall, x = product(happy, finrela, health)), offset=.005)

# Spine example
ggplot(data = happy) +
  geom_mosaic(aes(weight = wtssall, x = product(health), fill = health)) +
  facet_grid(happy~.)

library(stargazer)

stargazer(attitude)
linear.1 <- lm(rating ~ complaints + privileges + learning + raises + critical,data=attitude)
linear.2 <- lm(rating ~ complaints + privileges + learning, data=attitude)## create an indicator dependent variable, and run a probit model
attitude$high.rating <- (attitude$rating > 70)
probit.model <- glm(high.rating ~ learning + critical + advance, data=attitude,family = binomial(link = "probit"))
stargazer(linear.1, linear.2, probit.model, title="Results", align=TRUE) %>% pander

model_metrics %>% mutate_at("combination", ~as.factor(.) %>% fct_relevel(coef_tbl %>% select(., -Regression) %>% map_int(~sum(!is.na(.))) %>% sort() %>% names())) %>% 
  arrange(combination) %>% 
  pull(fit) %>% 
  exec(stargazer, ., title = "Results", align = TRUE, type = "text", table.placement="H",
       omit.stat=c("f", "ser"), order=c("Constant",
                                        "Class_X2nd",
                                        "Class_X3rd",
                                        "Class_Crew",
                                        "Sex_Male",
                                        "Age_Child",
                                        "Sex_Male_x_Age_Child"),
       add.lines= model_metrics %>% select( c(kap, accuracy)) %>% imap(~c(paste0("cv_",.y), round(.x, digits = 3))))


model_metrics %>% select(c(kap, accuracy)) %>% imap(~c(paste0("cv_",.y), round(.x, digits = 3)))



model_metrics %>% pull(kap) %>% enframe




linear.1 <- lm(rating ~ complaints + privileges + learning + raises + critical, data=attitude)
linear.2 <- lm(rating ~ complaints + privileges + learning, data=attitude)

stargazer(linear.1, linear.2, type="text", keep.stat=c("n"),  
          add.lines=list(h = c("Note 1", "Xxxxxxxxxxxxxxx", ""),b = c("Note 2", "", "X")))
