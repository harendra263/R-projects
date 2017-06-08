#==================================================================================
# Data wrangling and Feature Engineering
#==================================================================================

#==================================================================================
train <- read.csv(file.choose(), header = TRUE)

View(train)

zero.fare <- train %>%
  filter(Fare==0.0)
View(zero.fare)

#==================================================================================
#lets get the total by Pclass
#==================================================================================
zero.fare.pclass <- zero.fare %>%
  group_by(Pclass) %>%
  summarise(Total = n()) %>%
  arrange(desc(Total))

View(zero.fare.pclass)
#==================================================================================
# Add new feature for the title of each passenger
#==================================================================================

train <- train %>%
  mutate(Title = str_extract(Name,"[a-zA-Z]+\\."))

table(train$Title)

#==================================================================================
# Condense title down to small subsets
#==================================================================================

Title.lookup <- data.frame(Title = c("Mr." , "Capt." ,"Col.", "Don.","Dr.",
                                     "Jonkheer.", "Major.","Rev.","Sir.",
                                     "Mrs.","Dana.", "Lady.", "Mme.",
                                     "Countess.", 
                                     "Miss.","Mlle.","Ms."
                                     ,"Master."),
                           New.Title = c(rep("Mr.", 9),
                                         rep("Mrs.",5),
                                         rep("Miss.",3),
                                         "Master."),
                           stringsAsFactors = FALSE)
View(Title.lookup)

#====================================================================================
# Replace titles using lookup table #
#====================================================================================

train <- train %>%
  left_join(Title.lookup, by="Title")

View(train)


train <- train %>%
  mutate(Title = New.Title) %>%
           select(-New.Title)
View(train)
         
#======================================================================================
#Double check your work
#======================================================================================

table(train$Title)

train %>%
  filter((Sex == "female" & (Title == "Mr." | Title =="Master.")) |
         (Sex == "male"   & (Title == "Mrs." | Title == "Miss.")))

train$Title[train$PassengerId==797] <- "Mrs."

#======================================================================================
# Create Summary stats for fares for those Passengers with a Title 
# of "Mr." by Pclass
#======================================================================================



mr.fare.stats <- train %>%
  filter(Title == "Mr.") %>%
  group_by(Pclass) %>%
  summarize(Fare.min = min(Fare),
            Fare.max = max(Fare),
            Fare.mean = mean(Fare),
            Fare.median = median(Fare),
            Fare.var = var(Fare),
            Fare.SD = sd(Fare),
            Fare.IQR = IQR(Fare))

View(mr.fare.stats)
summary(train)

summary(train$Fare[train$Title == "Mr." & train$Pclass == 1])


#==================================================================================
# Create "tracking Feature" for those records that were originally
# had 0.00 for the fare variabe
#==================================================================================


train$fare.Zero <-  ifelse(train$Fare == 0.0 ,"Y" ,"N")
View(train)


#==================================================================================
#Create a lookup table for Zero fare values
#==================================================================================

Zero.fare.lookup <- train %>%
  filter(Title == "Mr.") %>%
  group_by(Pclass , Title) %>%
  summarize(New.Fare = median(Fare))

View(Zero.fare.lookup)
  

#===================================================================================
#Impute the zero fare using lookup table
#===================================================================================

train <- train %>%
  left_join(Zero.fare.lookup , by = c("Pclass", "Title")) %>%
  mutate(Fare = ifelse(Fare ==0.0 , New.Fare, Fare)) %>%
  select(-New.Fare)

View(train)


#===================================================================================
#Take a closer look at the age variable all up
#===================================================================================

age.stats <-  train %>%
  group_by(Pclass, Title) %>%
  summarize(Age.Min = min(Age , na.rm= TRUE),
            Age.Max = max(Age ,na.rm =TRUE),
            Age.mean = mean(Age , na.rm =TRUE),
            Age.median = median(Age , na.rm =TRUE),
            Age.NA.Count= sum(is.na(Age)),
            Age.Var = var(Age ,na.rm =TRUE),
            Age.Sd = sd(Age , na.rm=TRUE),
            Age.IQR = IQR(Age , na.rm =TRUE)) %>%
  arrange(Title ,Pclass)

View(age.stats)
  
  
#==================================================================================
# Create "tracking Feature" for those records that were originally
# had missing ages
#==================================================================================

train$Age.Missing <- ifelse(is.na(train$Age),"Y", "N")

View(train)

#===================================================================================
# Create a lookup table
#===================================================================================

age.lookup <- age.stats %>%
  select(Pclass , Title ,Age.mean ,Age.median)

View(age.lookup)

#=====================================================================================
# Impute missing ages using lookup
#=====================================================================================

train <- train %>%
  left_join(age.lookup , by =c("Pclass" , "Title")) %>%
  mutate(Age = ifelse(Age.Missing == "Y",
                      ifelse(Title == "Miss.", Age.median , Age.mean),
                      Age)) %>%
  select(-Age.median ,-Age.mean)

View(train)


#===================================================================================
#Look at imputed age Distribution all up 
#===================================================================================

quantile(train$Age , probs = seq(0.05,1,0.05))


#===================================================================================
#Create Ticket-based features
#===================================================================================

ticket.lookup <- train %>%
  group_by(Ticket) %>%
  summarize(Group.Count =n(),
            Avg.Fare = max(Fare)/n(),
            Female.Count = sum(Sex == "female"),
            Male.Count =sum(Sex == "male"),
            Child.Count = sum(Age <18),
            Elderly.Count = sum(Age > 54.0),
            Female.Ratio = sum(Sex == "female") / n(),
            Male.Ratio = sum(Sex == "male") / n(),
            Child.Ratio = sum(Age <18) / n(),
            Elderly.Ratio = sum(Age > 54.0) / n(),
            Female.Child.Ratio = (sum(Age <18) + sum(Sex == "female" & Age >=18)) / n(),
            Min.Age = min(Age),
            Max.Age = max(Age))

View(ticket.lookup)

#===================================================================================
# Double-check our work
#===================================================================================

ticket.lookup %>% filter(Ticket =="3101295")

View(train %>% filter(Ticket == "3101295"))

#===================================================================================
#Populate Training data via lookup table
#===================================================================================

train <- train %>%
  left_join(ticket.lookup , by = "Ticket")


View(train %>%  filter(Ticket == "3101295"))

#===================================================================================
# # The payoff - Investigate the hypothesis that travelling with children
# might be predictive
#===================================================================================

library(ggplot2)

# Set up factors
 train$Survived <-  as.factor(train$Survived)
 train$Pclass <- as.factor(train$Pclass)
 
 #subset
 tickets.children <- train %>%
   filter(Child.Count > 0)
 #Visualize
 ggplot(tickets.children ,aes(x= Pclass ,fill =Survived)) + 
   theme_bw() +
   geom_bar() +
   facet_wrap(~ Title) +
   labs( y = "Count of Passenger" ,
         title = "Survival Rates for Tickets Groups Travelling with Children")
 
 #Subset
 tickets.no.children <-  train %>%
   filter( Child.Count ==0)
 
 #Visulize
 ggplot(tickets.no.children , aes(x= Pclass, fill = Survived)) +
   theme_bw() +
   geom_bar() +
   facet_wrap(~Title) +
   labs( y ="Count of Passenger" ,
         title = "Survival rates of tickets group travelling without children")

























