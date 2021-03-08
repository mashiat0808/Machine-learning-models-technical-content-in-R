#Data Visualization Assignment 


dataset <- read.csv(file.choose())

#Task_1: Visualize the performance of the students based on their absence days. Based on your visualization make 
#a decision that is being present in the class is an important factor for being a good student?
#{Hint: Here you have one column named "class" that column shows the performance of the students
#M- Medium, L-Lower, H-Higher}
library (dplyr)
library(ggplot2)
absencedays <- dataset  %>%
  group_by(StudentAbsenceDays, Class)%>% 
  summarise(Number=n())%>% 
  arrange(desc=Number) %>%  select(StudentAbsenceDays, Number, Class)
 
ggplot(data=absencedays, aes(x=factor(Class), y=Number, fill=StudentAbsenceDays)) +
  geom_bar(stat="identity", position=position_dodge()) +  
  labs(title= "Is being present in the class is an important factor for being a good student?",
       y="Number of Students",
       x="Grade of Students") + theme_minimal()+ scale_fill_brewer(palette="Blues")

##It appears that students with high and medium grade have less than 7 days of absense in most the cases
##and low grade students are more absent in class than high and medium grade students

#Task_2: We all know a sentence that "School starts at home". So relation with our parents affects our study as well. 
#So, here in the dataset there is a column named "relation" where it is stated you have relation with whom. Now your work is to 
#analyze and visualize that which students has higher performance index, the one who has good relation with mom or the one has good relation with dad?
#{Hint: Here you have one column named "class" that column shows the performance of the students
#M- Medium, L-Lower, H-Higher}
parentrelation <- dataset  %>%
  group_by(Relation, Class)%>% 
  summarise(Number=n())%>% 
  arrange(desc=Number) %>%  select(Relation, Number, Class)

ggplot(data=parentrelation, aes(x=factor(Class), y=Number, fill=Relation)) +
  geom_bar(stat="identity", position=position_dodge()) +  
  labs(title= "Which students has higher performance index, comparing their relationship with parents?",
       y="Number of Students",
       x="Grade of Students") + theme_minimal()+ scale_fill_brewer(palette="Blues")

##It appears that either parent can play an important role to the performance of the students grades


#Task_3: "The more the resources, the better the student is performing" - you have to prove if this statement is correct 
#or not by analyzing the dataset. Analyze and visualize that relationship between the student's class and Visited Resources.
resource <- dataset   %>%
  group_by(VisITedResources, Class)%>% 
  summarise(Number=n())%>% 
  arrange(desc=Number) %>%  select(VisITedResources, Number, Class)

ggplot(resource, aes(y=Class, x=VisITedResources, shape=Class, color=Class)) +
  geom_point() +
  labs(title= "Which students has higher performance index, based on their resources?",
                    y="Number of Students",
                    x="Resources of Students") + theme_minimal()

##Resources are important in students performance index


#Task_4: Based on gender find out where are the most of the students from. Visualize the result. 
gendernation<- dataset  %>%
  group_by(gender, NationalITy)%>% 
  summarise(Number=n())%>% 
  arrange(desc=Number) %>%  select(gender, Number, NationalITy)

ggplot(data=gendernation, aes(x=factor(NationalITy), y=Number, fill=gender)) +
  geom_bar(stat="identity") +  
  labs(title= "Number of students from countries, based on their gender",
       y="Number of Students",
       x="Country") + theme_minimal()

##Most students are from Kuwait and then Jordan


#Task_5: Find out relationship between Gender and the topic they are studying. Find out who is dominating. 
gendertopic<- dataset  %>%
  group_by(gender, Topic)%>% 
  summarise(Number=n())%>% 
  arrange(desc=Number) %>%  select(gender, Number, Topic)

ggplot(data=gendertopic, aes(x=factor(Topic), y=Number, fill=gender)) +
  geom_bar(stat="identity") +  
  labs(title= "Students' Topic that they study, based on their gender",
       y="Number of Students",
       x="Topic") + theme_minimal()

##The most studied topic by students are IT & French
