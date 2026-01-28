library(tidyverse)

survey_data <- data.frame(
  id = 1:50,
  experience = sample(1:20, 50, replace = TRUE),   
  work_hours = sample(30:60, 50, replace = TRUE),   
  satisfaction = sample(1:10, 50, replace = TRUE),
  status = sample(c("Junior", "Senior"), 50, replace = TRUE)
)

print(view(survey_data))
#---------------------------------------------------
summary(survey_data)
status_summary <- survey_data%>%
  group_by(status)%>%
  summarise(
    avg_satisfaction=mean(satisfaction),
    avg_hours=mean(work_hours),
    count=n()
  )
print(status_summary)

#---------------------------------------------------
ggplot(survey_data, aes(x=work_hours, y=satisfaction, color=status))+
  geom_point(size=3, alpha=0.7)+
  geom_smooth(method="lm", se=FALSE)+
  labs(title="Work Hours vs Job Satisfaction",
       x="Weekly Work Hours",
       y="Satisfaction Score")+
  theme_minimal()

#---------------------------------------------------

model <- lm(satisfaction ~ experience + work_hours, data=survey_data)
summary(model)

