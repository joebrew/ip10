library(tidyverse)
library(readxl)

# Read data
df <- readxl::read_excel('data/base triagem excel.xlsx')

# Number of patients
nrow(df)

# Mean age ?

# Sex
table(df$sexo)
prop.table(table(df$sexo))

# Baceteriologicall confirmed
table(df$tbconfirmed)

# CD4 counts
table(df$CD4 < 100)
prop.table(table(df$CD4 < 100))

# Died
table(df$toutcomes == 'died')
prop.table(table(df$toutcomes == 'died'))
table(is.na(df$outcomes))

# Availability of blood samples
table(is.na(df$day7))
table(is.na(df$day60))

# IP10 kinetics
df$ip10high
df$tbconfirmed
chisq.test(x = df$ip10high,
           y = df$tbconfirmed)

# Decline
df$decline <- df$day0 > (df$day7 + 300)

chisq.test(x = df$decline,
           y = df$tbconfirmed)

# Among those with no drug resistance
no_res <- df[df$res == 'N']

# Proportion of good response
x <- table(x = ifelse(df$decline, 'good response', 'bad response'),
           y = ifelse(df$tbconfirmed == 'YES', 'TB confirmed', 'Not confirmed'))
prop.table(x, 2)
prop.test(x)

