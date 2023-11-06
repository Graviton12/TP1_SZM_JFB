# loading dataset
library(tidyverse)
dataset=read.csv('ParisHousing.csv')

#Pre-processing Data
# Viewing dataset
head(dataset)

# information
glimpse(dataset)

# column names
names(dataset)

# number of variables
length(dataset) # there are 17 variables

# checking for missing values
colSums(is.na(dataset)) # there is no missing values

# Exploratory Data Analysis
# statistic of dataset
summary(dataset)

# statistics for square meters
paste('Mean', mean(dataset$squareMeters))
paste('Median', median(dataset$squareMeters))
paste('Variance', var(dataset$squareMeters))
paste('Standard Deviation', sd(dataset$squareMeters))

# statistics for number of rooms
paste('Mean', mean(dataset$numberOfRooms))
paste('Median', median(dataset$numberOfRooms))
paste('Variance', var(dataset$numberOfRooms))
paste('Standard Deviation', sd(dataset$numberOfRooms))

# statistics for number of floors
paste('Mean', mean(dataset$floors))
paste('Median', median(dataset$floors))
paste('Variance', var(dataset$floors))
paste('Standard Deviation', sd(dataset$floors))

# statistics for number of previous owners
paste('Mean', mean(dataset$numPrevOwners))
paste('Median', median(dataset$numPrevOwners))
paste('Variance', var(dataset$numPrevOwners))
paste('Standard Deviation', sd(dataset$numPrevOwners))

# statistics for number of basement
paste('Mean', mean(dataset$basement))
paste('Median', median(dataset$basement))
paste('Variance', var(dataset$basement))
paste('Standard Deviation', sd(dataset$basement))

# statistics for number of attic
paste('Mean', mean(dataset$attic))
paste('Median', median(dataset$attic))
paste('Variance', var(dataset$attic))
paste('Standard Deviation', sd(dataset$attic))

# statistics for number of garage
paste('Mean', mean(dataset$garage))
paste('Median', median(dataset$garage))
paste('Variance', var(dataset$garage))
paste('Standard Deviation', sd(dataset$garage))

# looking at distribution of data
# histogram of square meter
ggplot(data=dataset, aes(x=squareMeters))+geom_histogram(fill = 'orange', color='brown')+
  xlab('Square Meters')

# histogram of number of rooms
ggplot(data=dataset, aes(x=numberOfRooms))+geom_histogram(fill = 'pink', color='brown')+
  xlab('Number of Rooms')

# histogram of number of previous owners
ggplot(data=dataset, aes(x=numPrevOwners))+geom_histogram(binwidth = 1, fill = 'red', color='brown')+
  xlab('Number of Previous Owners')

# histogram of number of basements
ggplot(data=dataset, aes(x=basement))+geom_histogram(fill = 'lightblue', color='blue')+
  xlab('Number of Basements')

# histogram of number of attics
ggplot(data=dataset, aes(x=attic))+geom_histogram(fill = 'green', color='darkgreen')+
  xlab('Number of Attics')

# histogram of number of garages
ggplot(data=dataset, aes(x=garage))+geom_histogram(fill = 'purple', color='darkred')+
  xlab('Number of Garages')

# histogram of when house was made
ggplot(data=dataset, aes(x=made))+geom_histogram(fill = 'darkorange', color='darkred')+
  xlab('When House was Made')

# line graph of price and square meters
ggplot(data=dataset, aes(x=squareMeters, y=price))+geom_line(color = 'blue')+
  xlab('Square Meters')+ylab('Price')

# regression analysis
