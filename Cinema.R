ticket_cost <- 75
ticket_cost_child <- 50
movies <- c('Monster Hunter',
            'The Croods: A New Age',
            'Honest Thief',
            'The Witches',
            'Survive The Night')
screens <- 13
seats <- 24  #The actual number of seats is 48,
             #but it split to half because of COVID-19 situation

week_days <- rep(0, 7)
total_revenues <- rep(0, 7)

Highest_rev <- 0
Day_counter <- 0

for (d in 1:length(week_days))
{
  revenue <- 0
  print(d)
        
  for (s in 1:screens)
  {
    visitors_adults <- sample(seats, 1)
    visitors_children <- sample((seats-visitors_adults),1)
          
    print('Adults Visitors:')
    print(visitors_adults)
          
    print('Children visitors:')
    print(visitors_children)
          
    totalvistors <- visitors_adults + visitors_children
    print('Total visitors:')
    print(totalvistors)

    adults_cost <- ticket_cost*visitors_adults
    children_cost <- ticket_cost_child*visitors_children
          
    adults_revenue <- (adults_cost*screens)
    children_revenue <- (children_cost*screens)
          
    total_revenue <- adults_revenue + children_revenue
    
    #Calculate the highest revenue
    if (total_revenue > Highest_rev)
    { Day_counter <- d
      Highest_rev <- total_revenue }
  }
        
  print('Revenue:')
  total_revenues[d] <- total_revenue
  print(total_revenues)
 
  #New Visitors
  visitors_adults <- 0
  visitors_children <- 0
}

#Plotting  
library(ggplot2)
  #Plot number 1
  qplot(total_revenues)
  
  #Plot number 2
  plot(total_revenues)
  
  #Plot number 3
  set.seed(2020)
    date <- seq(from = as.Date("2020-11-29"),
              to = as.Date("2020-12-05"),
              by = "days")
    value <- total_revenues
    dat <- data.frame(date, value)
  ggplot(dat, aes(x = date, y = value)) + 
          geom_point() + 
          geom_line() +
          scale_x_date(date_breaks = "day", date_labels = "%a") +
          theme_bw()
  
  #Plot number 4
  lbls <- c("1 Sun", "2 Mon", "3 Tue", "4 Wed",
            "5 Thur", "6 Fri", "7 Sat")
  pie(total_revenues, labels = lbls, main="Pie Chart of Revenue")
  
  
#Which day had the highest revenue?
  print("The Day that had the Highest Revenue is:")
  print(Day_counter)