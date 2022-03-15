##---------------------------------------------------
##---------------------------------------------------
##----- ADEC 7910: Software Tools for Data Analysis
##----- R Homework 03 Template
##----- Ross Walendziak, December 2021
##---------------------------------------------------
##---------------------------------------------------

##--------------------------------------------------------------------
##----- General
##--------------------------------------------------------------------
  
  # Clear the workspace
  rm(list = ls()) # Clear environment
  gc()            # Clear unused memory
  cat("\f")       # Clear the console
  if(!is.null(dev.list())) dev.off() # Clear all plots

  
  # Prepare needed libraries
  packages <- c("ggplot2"
                , "lemon"
                , "gridExtra" # For Q1
                , "ggrepel"   # For labels in Q2.b
                , "scales"
                )
  for (i in 1:length(packages)) {
    if (!packages[i] %in% rownames(installed.packages())) {
      install.packages(packages[i], dependencies = TRUE)
    }
    library(packages[i], character.only = TRUE)
  }
  rm(packages)
  
  # Set working directory and path to data, if need be
  setwd("/Users/Ross Walendziak/Documents/Software Tools/Assignment 5")
  
  # Load data
  sales <- read.csv("r.hw03.sales.csv"
  									, check.names = FALSE
  									, stringsAsFactors = FALSE
  									, na.strings = ""
  									)
  items <- read.csv("r.hw03.items.csv"
  									, check.names = FALSE
  									, stringsAsFactors = FALSE
  									, na.strings = ""
  									)
  
  # Merge data together
  sales <- merge(sales,
                 items,
                 by = "item.id",
                 #, by.x = "c.id", by.y = "customer.id" # If ID vars have different names in x and y
                 #, all.x = FALSE, all.y = FALSE # INNER join
                 all.x = TRUE, all.y = FALSE # LEFT join
                 #, all.x = FALSE, all.y = TRUE # RIGHT join
                 #, all = TRUE # FULL OUTER join
                 )
  
  # Reorder variables
  var.order <- c("date"
                , "category"
                , "subcategory"
                , "item.name"
                , "volume"
                , "price"
                , "sale.bottles"
                , "sale.volume"
                , "sale.dollars"
                )
  sales <- sales[,var.order]

##--------------------------------------------------------------------
##----- Q1
##--------------------------------------------------------------------  
  
  #Prepare the Data for plotting
  
  # Change order.date to type "date"
  sales$date <- as.Date(sales$date, format = "%Y-%m-%d")
  # Date formats:
  #   %Y: 4-digit year (1982)
  #   %y: 2-digit year (82)
  #   %m: 2-digit month (01)
  #   %d: 2-digit day of the month (13)
  #   %A: weekday (Wednesday)
  #   %a: abbreviated weekday (Wed)
  #   %B: month (January)
  #   %b: abbreviated month (Jan)
  
  # First we need to aggregate sales by date
  sales.agg <- aggregate(sale.dollars ~ date
                          , sales
                          , sum
  )
  
  # Next we need to create a dataset with all the dates from Jan 2015 - Dec 2015:
  sales.dates <- data.frame(date = seq(from = as.Date("2015-01-01")
                                              , to = as.Date("2015-12-31")
                                              , by = "day"
  )
  )
  # Now merge two together:
  sales.daily <- merge(sales.dates, sales.agg
                        , by = "date"
                        , all.x = TRUE
  )
  rm(sales.agg)
  rm(sales.dates)
  # Next we need to create calendar dimensions: days, weeks, months, quaters and years
  # Date formats:
  #   %Y: 4-digit year (1982)
  #   %y: 2-digit year (82)
  #   %m: 2-digit month (01)
  #   %d: 2-digit day of the month (13)
  #   %A: weekday (Wednesday)
  #   %a: abbreviated weekday (Wed)
  #   %B: month (January)
  #   %b: abbreviated month (Jan)
  # Days:
  sales.daily$day <- as.numeric(format(sales.daily$date, "%d"))

  sales.daily$weekday <- factor(format(sales.daily$date, "%a") # Alternatively, use weekdays() function
                                 , levels = rev(c("Mon" # See below for why
                                                  , "Tue"
                                                  , "Wed"
                                                  , "Thu"
                                                  , "Fri"
                                                  , "Sat"
                                                  , "Sun"
                                 )
                                 )
                                 , ordered = TRUE
  )
  # Week of month, as a difference between week of year current and week of year for 1st day of month
  # Calculate week of the year
  sales.daily$week <- as.numeric(format(sales.daily$date, "%W")) + 1 
  # Calculate week of year number for 1st day of every month
  tmp <- as.numeric(format(as.Date(cut(sales.daily$date, "month")), "%W"))
  sales.daily$week <- sales.daily$week - tmp
  rm(tmp)
  
  # Months:
  sales.daily$month <- factor(format(sales.daily$date, "%b") # Alternatively, use months() function
                               , levels = c("Jan"
                                            , "Feb"
                                            , "Mar"
                                            , "Apr"
                                            , "May"
                                            , "Jun"
                                            , "Jul"
                                            , "Aug"
                                            , "Sep"
                                            , "Oct"
                                            , "Nov"
                                            , "Dec"
                               )
                               , ordered = TRUE
  )
  # Quaters:
  sales.daily$quarter <- factor(quarters(sales.daily$date)
                                 , levels = c("Q1"
                                              , "Q2"
                                              , "Q3"
                                              , "Q4"
                                 )
                                 #, labels = c("", "", "", "") # To avoid seeing Q1 in pictures
                                 , ordered = TRUE
  )
  
  #Levels
  sales.daily$level = cut(sales.daily$sale.dollars,
                          breaks = c(0, 1000000, 1200000, 1400000, 1600000, 2000000),
                          labels = FALSE
                          )
  
  sales.daily$level[is.na(sales.daily$level)] = 0
  
  # Q1 chart
  q1 <- ggplot(sales.daily
                     , aes(x = weekday
                           , y = week
                           , fill = level
                     )
  ) +
    geom_tile(colour = "white") + # This creates a small rectangular for every date 
    geom_text(aes(label = day)) + # Day numbers inside tiles
    scale_fill_gradientn(colors = c('grey', 'red', 'orange', 'yellow', 'green', 'blue')# This uses a 6-color gradient scale 
                        , values = NULL
                        , space = "Lab"
                        , na.value = "gray"
                        , guide = 'legend'
                        , aesthetics = 'fill'
                        , name = ""
                        , labels = c( 'NA' 
                                     , paste(0,"-",1, "mln")
                                     , paste(1,"-",1.2, "mln")
                                     , paste(1.2,"-",1.4, "mln")
                                     , paste(1.4,"-",1.6, "mln")
                                     , paste(1.6,"-",2, "mln")
                                    )
    ) + 
    # facet_rep_wrap is from package "lemon" to keep month labels on every row
    facet_rep_wrap( ~ month   # formula defines which variables identify subsets of data for different facets
                    , ncol = 3 # This is needed to define when to wrap facets
                    , strip.position = "top"
                    , repeat.tick.labels = TRUE
    ) + 
    scale_y_reverse() + # Proper order of weeks
    scale_x_discrete(limits = rev(levels(sales.daily$weekday))) + # Proper order of weekdays
    labs(x = ""
         , y = ""
         , title = "Daily Total Sales, 2015"
    ) + 
    theme_bw() + 
    theme(strip.background = element_blank() # No background color
          , strip.placement = "top"
          , axis.text.y = element_blank()
          , legend.position = "bottom"
          , legend.box = 'horizontal'
    )
  
  q1
  
  rm(sales.daily)

  
  # Export Q1 chart
  png(filename = "q1.png", width = 1920, height = 1920, res = 240)
    q1
  dev.off()

##--------------------------------------------------------------------
##----- Q2
##--------------------------------------------------------------------  
  sales$price.per.l = (sales$price / sales$volume)*1000
  
  sales_btm_95 = sales[sales$price.per.l <  quantile(sales$price.per.l, probs = 0.95), ]
  
  sample_size = round(0.10*nrow(sales_btm_95), 0)
  
  set.seed(1)
  sample_sales_btm_95 = sales_btm_95[sample(nrow(sales_btm_95), sample_size), ]

    # Q2a chart
    q2a<- ggplot(sample_sales_btm_95, aes(x = category
                                         , y = price.per.l
                                         , fill = factor(category)
                                         , mapping = category
                                         )
                ) + 
    #Plot boxes
    geom_boxplot(aes(fill=factor(category)),
                 outlier.shape = 21,
                 outlier.size = 2,
                 outlier.color = "black",
                 outlier.fill = NULL
                 ) +
    #geom_boxplot(aes(fill=factor(category))) +
    scale_y_continuous(breaks = seq(from = 0        # Adjust x-axis ticks
                                    , to = 40
                                    , by = 2
    )
    , labels = seq(from = 0      # Adjust labels for ticks
                   , to = 40
                   , by = 2
    )
    ) +
    labs(title = "Liquor categories, price per liter"
         , subtitle = "Excludes top 5% of values"
         , x = "Category"
         , y = "Price per Liter, $"
    ) +
    coord_flip() + 
    theme_bw() + 
    theme(plot.title = element_text(hjust = 0.5
                                    , face = "bold"
                                    , size = 20
                                    , color = "#912600"
    )
    , plot.subtitle = element_text(hjust = 0.5
                                   , face = "bold"
                                   , size = 14
                                   , color = "#912600"
    )
    , axis.title.x = element_text(face = "bold"
                                  , color = "#912600"
                                  , size = 14
    )
    , axis.text.x  = element_text(face = "bold"
                                  , vjust = 0.5
                                  , size = 14
    )
    , axis.title.y = element_text(face = "bold"
                                  , color = "#912600"
                                  , size = 14
    )
    , axis.text.y  = element_text(face = "bold"
                                  , vjust = 0.5
                                  , size = 12
    )
    , legend.title = element_blank()
    )
  
  q2a
  
  rm(sample_sales_btm_95)
  rm(sales_btm_95)
  rm(sample_size)

####### Organize/gather/compute/prepare data for q2b  ########
  
  subcategory_total_volume = aggregate(volume ~ subcategory,
                                       sales,
                                       sum
                                       )
  
  names(subcategory_total_volume)[2] = "subcategory_volume"
  
  sales <- merge(sales,
                 subcategory_total_volume,
                 by = "subcategory",
                 all.x = TRUE, all.y = FALSE # LEFT join
                 )

  sales$price.per.l.w = (sales$price * (sales$volume / sales$subcategory_volume))
  
  subcategory_vol_weighted_price = aggregate(price.per.l.w ~ subcategory,
                                             sales,
                                             sum
                                             )
  q2b_df = merge(subcategory_total_volume,
                 subcategory_vol_weighted_price,
                 by = 'subcategory'
                 )
  
  rm(subcategory_total_volume)
  rm(subcategory_vol_weighted_price)
  
  sales_cat_subcat = unique(sales[, c('category', 'subcategory')])
  
  q2b_df = merge(q2b_df,
                 sales_cat_subcat,
                 by = 'subcategory',
                 all.x = TRUE, all.y = FALSE #LEFT JOIN
                 )
  
  rm(sales_cat_subcat)
  
  q2b_df$label = paste0(q2b_df$subcategory, ", ", round(q2b_df$price.per.l.w,1), " $/l")
  
  q2b_label_df = q2b_df[q2b_df$subcategory %in% c("80 Proof Vodkas",
                                                  "Canadian Whiskies",
                                                  "Spiced Rum",
                                                  "Vodkas",
                                                  "Miscellaneous Whiskies",
                                                  "Single Malt Scotch",
                                                  "Japanese Whisky")
                        , ]
                                                 
  q2b_df = q2b_df[, c('category',
                      'subcategory',
                      'price.per.l.w',
                      'subcategory_volume',
                      'label')]
  
  rm(sales_cat_subcat)
  
  #q2b Chart
  
  q2b <- ggplot(q2b_df
                , aes(x = price.per.l.w
                      , y = (subcategory_volume)/100000
                      , color = category
                      , shape = NULL
                      , mapping = label
                      )
                ) +
    geom_point(size = 5           # Size of points
               , alpha = 100          # Transparency of points
               , shape = 1
               ) +
    labs(title = "Liquor subcategories"
         , subtitle = "price vs quantity"
         , x = "Average weighted price per liter, $"
         , y = "Liters sold, thousands"
         ) +
    theme_bw() +
    geom_text_repel(aes(label=label),
                    data = q2b_label_df,
                    #max.overlaps = 4,
                    color = 'black',
                    arrow = TRUE) +
    scale_y_continuous(breaks = seq(from = 0        # Adjust y-axis ticks
                                    , to = 3000
                                    , by = 250
                                   )
                      , labels = seq(from = 0      # Adjust labels for ticks
                                     , to = 3000
                                     , by = 250
                                    )
                      ) +
    scale_x_continuous(breaks = seq(from = 0        # Adjust x-axis ticks
                                    , to = 80
                                    , by = 2
                                    )
                       , labels = seq(from = 0      # Adjust labels for ticks
                                      , to = 80
                                      , by = 2
                                      )
                       , limits = c(2, 86)
                       ) +
    
    theme(plot.title = element_text(hjust = 0.5
                                    , face = "bold"
                                    , size = 20
                                    , color = "#912600"
                                    )
    , plot.subtitle = element_text(hjust = 0.5
                                   , face = "bold"
                                   , size = 14
                                   , color = "#912600"
                                   )
    , axis.title.x = element_text(face = "bold"
                                  , color = "#912600"
                                  , size = 14
                                  )
    , axis.text.x  = element_text(face = "bold"
                                  , vjust = 0.5
                                  , size = 12
                                  )
    , axis.title.y = element_text(face = "bold"
                                  , color = "#912600"
                                  , size = 14
                                  )
    , axis.text.y  = element_text(face = "bold"
                                  , vjust = 0.5
                                  , size = 12
                                  )
    , legend.text = element_text(face = "bold"
                                 , size = 15
                                 )
    , legend.position = c(0.85, 0.8)    # Or use "bottom", "top", "left", "right"
    , legend.key.size = unit(1, 'lines') # Size of elements inside legend box
    , legend.title = element_blank()    # Hides legend title
    , legend.background = element_blank()
    )
  
  q2b
  
  rm(q2b_df)
  rm(q2b_label_df)
  
  # Export Q2a chart
  png(filename = "q2a.png", width = 2880, height = 1920, res = 240)
    q2a
  dev.off()
  
  # Export Q2b chart
  png(filename = "q2b.png", width = 2880, height = 1920, res = 240)
    q2b
  dev.off()
  
##--------------------------------------------------------------------
##----- Q3
##--------------------------------------------------------------------
  
  ####Crate Input dataframe for Question 3b####
  
  sales$weekday <- factor(format(sales$date, "%a") # Alternatively, use weekdays() function
                                , levels = rev(c("Mon" # See below for why
                                                 , "Tue"
                                                 , "Wed"
                                                 , "Thu"
                                                 , "Fri"
                                                 , "Sat"
                                                 , "Sun"
                                              ))
                                , ordered = TRUE
  )
  
  sales_weekday_category = aggregate(sale.dollars ~ weekday + category,
                                     sales,
                                     sum
                                     )
  
  sales_category = aggregate(sale.dollars ~ category,
                             sales,
                             sum)
  
  q3b_df = merge(sales_weekday_category,
                 sales_category,
                 by = 'category',
                 all.x = TRUE, all.y = FALSE #LEFT JOIN
                 )
  
  q3b_df$weekday_percent_of_sales = (q3b_df$sale.dollars.x / q3b_df$sale.dollars.y)*100
  

  ####Create input dataframe for question 3a####
  
  sales$month <- factor(format(sales$date, "%b") # Alternatively, use months() function
                              , levels = c("Jan"
                                           , "Feb"
                                           , "Mar"
                                           , "Apr"
                                           , "May"
                                           , "Jun"
                                           , "Jul"
                                           , "Aug"
                                           , "Sep"
                                           , "Oct"
                                           , "Nov"
                                           , "Dec"
                                           )
                              , ordered = TRUE
                              )
  
  sales_month_category = aggregate(sale.dollars ~ month + category,
                                   sales,
                                   sum
                                   )
  
  q3a_df = merge(sales_month_category,
                 sales_category,
                 by = 'category',
                 all.x = TRUE, all.y = FALSE #LEFT JOIN
                )
  
  q3a_df$month_percent_of_sales = (q3a_df$sale.dollars.x / q3a_df$sale.dollars.y)*100
  
  #Clean up extra variables / columns
  sales$weekday <- NULL
  sales$month <- NULL
  q3a_df$sale.dollars.x <- NULL
  q3a_df$sale.dollars.y <- NULL
  q3b_df$sale.dollars.x <- NULL
  q3b_df$sale.dollars.y <- NULL
  rm(sales_weekday_category)
  rm(sales_month_category)
  rm(sales_category)
  
  
  # Q3a chart

  q3a <- ggplot(q3a_df, aes(x = month
                            , y = month_percent_of_sales
                            , color = category,
                            )
                ) +
    geom_line(aes(group = category), size = 1) + 
    geom_point(size = 4,
               shape = 21,
               color = 'black',
               aes(fill=category)
               ) +
    theme_bw() +
    labs(title = "% of total sales per month"
         , subtitle = ""
         , x = "Month"
         , y = ""
    )
  
  q3a
  
  # Q3b chart
  q3b <- ggplot(q3b_df, aes(x = weekday #how do you reorder this without the use of the decs function?
                            , y = weekday_percent_of_sales
                            , color = category,
  )
  ) +
    geom_line(aes(group = category), size = 1) + 
    geom_point(size = 4,
               shape = 21,
               color = 'black',
               aes(fill=category)
    ) +
    theme_bw() +
    labs(title = "% of total sales per weekday"
         , subtitle = ""
         , x = "Weekday"
         , y = ""
    ) +
    scale_x_discrete(limits=rev) +
    theme(legend.position = "none")
  
  q3b
  
  rm(q3a_df)
  rm(q3b_df)
  
  # Export Q3a chart
  png(filename = "q3a.png", width = 2880, height = 1920, res = 240)
    q3a
  dev.off()
  
  # Export Q3b chart
  png(filename = "q3b.png", width = 2880, height = 1920, res = 240)
    q3b
  dev.off()
  
  # Export Q3c chart
  png(filename = "q3c.png", width = 2880, height = 1920, res = 240)
    grid.arrange(q3a, q3b, nrow = 1, ncol = 2, widths = c(2, 1))
  dev.off()
  
##--------------------------------------------------------------------
##----- Q4
##--------------------------------------------------------------------
  
  #Prep dollars
  sales_agg = aggregate(sale.dollars ~ category,
                        sales,
                        sum
                        )
  sales_agg = sales_agg[order(sales_agg$sale.dollars, decreasing = TRUE), ]
  sales_agg$rank.dollars = seq(1:10)
  sales_agg$sale.dollars <- NULL
  
  #Prep volume
  volume_agg = aggregate(sale.volume ~ category,
                         sales,
                         sum
                         )
  volume_agg = volume_agg[order(volume_agg$sale.volume, decreasing = TRUE), ]
  volume_agg$rank.volume = seq(1:10)
  volume_agg$sale.volume <- NULL
  
  #Prep bottles
  bottle_agg = aggregate(sale.bottles ~ category,
                         sales,
                         sum
                        )
  bottle_agg = bottle_agg[order(bottle_agg$sale.bottles, decreasing = TRUE), ]
  bottle_agg$rank.bottles = seq(1:10)
  bottle_agg$sale.bottles <- NULL
  
  #Merge Rankings
  q4_df = merge(sales_agg,
                volume_agg,
                 by = 'category',
                 all.x = TRUE, all.y = TRUE #Full Join
                )
  
  q4_df = merge(q4_df,
                bottle_agg,
                by = 'category',
                all.x = TRUE, all.y = TRUE #Full Join
                )
  
  rm(sales_agg)
  rm(volume_agg)
  rm(bottle_agg)
  
  q4_df
  
  # Q4 chart
  
  
    # Ranking based on absolute sales values
    q4 <- ggplot(q4_df) +
      geom_segment(aes(x = 4 , xend = 5, 
                       y = rank.dollars, yend = rank.volume, 
                       color = factor(category))) +
      geom_segment(aes(x = 5 , xend = 6, 
                       y = rank.volume, yend = rank.bottles, 
                       color = factor(category),
                       linetype = "solid"
                       )
                   ) +
      scale_x_continuous(breaks = c(1:9),
                         labels = c("", "", "", "Sales, $", "Sales, liters", 
                                    "Sales, bottles", "", "", "")
                        ) +
      coord_cartesian(xlim = c(4,6)) + 
      geom_vline(aes(xintercept = 4), linetype="dashed", size = 0.5) +
      geom_vline(aes(xintercept = 5), linetype="dashed", size = 0.5) +
      geom_vline(aes(xintercept = 6), linetype="dashed", size = 0.5) +
      scale_y_reverse(labels = NULL) + 
      geom_point(aes(x = 4, y = rank.dollars, color=factor(category), size = 100)) +
      geom_text(aes(x = 4, y = rank.dollars, label=rank.dollars)) +
      geom_point(aes(x = 5, y = rank.volume, color=factor(category), size = 100)) +
      geom_text(aes(x = 5, y= rank.volume, label=rank.volume)) +
      geom_point(aes(x = 6, y=rank.bottles, color=factor(category), size = 100)) +
      geom_text(aes(x = 6, y= rank.bottles, label=rank.bottles)) +
      geom_text(aes(label = category
                    , y = rank.dollars
                    , x = 4 - 0.05
                    )
                    , color = "black" # To avoid lables being colored same as lines
                    , hjust = 1, size = 3.5
                    ) + 
      geom_text(aes(label = category
                    , y = rank.bottles
                    , x = 6 + 0.25
                    )
                    , color = "black" # To avoid lables being colored same as lines
                    , hjust = 1, size = 3.5
                    ) + 
      labs(title = "Liquor Category Rankings"
           , subtitle = "" # To create more space under title
           , x = ""
           , y = ""
      ) +
      theme_bw() +
      theme(legend.position = "none",
            )
    
    q4
    rm(q4_df)  
      
      png(filename = "q4.png", width = 2880, height = 1920, res = 240)
      q4
      dev.off()