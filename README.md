# Liquor-Sales-Data-Visualization
Visualizing liquor sales data using GGPLOT2

# Project Overview:
* Summarized a large liquor sales data from the state of Iowa to visually depict liquor sales from multiple angles.
* Specifically, GGPLOT2 was used to produce visualizations for:
    * A calander heatmap for sales data from the year 2015.
    * A box-and-whisker plot for the pricing of liquor categories.
    * Scatter plot of price vs volume across liquor categories.
    * Time series plot of monthly liquor sales by category.
    * Time series plot of weekly liquor sales by category.
    * Ranking of liquor categories by total sales dollars, total sales volume and number of by bottles sold.

# Data Source:
https://data.iowa.gov/Sales-Distribution/Iowa-Liquor-Sales/m3tr-qhgy

# Code and Resources Used:
* R Version: 4.1.2
* Packages: ggplot2, lemon, gridExtra, ggrepel, scales

# Data Management:
* The sales and items data files were merged together on "item.id"
* The date variable was converted to date object.
* Factor variables were created for the day of the week, month of the year and quarter of the year.
* A random sample of the bottom 95% of the data is taken for astetic display of the scatter plot.
* Sales dollars and sales volume data is aggregated by liquor subcategory for time series plots.
* All visuals are output to the cwd as .png files.

# Visualization Results:

![](https://github.com/ross-walendziak/Liquor-Sales-Data-Visualization/blob/main/Graphics/q1.png)

![](https://github.com/ross-walendziak/Liquor-Sales-Data-Visualization/blob/main/Graphics/q2a.png)

![](https://github.com/ross-walendziak/Liquor-Sales-Data-Visualization/blob/main/Graphics/q2b.png)

![](https://github.com/ross-walendziak/Liquor-Sales-Data-Visualization/blob/main/Graphics/q3c.png)

![](https://github.com/ross-walendziak/Liquor-Sales-Data-Visualization/blob/main/Graphics/q4.png)
