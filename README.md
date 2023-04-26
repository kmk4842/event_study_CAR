# Event study calculations in R
This script generates cumulative abnormal returns on stocks in daily trading windows around specified dates. I developed it for Polish stocks using data from [stooq.pl](http://www.stooq.pl), but it can easily be adapted to other data. Results are standardized to normal distribution, so that you can run simple statistical tests for significance.

The script does not rely on any special libraries except chron, so you have full control over all calculations.

## File structure 
* CSAR_calculations.R is the main script that you'd modify as needed
* CSAR_scripts.R contains functions that you would not normaly modify
* wse_stocks.csv is an example file of daily stock quotes
* wse_wig.csv is an example file of daily stock index quotes
* events.csv is an example file of dates around which CARs are calculated
## References
* The event study method comes from: Loderer, C.F. & Mauer, D.C. (1992). Corporate Dividends and Seasoned Equity Issues: An Empirical Investigation. Journal of Finance, 47(1), 201-225. https://doi.org/10.1111/j.1540-6261.1992.tb03983.x
* I developed this script for: Dobija, D., Klimczak K.M., Roztocki, N., Weistroffer, H. R. (2012). Information technology investment announcements and market value in transition economies: Evidence from Warsaw Stock Exchange. The Journal of Strategic Information Systems, 21(4), 308–319. https://doi.org/10.1016/j.jsis.2012.07.003
* I used it also for: Klimczak K.M. (2011). [Market reaction to mandatory IFRS adoption: Evidence from Poland](http://online-cig.ase.ro/jcig/art/10_2_6.pdf), Accounting and Management Information Systems, 10(2), 228-248.
* And for: Klimczak K. M., Dynel M. (2018). Evaluation Markers and Mitigators in Analyst Reports in Light of Market Response to Stock Recommendations. International Journal of Business Communication, 55(3), pp. 310-337. https://doi.org/10.1177/2329488417738082