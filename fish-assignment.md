
Unit 3: Fisheries Collapse Module
=================================

This module will focus on understanding and replicating fisheries stock assessment data and fisheries collapse. Follow along with Carl, as he live codes the first exercise and then complete the second as your group assignment.

The Database
------------

We will use data from the [RAM Legacy Stock Assessment Database](http://ramlegacy.marinebiodiversity.ca/ram-legacy-stock-assessment-database)

First, load in the necessary librarys. Note that this time we need a package we haven't used before `readxl`. This package is useful for reading in .xls or .xlsx files. As always if you want more info on a package run `?readxl` after loading it.

``` r
library("tidyverse")
library("readxl")
```

Reading in the tables
---------------------

``` r
## old link not working today:
#download.file("https://depts.washington.edu/ramlegac/wordpress/databaseVersions/RLSADB_v3.0_(assessment_data_only)_excel.zip", 

# backup copy for class:
download.file("https://github.com/espm-157/fish-template/releases/download/data/ramlegacy.zip", 
              "ramlegacy.zip")
path <- unzip("ramlegacy.zip")  #unzip the .xls files

sheets <- readxl::excel_sheets(path) #use the readxl package to identify sheet names 

ram <- lapply(sheets, readxl::read_excel, path = path)  #read the data from all 3 sheets into a list

names(ram) <- sheets # give the list of datatables their assigned sheet names

## check your names
names(ram)
```

    ##  [1] "area"                    "assessment"             
    ##  [3] "assessmethod"            "assessor"               
    ##  [5] "biometrics"              "bioparams"              
    ##  [7] "bioparams_ids_views"     "bioparams_units_views"  
    ##  [9] "bioparams_values_views"  "management"             
    ## [11] "stock"                   "taxonomy"               
    ## [13] "timeseries"              "timeseries_ids_views"   
    ## [15] "timeseries_units_views"  "timeseries_values_views"
    ## [17] "tsmetrics"

``` r
## check your data
head(ram$area)
```

    ## # A tibble: 6 x 6
    ##   country  areatype areacode areaname          alternateareana… areaid    
    ##   <chr>    <chr>    <chr>    <chr>             <chr>            <chr>     
    ## 1 Argenti… CFP      ARG-N    Northern Argenti… NA               Argentina…
    ## 2 Argenti… CFP      ARG-S    Southern Argenti… NA               Argentina…
    ## 3 Austral… AFMA     CASCADE  Cascade Plateau   NA               Australia…
    ## 4 Austral… AFMA     ESE      Eastern half of … NA               Australia…
    ## 5 Austral… AFMA     GAB      Great Australian… NA               Australia…
    ## 6 Austral… AFMA     MI       Macquarie Island  <NA>             Australia…

``` r
ram$taxonomy %>% 
  filter(genus == "Gadus") %>%
  select(tsn) %>%
  left_join(ram$stock) %>%
  left_join(ram$area)
```

    ## # A tibble: 24 x 14
    ##       tsn stockid scientificname commonname areaid stocklong region
    ##     <dbl> <chr>   <chr>          <chr>      <chr>  <chr>     <chr> 
    ##  1 164711 PCODBS… Gadus macroce… Pacific c… USA-N… Pacific … US Al…
    ##  2 164711 PCODGA  Gadus macroce… Pacific c… USA-N… Pacific … US Al…
    ##  3 164711 PCODHS  Gadus macroce… Pacific c… Canad… Pacific … Canad…
    ##  4 164711 PCODWC… Gadus macroce… Pacific c… Canad… Pacific … Canad…
    ##  5 164712 COD2J3… Gadus morhua   Atlantic … Canad… Atlantic… Canad…
    ##  6 164712 COD3M   Gadus morhua   Atlantic … multi… Atlantic… Canad…
    ##  7 164712 COD3NO  Gadus morhua   Atlantic … multi… Atlantic… Canad…
    ##  8 164712 COD3Pn… Gadus morhua   Atlantic … Canad… Atlantic… Canad…
    ##  9 164712 COD3Ps  Gadus morhua   Atlantic … Canad… Atlantic… Canad…
    ## 10 164712 COD4TVn Gadus morhua   Atlantic … Canad… Atlantic… Canad…
    ## # ... with 14 more rows, and 7 more variables: inmyersdb <dbl>,
    ## #   myersstockid <chr>, country <chr>, areatype <chr>, areacode <chr>,
    ## #   areaname <chr>, alternateareaname <chr>

``` r
ram$stock %>% group_by(tsn) %>%
  summarize(count = n()) %>%
  arrange(desc(count))
```

    ## # A tibble: 244 x 2
    ##       tsn count
    ##     <dbl> <int>
    ##  1 164712    20
    ##  2 161722    18
    ##  3 164744    11
    ##  4 660225     9
    ##  5 165000     8
    ##  6 551209     8
    ##  7 164722     7
    ##  8 173001     7
    ##  9  98428     6
    ## 10 161813     6
    ## # ... with 234 more rows

``` r
ram$stock
```

    ## # A tibble: 513 x 9
    ##    stockid    tsn scientificname commonname areaid stocklong region
    ##    <chr>    <dbl> <chr>          <chr>      <chr>  <chr>     <chr> 
    ##  1 ACADRE… 166774 Sebastes fasc… Acadian r… USA-N… Acadian … US Ea…
    ##  2 AFLONCH 166156 Beryx splende… Alfonsino  multi… Alfonsin… South…
    ##  3 ALBAIO  172419 Thunnus alalu… albacore … multi… Albacore… India…
    ##  4 ALBAMED 172419 Thunnus alalu… albacore … multi… Albacore… Medit…
    ##  5 ALBANA… 172419 Thunnus alalu… Albacore … multi… Albacore… Atlan…
    ##  6 ALBANP… 172419 Thunnus alalu… Albacore … Multi… Albacore… US We…
    ##  7 ALBASA… 172419 Thunnus alalu… albacore … multi… Albacore… Atlan…
    ##  8 ALBASP… 172419 Thunnus alalu… Albacore … multi… Albacore… Pacif…
    ##  9 ALPLAI… 172901 Pleuronectes … Alaska pl… USA-N… Alaska p… US Al…
    ## 10 AMPL23K 172877 Hippoglossoid… American … Canad… American… Canad…
    ## # ... with 503 more rows, and 2 more variables: inmyersdb <dbl>,
    ## #   myersstockid <chr>

``` r
cod_tsn <- cod_time_area <- ram$taxonomy %>% 
  filter(genus == "Gadus") %>%
  select(tsn)

cod_tsn
```

    ## # A tibble: 2 x 1
    ##      tsn
    ##    <dbl>
    ## 1 164711
    ## 2 164712

``` r
cod <- ram$taxonomy %>% 
  filter(scientificname == "Gadus morhua") %>%
  select(tsn) %>%
  left_join(ram$stock, by = "tsn") %>%
  left_join(ram$area, by = "areaid") %>%
  left_join(ram$timeseries, by= "stockid") %>%
  left_join(ram$tsmetrics, by = c("tsid" = "tsunique")) %>%
  filter(tscategory == "CATCH or LANDINGS")  
  
cod
```

    ## # A tibble: 2,389 x 24
    ##       tsn stockid scientificname commonname areaid stocklong.x region
    ##     <dbl> <chr>   <chr>          <chr>      <chr>  <chr>       <chr> 
    ##  1 164712 COD2J3… Gadus morhua   Atlantic … Canad… Atlantic c… Canad…
    ##  2 164712 COD2J3… Gadus morhua   Atlantic … Canad… Atlantic c… Canad…
    ##  3 164712 COD2J3… Gadus morhua   Atlantic … Canad… Atlantic c… Canad…
    ##  4 164712 COD2J3… Gadus morhua   Atlantic … Canad… Atlantic c… Canad…
    ##  5 164712 COD2J3… Gadus morhua   Atlantic … Canad… Atlantic c… Canad…
    ##  6 164712 COD2J3… Gadus morhua   Atlantic … Canad… Atlantic c… Canad…
    ##  7 164712 COD2J3… Gadus morhua   Atlantic … Canad… Atlantic c… Canad…
    ##  8 164712 COD2J3… Gadus morhua   Atlantic … Canad… Atlantic c… Canad…
    ##  9 164712 COD2J3… Gadus morhua   Atlantic … Canad… Atlantic c… Canad…
    ## 10 164712 COD2J3… Gadus morhua   Atlantic … Canad… Atlantic c… Canad…
    ## # ... with 2,379 more rows, and 17 more variables: inmyersdb <dbl>,
    ## #   myersstockid <chr>, country <chr>, areatype <chr>, areacode <chr>,
    ## #   areaname <chr>, alternateareaname <chr>, assessid <chr>,
    ## #   stocklong.y <chr>, tsid <chr>, tsyear <dbl>, tsvalue <dbl>,
    ## #   tscategory <chr>, tsshort <chr>, tslong <chr>, tsunitsshort <chr>,
    ## #   tsunitslong <chr>

``` r
#note: changed from filtering by genus to by species 
```

``` r
cod <- ram$taxonomy %>% 
  left_join(ram$stock, by = "tsn") %>%
  left_join(ram$area, by = "areaid") %>%
  left_join(ram$timeseries, by= "stockid") %>%
  left_join(ram$tsmetrics, by = c("tsid" = "tsunique")) %>%

  filter(tscategory == "CATCH or LANDINGS")  %>%
  filter(tsunitsshort == "MT") %>%
  filter(genus == "Gadus", species == "morhua") 
  

cod
```

    ## # A tibble: 2,389 x 38
    ##       tsn scientificname.x kingdom phylum classname ordername family genus
    ##     <dbl> <chr>            <chr>   <chr>  <chr>     <chr>     <chr>  <chr>
    ##  1 164712 Gadus morhua     Animal… Chord… Actinopt… Gadiform… Gadid… Gadus
    ##  2 164712 Gadus morhua     Animal… Chord… Actinopt… Gadiform… Gadid… Gadus
    ##  3 164712 Gadus morhua     Animal… Chord… Actinopt… Gadiform… Gadid… Gadus
    ##  4 164712 Gadus morhua     Animal… Chord… Actinopt… Gadiform… Gadid… Gadus
    ##  5 164712 Gadus morhua     Animal… Chord… Actinopt… Gadiform… Gadid… Gadus
    ##  6 164712 Gadus morhua     Animal… Chord… Actinopt… Gadiform… Gadid… Gadus
    ##  7 164712 Gadus morhua     Animal… Chord… Actinopt… Gadiform… Gadid… Gadus
    ##  8 164712 Gadus morhua     Animal… Chord… Actinopt… Gadiform… Gadid… Gadus
    ##  9 164712 Gadus morhua     Animal… Chord… Actinopt… Gadiform… Gadid… Gadus
    ## 10 164712 Gadus morhua     Animal… Chord… Actinopt… Gadiform… Gadid… Gadus
    ## # ... with 2,379 more rows, and 30 more variables: species <chr>,
    ## #   myersname <chr>, commonname1 <chr>, commonname2 <chr>,
    ## #   myersscientificname <chr>, myersfamily <chr>, FisheryType <chr>,
    ## #   stockid <chr>, scientificname.y <chr>, commonname <chr>, areaid <chr>,
    ## #   stocklong.x <chr>, region <chr>, inmyersdb <dbl>, myersstockid <chr>,
    ## #   country <chr>, areatype <chr>, areacode <chr>, areaname <chr>,
    ## #   alternateareaname <chr>, assessid <chr>, stocklong.y <chr>,
    ## #   tsid <chr>, tsyear <dbl>, tsvalue <dbl>, tscategory <chr>,
    ## #   tsshort <chr>, tslong <chr>, tsunitsshort <chr>, tsunitslong <chr>

``` r
#note: changed from filtering by genus to by species (Dgfa pacific)
```

``` r
cod %>% 
group_by(tsyear) %>%
summarize(total_catch = sum(tsvalue, na.rm= TRUE)) %>%
ggplot(aes(tsyear, total_catch)) + geom_line()
```

![](fish-assignment_files/figure-markdown_github/unnamed-chunk-10-1.png)

``` r
cod %>%
  select (areaname, areacode, areaid, region, country) %>%
  distinct()
```

    ## # A tibble: 20 x 5
    ##    areaname                areacode areaid           region      country  
    ##    <chr>                   <chr>    <chr>            <chr>       <chr>    
    ##  1 Southern Labrador-East… 2J3KL    Canada-DFO-2J3KL Canada Eas… Canada   
    ##  2 Flemish Cap             3M       multinational-N… Canada Eas… multinat…
    ##  3 Southern Grand Banks    3NO      multinational-N… Canada Eas… multinat…
    ##  4 Northern Gulf of St. L… 3Pn4RS   Canada-DFO-3Pn4… Canada Eas… Canada   
    ##  5 St. Pierre Bank         3Ps      Canada-DFO-3Ps   Canada Eas… Canada   
    ##  6 Southern Gulf of St. L… 4T       Canada-DFO-4T    Canada Eas… Canada   
    ##  7 Eastern Scotian Shelf   4VsW     Canada-DFO-4VsW  Canada Eas… Canada   
    ##  8 Western Scotian Shelf   4X       Canada-DFO-4X    Canada Eas… Canada   
    ##  9 Western Baltic          22-24    multinational-I… European U… multinat…
    ## 10 Eastern Baltic          25-32    multinational-I… European U… multinat…
    ## 11 Faroe Plateau           Vb1      multinational-I… Europe non… multinat…
    ## 12 Georges Bank            5Z       USA-NMFS-5Z      US East Co… USA      
    ## 13 Gulf of Maine           5Y       USA-NMFS-5Y      US East Co… USA      
    ## 14 Iceland Grounds         Va       multinational-I… Europe non… multinat…
    ## 15 Irish Sea               VIIa     multinational-I… European U… multinat…
    ## 16 Kattegat and Skagerrak  IIIa     multinational-I… European U… multinat…
    ## 17 North-East Arctic       I-II     multinational-I… Europe non… multinat…
    ## 18 North Sea               IV       multinational-I… European U… multinat…
    ## 19 West of Scotland        VIa      multinational-I… European U… multinat…
    ## 20 Celtic Sea              VIIe-k   multinational-I… European U… multinat…

``` r
#Which in north atlantic, which not?
#Filter out area IDs by hand
#First, get rid of pacific cod. Filter by species rather than genus
```

``` r
ram$tsmetrics %>%
  filter(tscategory == "CATCH or LANDINGS") %>%
  left_join(ram$timeseries, by = c("tsunique" = "tsid"))
```

    ## # A tibble: 47,279 x 11
    ##    tscategory tsshort tslong tsunitsshort tsunitslong tsunique assessid
    ##    <chr>      <chr>   <chr>  <chr>        <chr>       <chr>    <chr>   
    ##  1 CATCH or … Ctouse  Catch… MT           Metric tons Ctouse-… NEFSC-A…
    ##  2 CATCH or … Ctouse  Catch… MT           Metric tons Ctouse-… NEFSC-A…
    ##  3 CATCH or … Ctouse  Catch… MT           Metric tons Ctouse-… NEFSC-A…
    ##  4 CATCH or … Ctouse  Catch… MT           Metric tons Ctouse-… NEFSC-A…
    ##  5 CATCH or … Ctouse  Catch… MT           Metric tons Ctouse-… NEFSC-A…
    ##  6 CATCH or … Ctouse  Catch… MT           Metric tons Ctouse-… NEFSC-A…
    ##  7 CATCH or … Ctouse  Catch… MT           Metric tons Ctouse-… NEFSC-A…
    ##  8 CATCH or … Ctouse  Catch… MT           Metric tons Ctouse-… NEFSC-A…
    ##  9 CATCH or … Ctouse  Catch… MT           Metric tons Ctouse-… NEFSC-A…
    ## 10 CATCH or … Ctouse  Catch… MT           Metric tons Ctouse-… NEFSC-A…
    ## # ... with 47,269 more rows, and 4 more variables: stockid <chr>,
    ## #   stocklong <chr>, tsyear <dbl>, tsvalue <dbl>

Exercise 1: Investigating the North-Atlantic Cod
================================================

First, We seek to replicate the following figure from the Millenium Ecosystem Assessment Project using the RAM data.

![](https://github.com/espm-157/website/raw/master/static/img/cod.jpg)

Task 1: Joining the necessary data
----------------------------------

To replicate this plot, we need a table with the following columns: `"country"`, `"ssb_unit"`, `"catch_landings_unit"`, `"scientificname"`, `"commonname"`, `"year"`, `"ssb"`, and `"TC"`.

Using the `select()` and `join()` functions you were introduced to in in Module 1, build a tidy table with the desired columns.

``` r
 ram$bioparams_values_views
```

    ## # A tibble: 376 x 20
    ##    assessid stockid stocklong    Bmsy SSBmsy  Nmsy    MSY    Fmsy    Umsy
    ##    <chr>    <chr>   <chr>       <dbl>  <dbl> <dbl>  <dbl>   <dbl>   <dbl>
    ##  1 NEFSC-A… ACADRE… Acadian …     NA  271000    NA  10139  0.0377  0.0163
    ##  2 IOTC-AL… ALBAIO  Albacore… 346291.     NA    NA  27022  0.18   NA     
    ##  3 ICCAT-A… ALBAMED Albacore… 167213.     NA    NA  28900 NA      NA     
    ##  4 ICCAT-A… ALBANA… Albacore…  86556.  53660    NA  29000  0.175   0.228 
    ##  5 ISC-ALB… ALBANP… Albacore… 806325. 342854    NA 119094  0.122   0.147 
    ##  6 ICCAT-A… ALBASA… Albacore… 200546.     NA    NA  27964 NA      NA     
    ##  7 SPC-ALB… ALBASP… Albacore… 692000  120000    NA  97610 NA       0.141 
    ##  8 AFSC-AL… ALPLAI… Alaska p…     NA  178200    NA     NA  0.19   NA     
    ##  9 NAFO-SC… AMPL3L… American…     NA      NA    NA     NA NA      NA     
    ## 10 NAFO-SC… AMPL3M  American…     NA      NA    NA     NA NA      NA     
    ## # ... with 366 more rows, and 11 more variables: B0 <dbl>, SSB0 <dbl>,
    ## #   M <dbl>, Bmsytouse <dbl>, Umsytouse <dbl>, Bmgt <lgl>, SSBmgt <dbl>,
    ## #   Fmgt <dbl>, Umgt <dbl>, Bmgttouse <dbl>, Umgttouse <dbl>

Task 2: Mapping the Area table to marine regions
------------------------------------------------

In order to replicate the collapse of Atlantic Cod, we need to be able to map area table from the Ram database to the marine regions.

*As an aside, this database is unclear what kind of areas the `area` table is using, they do not appear to be LMEs, EEZs, or other obvious marine region classification. Regardless, we will use them to extract the North America cod stocks.*

Write code to pull all marine areas (listed in `ram$area`) that contain a certain substring in their name -- ex. "Georges Bank". Hint: you want want to consider functions `filter()` or `grep()`

We are interested in mapping the data from just the areas where Atlantic Cod are found. Using the table you built above, pull out distinct areas that contain Atlantic Cod populations into a new tidytable. Hint: you may want to use functions like `filter()` or `distinct()`

Task 3: Subsetting our data by regional id
------------------------------------------

Using bracket notation and or the `filter()` and `pull()` functions, try pulling certain subsets of ids from your table of cod areas. ex. the first 8 ids, or the ids of areas just within a certain country.

Create a vector of ids of areas with Atlantic Cod and in Canada.

Task 4: Plotting Total Catch in Canada
--------------------------------------

Calculate and plot the catch in million tons (MT) of Atlantic Cod from Canada using the data table and vector of ids you created above. Hint: you may want to use functions like `group_by()`, `filter()`, and/or `summarise()`

``` r
cod %>%
  filter(country == "Canada") %>%
  group_by(tsyear) %>%
  summarise(total_catch = sum(tsvalue, na.rm=TRUE)) %>%
  ggplot(aes(tsyear, total_catch)) + geom_line()
```

![](fish-assignment_files/figure-markdown_github/unnamed-chunk-17-1.png)

**Question:** How does this graph compare to the one presented above?

Based on the amount of cod caught in Canada in the 19th century, Canada has a longer history of heavy fishing and it's water have been heavily fished consitently over time. Simialr to the graph of the total fish caught throughout the world, the amount of fish caught spikedto its highest amount around the 1960s. But unlike the cod catches worldwide, the amount caught in Canada dropped overall in the late 1970s, due a regulation put in place to put regulation of fishing in control of the goverment for 200 miles offshore. This is not reflected in the worldwide graph because this regulationmonly occured in Canadian waters.

Exercise 2: Group Assignment
============================

Stock Collapses
---------------

We seek to replicate the temporal trend in stock declines shown in [Worm et al 2006](http://doi.org/10.1126/science.1132294):

![](https://espm-157.carlboettiger.info/img/worm2006.jpg)

**Question 1:** What years does this plot include? What is it plotting?

This graph plots data from about 1950 to 2006. This graph is plotting species richness in differnet areas. The diamond shapes are plotting by year, and the triangles are plotting cumulative losses in species richness. The marine ecosystems lacking in species richness are expressing a much faster rate of decline than the marine ecosystems with high levels of species richness.

Task 1: Plotting total taxa caught worldwide 1950-2006
------------------------------------------------------

Adapting the table you created in the first exercise, select and manipulate the necessary columns to plot the number of total taxa caught each year from 1950 til 2006 using `geom_point()`.

Hint: you may want to use functions like `group_by()`, `tally()` and be sure to carefully consider how to handle or omit missing values.

``` r
# carl/andrew code, solid
fishes <-
  ram$timeseries_values_views %>%
  select(assessid, stockid, year, SSB, TC) %>%
  left_join(ram$stock) %>%
  left_join(ram$area) %>% 
  left_join(ram$timeseries_units_views %>% 
      rename(TC_units = TC, SSB_units = SSB)) %>% 
  select(commonname, areaname, country, year, SSB, TC,
         TC_units, SSB_units,
         scientificname, assessid, stockid, areaid)
fishes
```

    ## # A tibble: 22,396 x 12
    ##    commonname areaname country  year    SSB    TC TC_units SSB_units
    ##    <chr>      <chr>    <chr>   <dbl>  <dbl> <dbl> <chr>    <chr>    
    ##  1 Acadian r… Gulf of… USA      1913 642256     7 MT       MT       
    ##  2 Acadian r… Gulf of… USA      1914 642246    30 MT       MT       
    ##  3 Acadian r… Gulf of… USA      1915 642221    40 MT       MT       
    ##  4 Acadian r… Gulf of… USA      1916 642194    53 MT       MT       
    ##  5 Acadian r… Gulf of… USA      1917 642167    82 MT       MT       
    ##  6 Acadian r… Gulf of… USA      1918 642156    73 MT       MT       
    ##  7 Acadian r… Gulf of… USA      1919 642209    25 MT       MT       
    ##  8 Acadian r… Gulf of… USA      1920 642334    31 MT       MT       
    ##  9 Acadian r… Gulf of… USA      1921 642510    13 MT       MT       
    ## 10 Acadian r… Gulf of… USA      1922 642743     9 MT       MT       
    ## # ... with 22,386 more rows, and 4 more variables: scientificname <chr>,
    ## #   assessid <chr>, stockid <chr>, areaid <chr>

``` r
#The filters below were removed because they were deemed unneccesary
 #filter(tscategory == "CATCH or LANDINGS")  %>%
 #filter(tsunitsshort == "MT") %>%
first_fish <- fishes %>%   
  group_by(year, scientificname) %>%
  summarize(combine_stocks = sum(TC, na.rm = TRUE)) %>%
  filter(year >= 1950, year <= 2006) %>%
  na.omit() %>%
  group_by(year) %>%
  tally() 

  
first_fish
```

    ## # A tibble: 57 x 2
    ##     year     n
    ##    <dbl> <int>
    ##  1  1950    72
    ##  2  1951    75
    ##  3  1952    78
    ##  4  1953    79
    ##  5  1954    79
    ##  6  1955    82
    ##  7  1956    88
    ##  8  1957    89
    ##  9  1958    91
    ## 10  1959    93
    ## # ... with 47 more rows

``` r
ggplot(first_fish, aes(x= year, n)) + geom_point()
```

![](fish-assignment_files/figure-markdown_github/unnamed-chunk-20-1.png)

Task 2: Removing incomplete datasets
------------------------------------

Species can either have missing data (within a series) or a time range that just doesn't span the full interval. Grouping by stockid instead of year, build a character vector containing only those stockids that have data for the full range (1950-2006).

``` r
#filtering out the tally using "57" collects the values for which there is data for all 57 years.
second_fish <- fishes %>%   
  group_by(year, scientificname) %>%
  summarize(combine_stocks = sum(TC)) %>%
  filter(year >= 1950, year <= 2006) %>%
  na.omit() %>%
  group_by(scientificname) %>%
  count(by = "tsyear") %>%
  filter(n == 57)
 
  
second_fish
```

    ## # A tibble: 34 x 3
    ## # Groups:   scientificname [34]
    ##    scientificname            by         n
    ##    <chr>                     <chr>  <int>
    ##  1 Eopsetta jordani          tsyear    57
    ##  2 Epigonus telescopus       tsyear    57
    ##  3 Hippoglossus hippoglossus tsyear    57
    ##  4 Hippoglossus stenolepis   tsyear    57
    ##  5 Hyperoglyphe antarctica   tsyear    57
    ##  6 Katsuwonus pelamis        tsyear    57
    ##  7 Limanda ferruginea        tsyear    57
    ##  8 Merluccius capensis       tsyear    57
    ##  9 Merluccius paradoxus      tsyear    57
    ## 10 Microstomus pacificus     tsyear    57
    ## # ... with 24 more rows

``` r
 #....could also have used  na.omit()
```

**Question 2:** How many taxa have data for the full range? 242? 57?

``` r
taxa <-
ram$taxonomy %>% 
  left_join(ram$stock, by = "tsn") %>%
  left_join(ram$area, by = "areaid") %>%
  left_join(ram$timeseries, by= "stockid") %>%
  left_join(ram$tsmetrics, by = c("tsid" = "tsunique")) %>%
  

  group_by(scientificname.x) %>%
  
  filter(tsyear >= 1950) %>%
  filter(tsyear <= 2006)
taxa
```

    ## # A tibble: 218,178 x 38
    ## # Groups:   scientificname.x [242]
    ##      tsn scientificname.x kingdom phylum classname ordername family genus
    ##    <dbl> <chr>            <chr>   <chr>  <chr>     <chr>     <chr>  <chr>
    ##  1  -999 Pseudocarcinus … Animal… Arthr… Malacost… Decapoda  Menip… Pseu…
    ##  2  -999 Pseudocarcinus … Animal… Arthr… Malacost… Decapoda  Menip… Pseu…
    ##  3  -999 Pseudocarcinus … Animal… Arthr… Malacost… Decapoda  Menip… Pseu…
    ##  4  -999 Pseudocarcinus … Animal… Arthr… Malacost… Decapoda  Menip… Pseu…
    ##  5  -999 Pseudocarcinus … Animal… Arthr… Malacost… Decapoda  Menip… Pseu…
    ##  6  -999 Pseudocarcinus … Animal… Arthr… Malacost… Decapoda  Menip… Pseu…
    ##  7  -999 Pseudocarcinus … Animal… Arthr… Malacost… Decapoda  Menip… Pseu…
    ##  8  -999 Pseudocarcinus … Animal… Arthr… Malacost… Decapoda  Menip… Pseu…
    ##  9  -999 Pseudocarcinus … Animal… Arthr… Malacost… Decapoda  Menip… Pseu…
    ## 10  -999 Pseudocarcinus … Animal… Arthr… Malacost… Decapoda  Menip… Pseu…
    ## # ... with 218,168 more rows, and 30 more variables: species <chr>,
    ## #   myersname <chr>, commonname1 <chr>, commonname2 <chr>,
    ## #   myersscientificname <chr>, myersfamily <chr>, FisheryType <chr>,
    ## #   stockid <chr>, scientificname.y <chr>, commonname <chr>, areaid <chr>,
    ## #   stocklong.x <chr>, region <chr>, inmyersdb <dbl>, myersstockid <chr>,
    ## #   country <chr>, areatype <chr>, areacode <chr>, areaname <chr>,
    ## #   alternateareaname <chr>, assessid <chr>, stocklong.y <chr>,
    ## #   tsid <chr>, tsyear <dbl>, tsvalue <dbl>, tscategory <chr>,
    ## #   tsshort <chr>, tslong <chr>, tsunitsshort <chr>, tsunitslong <chr>

``` r
taxa %>%
  select ("scientificname.x", "stockid", "commonname") %>%
  tally()
```

    ## # A tibble: 242 x 2
    ##    scientificname.x        n
    ##    <chr>               <int>
    ##  1 Allocyttus niger     1039
    ##  2 Amblyraja radiata      43
    ##  3 Ammodytes marinus     648
    ##  4 Anoplopoma fimbria   2370
    ##  5 Arctica islandica     203
    ##  6 Aristeus antennatus    99
    ##  7 Arius spp              99
    ##  8 Arripis trutta        512
    ##  9 Balistes capriscus    240
    ## 10 Beryx splendens        72
    ## # ... with 232 more rows

Task 3: Which fisheries have collapsed?
---------------------------------------

A fishery may be considered *collapsed* when total catch (TC) falls below 10% of its peak. For those stocks with complete data sets, create a new tidy table including columns: `stockid`, `TC`, `year`, `collapsed`, and `cumulative`, where `collapsed` is a logical (True or False) for whether or not that fishery could be considered collapsed in that year, and `cumulative` is the count of total years the fishery has been collapsed at that point in time.

``` r
collapse <- second_fish %>%
  left_join(fishes) %>%
  select(TC, year, scientificname) %>%
  na.omit() %>%
  unique() %>%
  filter(year >= 1950) %>%
  filter(year <= 2006)
collapse
```

    ## # A tibble: 3,388 x 3
    ## # Groups:   scientificname [34]
    ##       TC  year scientificname  
    ##    <dbl> <dbl> <chr>           
    ##  1 4586.  1950 Eopsetta jordani
    ##  2 3039   1951 Eopsetta jordani
    ##  3 2834.  1952 Eopsetta jordani
    ##  4 2254.  1953 Eopsetta jordani
    ##  5 2887.  1954 Eopsetta jordani
    ##  6 2840.  1955 Eopsetta jordani
    ##  7 2233.  1956 Eopsetta jordani
    ##  8 2823.  1957 Eopsetta jordani
    ##  9 2385.  1958 Eopsetta jordani
    ## 10 2562.  1959 Eopsetta jordani
    ## # ... with 3,378 more rows

Task 4: Plotting total catch
----------------------------

Using `geom_area()` plot the TC per stockid acros all years.

Task 5: Calculating percent collapsed
-------------------------------------

To replicate the original plot, we must calculate the percent of taxa collapsed over time. Using the `summarise()` function, and only the core stocks that have data across the full interval, build a new tidy table that gives the fraction of all stocks that are collapsed in each year and include a cumulative column that gives the fraction of all years (between 1950 and each year) that has experience at least one collapse.

Hint: when logical vectors are summed or converted to numerics, TRUE = 1 and FALSE = 0.

Task 6: Plotting proportion collapsed over time.
------------------------------------------------

Using `geom_line` twice to plot two individual lines (of different colors please), plot the cumulative number of collapsed fisheries through time and the fraction of collapsed fishers through time on the same graph.

Hint: try using `scale_y_reverse()` to flip the y axis for a different perspective on these fractions.

**Question 3:** What does this graph show us? How is it presenting information differently than the original graph for this exercise? Is it presenting the same information?
