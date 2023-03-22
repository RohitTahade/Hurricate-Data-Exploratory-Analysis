# Exploratory analysis of Atlantic hurricanes

### Background
Hurdat2 is a database of hurricanes and similar storms produced by the US National Oceanic and Atmospheric
Administration (NOAA). It contains data on
• the track each storm followed (latitude-longitude coordinates)
• the wind speed in knots (1 knot = 0.52 m/s) and the classification of the storm at each point along its track.
• the point at which each storm crossed onto land if it did cross onto land (“landfall”)
and a number of other variables, all organised by a unique ID code for each storm and sometimes a name. Databases 
like this are crucial for understanding how climate change has affected patterns of extreme weather and predicting 
how the choices we make about carbon emissions shape extreme weather in the future.
The original database is stored in a somewhat awkward format, but I have tidied it and you will find the full dataset 
for the Atlantic Ocean region in the dataframe hurrs inside the file Hurdata_tidy.RData.
The documentation from NOAA is at https://www.nhc.noaa.gov/data/hurdat/hurdat2-format-nov2019.pdf 
Background information on the Hurdat2 project and hurricane science in general are at https://www.noaa. 
gov/education/resource-collections/weather-atmosphere/hurricanes https://www.aoml.noaa.gov/hrd/hurdat/ 
Data_Storm.html
I have removed some variables and simplified the classification system. The data set can be found in 
“/processed_data” we will use the word “storm” to mean “anything with a unique ID in the database” and the column 
is Hurricane is TRUE/FALSE depending on whether the storm system had reached hurricane status (based on wind 
speed) at a given point in time.
    
    
    #calling required libraries
    library(tidyverse)

## Question 1 : What’s in this dataset?

#### a. Load the dataframe. Pick one famous hurricane that you know the name and make a dataframe similar to hurrs but containing only the data for your chosen hurricane. (Note that the people who name hurricanes have reused names sometimes.)

*Chose hurricane MARIA for analysis & filtering for id AL14 for study*

    hurrs %>% filter(name=="MARIA") %>% filter(str_detect(id,"AL14")) -> maria

#### b. Write a function called Hurdat2\_summary that takes the hurrs dataframe as an input and returns a named list containing

• the total number of storms • the range of years covered • a vector
containing the number of track points for each storm

    Hurdat2_summary <- function(df){
      #count the total number of storms
      total_storms<- summarise(df,count=n())
      
      #range of years covered
      range<-summarise(df,from=min(year),To=max(year))
      
      #a vector containing the number of track points for each storm
      track_points<- group_by(df,name) %>% summarise(total_track_pt=n()) 
      
      
      track_points <- setNames(as.numeric(track_points$total_track_pt),as.character(track_points$name))
      list("Total no. of Storms"=total_storms,"Range of years covered"=range,"Track Points of each storm"=track_points)
    }

\*Test your function by running it for three cases: • the full hurrs
dataframe • a version that has been filtered to contain only storms from
a single year • the one-hurricane dataframe you made in (a)

-   Test 1

        Hurdat2_summary(hurrs)

        ## $`Total no. of Storms`
        ## # A tibble: 1 × 1
        ##   count
        ##   <int>
        ## 1 52717
        ## 
        ## $`Range of years covered`
        ## # A tibble: 1 × 2
        ##    from    To
        ##   <dbl> <dbl>
        ## 1  1851  2020
        ## 
        ## $`Track Points of each storm`
        ##       ABBY       ABLE      AGNES    ALBERTO       ALEX       ALFA      ALICE     ALICIA 
        ##         99        153         35        233        125         11        117         25 
        ##      ALLEN    ALLISON       ALMA      ALPHA     AMELIA        AMY        ANA     ANDREA 
        ##         46        121        132         55          6         31        172         52 
        ##     ANDREW      ANITA       ANNA     ARLENE     ARTHUR     AUDREY       BABE      BAKER 
        ##         68         20        146        322        172         20         24        115 
        ##    BARBARA      BARRY      BECKY      BELLE     BERTHA      BERYL       BESS       BETA 
        ##         40        165        113         18        275        181         13         52 
        ##       BETH      BETSY      BETTY     BEULAH       BILL    BLANCHE        BOB     BONNIE 
        ##         29        182         42        165         99         30        105        273 
        ##     BRENDA       BRET    CAMILLE    CANDICE      CANDY      CARLA     CARMEN      CAROL 
        ##        124        139         37         25         15         98         49        152 
        ##   CAROLINE     CARRIE      CELIA      CESAR    CHANTAL    CHARLEY    CHARLIE      CHLOE 
        ##         33        124         99         61        171        163        153         97 
        ##      CHRIS  CHRISTINE      CINDY      CLARA  CLAUDETTE       CLEO      COLIN     CONNIE 
        ##        198         42        215         27        214        153         40         49 
        ##       CORA  CRISTOBAL      DAISY   DANIELLE      DANNY      DAVID       DAWN       DEAN 
        ##         19        115         71        214        190         55         39        148 
        ##     DEBBIE      DEBBY      DEBRA      DELIA      DELTA     DENNIS      DIANA      DIANE 
        ##        134        170         59         23         98        255         58         67 
        ##        DOG      DOLLY        DON      DONNA       DORA      DORIA     DORIAN      DORIS 
        ##        138        201         21         72         87         91        121         29 
        ##    DOROTHY     DOTTIE       EARL       EASY      EDITH       EDNA    EDOUARD      EIGHT 
        ##         77         15        211        115        166        100        228         32 
        ##     ELAINE      ELENA     ELEVEN       ELLA      ELLEN     ELOISE      EMILY       EMMY 
        ##         38         79          5        153         37         47        236         62 
        ##    EPSILON      ERIKA       ERIN    ERNESTO     ESTHER        ETA      ETHEL        EVE 
        ##         84        124        184        152         85         58         87         12 
        ##     EVELYN     FABIAN      FAITH        FAY       FAYE     FELICE      FELIX       FERN 
        ##         10         97         69        124         46         23        206         52 
        ##    FERNAND       FIFI    FIFTEEN      FIONA       FIVE      FLORA   FLORENCE    FLOSSIE 
        ##         16         63         39         55         32        127        330         50 
        ##     FLOSSY      FLOYD       FOUR        FOX       FRAN  FRANCELIA    FRANCES   FRANKLIN 
        ##         52        131          9        109        126         27        319         71 
        ##       FRED   FREDERIC     FRIEDA  GABRIELLE       GAIL      GAMMA     GASTON     GEORGE 
        ##         79         68         39        196         41         54        111         53 
        ##    GEORGES      GERDA       GERT   GERTRUDE    GILBERT      GILDA     GINGER      GINNY 
        ##        103         76        171         27         49         80        132         52 
        ##     GLADYS     GLORIA    GONZALO     GORDON      GRACE     GRACIE      GRETA     GUSTAV 
        ##        142        144         62        196         98         50        117        161 
        ##     HALLIE      HANNA     HANNAH     HARVEY     HATTIE      HAZEL      HEIDI     HELENA 
        ##         21        121         46        198         26         96         71         23 
        ##     HELENE      HENRI    HERMINE      HILDA      HOLLY       HOPE   HORTENSE        HOW 
        ##        282        112        124         78         53         39        119         68 
        ##       HUGO   HUMBERTO        IAN        IDA       IGOR        IKE       ILSA     IMELDA 
        ##         64        158         21         82         61         62         26          9 
        ##       INEZ       INGA     INGRID       IONE       IOTA      IRENE       IRIS       IRMA 
        ##         81        119         48         69         26        231        109         80 
        ##      ISAAC     ISABEL     ISAIAS     ISBELL    ISIDORE       ITEM       IVAN      JANET 
        ##        170         94         36         34        168         39        175         40 
        ##     JANICE     JEANNE      JENNY      JERRY        JIG       JOAN    JOAQUIN       JOSE 
        ##         54        158         60        138         47         53         76        153 
        ##  JOSEPHINE      JOYCE       JUAN     JUDITH      JULIA     JULIET       KARA      KAREN 
        ##        231         79         51         49         84         16         48        132 
        ##       KARL       KATE      KATIA      KATIE    KATRINA      KEITH     KENDRA       KING 
        ##        163        112         82         20         70         74         24         31 
        ##       KIRK      KLAUS     KRISTY       KYLE      LARRY      LAURA     LAURIE        LEE 
        ##         50         58         16        127         42        115         42        111 
        ##      LENNY     LESLIE       LILI       LISA       LOIS    LORENZO       LOVE       LUIS 
        ##         39        175        204        144         39         99         21         61 
        ##      MARCO      MARIA    MARILYN     MARTHA    MATTHEW    MELISSA    MICHAEL   MICHELLE 
        ##        100        161         79         17         79         82        102         35 
        ##       MIKE      MINDY      MITCH     NADINE       NANA       NATE     NESTOR   NICHOLAS 
        ##         14         14         78        127         50         84         15         80 
        ##     NICOLE       NINE   NINETEEN       NOEL     ODETTE       OLGA       OMAR        ONE 
        ##        118         19          9        113         23         82         59         15 
        ##       OPAL    OPHELIA      OSCAR       OTTO      PABLO     PALOMA      PATTY      PAULA 
        ##         39        175         49        124         38         36         11         21 
        ##   PAULETTE      PETER   PHILIPPE     RAFAEL    REBEKAH       RENE    RICHARD       RINA 
        ##         88         15        100         56         23         32         29         52 
        ##       RITA    ROXANNE      SALLY      SANDY       SEAN  SEBASTIEN      SHARY    SIXTEEN 
        ##         36         56         28         45         28         53          9          8 
        ##       STAN      TAMMY      TANYA      TEDDY        TEN      THETA      THREE      TOMAS 
        ##         17          9         29         49         48         33          5         54 
        ##       TONY TWENTY-TWO        TWO    UNNAMED      VICKY      VINCE    WILFRED      WILMA 
        ##         20         27         30      26792         24         15         17         48 
        ##       ZETA 
        ##         61

-   Test 2

        Hurdat2_summary(filter(hurrs,year==2005))

        ## $`Total no. of Storms`
        ## # A tibble: 1 × 1
        ##   count
        ##   <int>
        ## 1   907
        ## 
        ## $`Range of years covered`
        ## # A tibble: 1 × 2
        ##    from    To
        ##   <dbl> <dbl>
        ## 1  2005  2005
        ## 
        ## $`Track Points of each storm`
        ##      ALPHA     ARLENE       BETA       BRET      CINDY      DELTA     DENNIS      EMILY 
        ##         11         26         18          7         34         42         59         45 
        ##    EPSILON   FRANKLIN      GAMMA       GERT     HARVEY      IRENE       JOSE    KATRINA 
        ##         43         38         33          9         46         56          6         34 
        ##        LEE      MARIA       NATE   NINETEEN    OPHELIA   PHILIPPE       RITA       STAN 
        ##         26         51         29          9         69         28         36         17 
        ##      TAMMY        TEN TWENTY-TWO    UNNAMED      VINCE      WILMA       ZETA 
        ##          9         21         27          7         15         48          8

-   Test 3

        Hurdat2_summary(maria)

        ## $`Total no. of Storms`
        ## # A tibble: 1 × 1
        ##   count
        ##   <int>
        ## 1    93
        ## 
        ## $`Range of years covered`
        ## # A tibble: 1 × 2
        ##    from    To
        ##   <dbl> <dbl>
        ## 1  2005  2011
        ## 
        ## $`Track Points of each storm`
        ## MARIA 
        ##    93

#### c. Make a histogram of the number of track points, using the output of your function for the hurrs case.

     #converted list to a data frame 
    histplot<-as.data.frame(Hurdat2_summary(hurrs)) 
    # Taking rownames into name column
    histplot<-rownames_to_column(histplot,"name") 
    # filtering out unnamed storms and plotting histogram
    histplot %>% filter(name!="UNNAMED") %>%     
                 ggplot() +                      
                 geom_histogram(aes(x=Track.Points.of.each.storm),alpha=0.5,color="white", fill="red") + 
                 labs(title = "No. of Track points of storm all storms", x= "Track points")

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](Rcode_files/figure-markdown_strict/unnamed-chunk-7-1.png)

#### d. What year did they begin naming hurricanes and other storms?

    #Arranging as per year
    hurrs %>% arrange(year) %>% 
    #filtering out unnamed storms 
    filter(name!="UNNAMED") %>%
    #selecting first named entry and converting to numeric vector
    head(1) %>% select(year) %>% as.numeric() -> year_name
    #printing result
    print(paste("They began naming hurricanes and other storms in year ",year_name))

    ## [1] "They began naming hurricanes and other storms in year  1950"

# Question 2: Mapping

#### a. plot all the storms in the database atop a simple world map.

    #install.packages("maps") #Installing "maps" Package

    #defining a function to plot world map using 'ggplot2' and 'map' libraries
    hurricaneBasemap = function() {
      
      #converting 'world' map from 'maps' package into dataframe 
      world = map_data("world")  
      
      basemap = ggplot() +
        #from 'hurrs' dataset, 
        #min(longitude)=-110,max(longitude)= 63
        #min(latitude)=7.2, max(latitude)= 81
        # So, fixing coordinates considering some offset value
        coord_fixed(xlim=c(-130,30),ylim=c(0,90)) + 
        
        #plotting world map using data from 'world' dataframe
        geom_polygon(data=world, aes(x=long,y=lat,group=group), color="gray50", fill="white")
      return(basemap) 
    }
    hurricaneBasemap() +
      
      #connecting observations from each hurricane id
      geom_path(data=hurrs, aes(x=longitude,y=latitude,group=id,color=windspeed)) +
      
      #Giving sequential coloring to observations as per wind speed. sequence followed: Yellow to Red
      scale_color_distiller(type="seq",direction=1,palette="YlOrRd")

![](Rcode_files/figure-markdown_strict/unnamed-chunk-9-1.png)

#### b. write a function that takes a year as input and makes a similar plot for only the hurricanes during that year. The function doesn’t need to return a value, but it does need to generate a plot. Test your function by making the plot for a year of your choice.

function to plot hurricanes during specific year

    hurricane_in_year<- function(in_year){ 
      
      #filtering hurrs data as per year provided in argument
      hurrs_year <- hurrs %>% filter(year==in_year)
      
      #Plotting the hurricane data using function and plots used in question 2a
      hurricaneBasemap() +
        geom_path(data=hurrs_year, aes(x=longitude,y=latitude,group=id,color=windspeed)) +
        scale_color_distiller(type="seq",direction=1,palette="YlOrRd") +
        labs(title = paste("Hurricanes in year",in_year),x="Longitude",y="Latitude")
    }

plotting hurricanes in year 1990

    hurricane_in_year(1990)

![](Rcode_files/figure-markdown_strict/unnamed-chunk-11-1.png)

# Question 3: The local perspective

## a. Using the formula given in project introduction, write a function that calculates distance between (lon1,lat1) and (lon2,lat2). Test it.

loading word.cities database containing coordinates of cities from
’maps’package

    cities = world.cities 

defining a function to calculate distance from latitude and longitude
coordinates

    distance_between <- function(lon1, lat1, lon2, lat2) { 
      dx = (lon2 - lon1) * cos(pi * (lat1 + lat2) / 360)
      dy = lat2 - lat1
      
      D = 111.325 * sqrt(dx ^ 2 + dy ^ 2)
      return(D)
    }

Distance between Leeds and Cardiff

    distance_between(-1.55, 53.81, -3.18, 51.48) 

    ## [1] 281.7871

*Distance between Leeds and Cardiff is* 281.7871166 km

#### b. Adapt your function so that instead of (lon2,lat2) it takes the name of a city as input, finds its latitude– longitude coordinates, and returns the distance to (lat1,lon1). Test it. Give this function a new name and have it call the function from part (a).

    #defining function to calculate distance of city from coordinates 
    city_distance <- function(lon1, lat1, city_name) {
      lon2 <-cities %>% filter(name == city_name) %>% select(long) %>% as.numeric()
      lat2 <-cities %>% filter(name == city_name) %>% select(lat)  %>% as.numeric()
      distance_between(lon1, lat1, lon2, lat2)
    }

Calling the function

    #Distance between Leeds and Lisburn
    city_distance(-1.55, 53.81, "Lisburn")

    ## [1] 304.3833

Distance between Leeds and Lisburn 304.3833107 km

#### c. A common mistake in working with latitudes and longitudes is to swap them around. It isn’t always possible to catch user errors of this sort, but one test is that longitude can take values from -180 to 180 while latitude is always -90 to 90. Modify your function to check whether it’s likely that the user has swapped lat1 and lon1 and if so, provide a useful warning message.

defining function to check if entered coordinates are in correct range

    coord_check <- function(lon1, lat1, city_name) {
      if (-180 > lon1 | lon1 > 180){
        warning("Enter correct Longitude!")
      }
      else if(-90 > lat1 | lat1 > 90) {
        warning("Enter correct Latitude!")
      }
      else {
        lon2 <- cities %>% filter(name == city_name) %>% select(long) %>% as.numeric()
        lat2 <- cities %>% filter(name == city_name) %>% select(lat)  %>% as.numeric()
        distance_between(lon1, lat1, lon2, lat2)
      }
    }

calling the function

    #checking with wrong latitude
    #It should show warning:"Enter correct Latitude!"
    coord_check( -3.18, 92, "Lisburn")

    ## Warning in coord_check(-3.18, 92, "Lisburn"): Enter correct Latitude!

#### d. Choose an eastern North American or Caribbean city, and use your function to determine which storms passed within 100 km of it. Plot those storms on a map like those in Q1, with a dot on top showing where the city is.

function to find storms passed within 100km radius of a city

    near_city_100km<- function(city_name){
      city_coord<- filter(cities, name == city_name) #filtering city name
      #finding city coordinates
      lon1<- city_coord$long 
      lat1<- city_coord$lat 
      #adding a distance column containing distance of city from storm
      distance_100<- hurrs %>% mutate(distance = distance_between(lon1,lat1,longitude,latitude)) %>% 
        filter(distance<100) #filtering storms within 100km radius
      return( distance_100) # returning same filtered table 
    }

calling the function with city name New York

    near_city_100km("New York")

    ## # A tibble: 38 × 12
    ##    id       name     year month   day status latitude longitude windspeed isLan…¹ isHur…² dista…³
    ##    <chr>    <chr>   <dbl> <dbl> <dbl> <fct>     <dbl>     <dbl>     <dbl> <lgl>   <lgl>     <dbl>
    ##  1 AL071866 UNNAMED  1866    10 30.5  EX         41.3     -74          50 FALSE   FALSE     70.3 
    ##  2 AL051872 UNNAMED  1872    10 26.8  TS         40.6     -73.8        40 FALSE   FALSE     14.2 
    ##  3 AL061874 UNNAMED  1874     9 30    TS         40.9     -73.6        60 FALSE   FALSE     38.4 
    ##  4 AL041882 UNNAMED  1882     9 24.2  TS         40.7     -72.8        40 TRUE    FALSE     96.3 
    ##  5 AL051888 UNNAMED  1888     9 12    EX         40.6     -73.3        35 FALSE   FALSE     54.6 
    ##  6 AL041893 UNNAMED  1893     8 24.5  HU         40.7     -73.9        75 TRUE    TRUE       4.75
    ##  7 AL051894 UNNAMED  1894    10 10.6  HU         40.7     -72.9        75 TRUE    TRUE      87.9 
    ##  8 AL061900 UNNAMED  1900    10 14.5  EX         40.3     -73.7        35 FALSE   FALSE     45.9 
    ##  9 AL041903 UNNAMED  1903     9 16.8  TS         40.3     -75          55 FALSE   FALSE     98.8 
    ## 10 AL011915 UNNAMED  1915     8  4.75 EX         40.9     -74.3        50 FALSE   FALSE     39.7 
    ## # … with 28 more rows, and abbreviated variable names ¹​isLandfall, ²​isHurricane, ³​distance
    ## # ℹ Use `print(n = ...)` to see more rows

Plotting on map hurricanes within 100km radius of New York

    #function to return basemap to plot hurricanes within 100 km of city
    hurricaneBasemap1 = function() {
      library(maps) #calling 'maps' library to draw geographical maps
      
      #converting 'world' map from 'maps' package into dataframe 
      world = map_data("world")  
      
      basemap = ggplot() +
        coord_fixed(xlim=c(-80,-70),ylim=c(35,50)) + 
        geom_polygon(data=world, aes(x=long,y=lat,group=group), color="gray50", fill="white")
      return(basemap) 
    }

    # Plotting on map hurricanes within 100km radius of New York 
    hurricaneBasemap1() +
      
      #connecting observations from each hurricane id
      geom_path(data= near_city_100km("New York"), aes(x=longitude,y=latitude,group=id,color=windspeed),size=1) + 
      
      #Giving sequential coloring to observations as per wind speed. sequence followed: Yellow to Red
      scale_color_distiller(type="seq",direction=1,palette="YlOrRd") + labs(title = "Storms within 100km radius of New York",x="Longitude",y="Latitude") +
      # Green dot showing location of city  
      geom_point(data= filter(cities,name=="New York"),aes(x=long,y=lat),color="green",size=2)

![](Rcode_files/figure-markdown_strict/unnamed-chunk-21-1.png)

# Question 4: Climate change

#### a. Globally, the number of hurricane-strength storms has been clearly increasing because of climate change. Is this true in the Atlantic specifically? Plot the number of storms in each year.

Plotting all Hurricane-strength storms count over the years

    #counting total storms, hurricanes and storms which are not hurricanes from dataset.
    all_storms<-hurrs %>% group_by(year) %>% summarise("all_storms"=n())
    hurrs_true<-hurrs %>% group_by(year) %>% filter(isHurricane==TRUE) %>% summarise(hurricane=n())
    hurrs_false<-hurrs %>% group_by(year) %>% filter(isHurricane==FALSE) %>% summarise(not_hurricane=n())

    #Joing all counts in single dataset 'hurrs all'
    hurrs_all <-left_join(hurrs_true,hurrs_false) 

    ## Joining, by = "year"

    hurrs_all <- hurrs_all %>% left_join(all_storms) 

    ## Joining, by = "year"

    #using gather function to combine all storm count columns to single column
    df<-hurrs_all %>% gather(key = "storm_type", value = "storm_count",-year)

    #Plotting all Hurricane-strength storms count over the years
    ggplot() + 
      geom_smooth(data=hurrs_true,aes(x=year,y=hurricane), se=F,color="red",method = 'loess') +
      geom_line(data=hurrs_true,aes(x=year,y=hurricane),color="black") +
      labs(title = "Hurricane-strength storms count over the years", y="Hurricane Count")

    ## `geom_smooth()` using formula 'y ~ x'

![](Rcode_files/figure-markdown_strict/unnamed-chunk-22-1.png)

***The plot shows that, the number of hurricane strength storms is
increasing***

plotting count of Hurricane, not Hurricane and All storms counts

    ggplot(df, aes(x = year, y = storm_count)) + 
      geom_smooth(aes(color = storm_type, linetype = storm_type,),method = 'loess',se=FALSE) + 
      scale_color_manual(values = c("darkred", "steelblue","black")) +
      labs(title = "All types storms over the years", y="Storm Count")

    ## `geom_smooth()` using formula 'y ~ x'

![](Rcode_files/figure-markdown_strict/unnamed-chunk-23-1.png)

***From the plot it is observed that, total number of storms is
continuously increasing over the years along with non-hurricane storms.
Relatively, There is no significant increase in Hurricane-strength
storms***

#### b. The total number isn’t the only aspect of hurricane statistics that might change with climate change. What about the geographical area affected? Make a plot that lets you visually evaluate whether the latitude range where storms cross the coastline (make landfall) is expanding in recent decades. Does your plot suggest any concerns about data quality and data coverage in this database?

Defining a function to plot hurricane landfall from 1851 till specific
year

    #creating basemap to plot on world map
    hurricaneBasemap1 = function() {
      library(maps) 
      world = map_data("world")  
      basemap = ggplot() + coord_fixed(xlim=c(-120,-20),ylim=c(0,60)) +
        geom_polygon(data=world, aes(x=long,y=lat,group=group), color="gray50", fill="white")
      return(basemap) 
    }
    # Defining a function to plot hurricane landfall from 1851 till specific year
    landfall_till<- function(till_year){
      hurrs %>% filter(isLandfall==TRUE,year<=till_year) -> df2
      plot<-hurricaneBasemap1() +
        geom_point(data = df2, aes( x = longitude , y = latitude , group = id , color = year )) +
        scale_color_distiller( type = "seq" , direction = 1 , palette = "YlOrRd" ) + 
        labs( title = paste("Hurricane landfall from 1851 to ",till_year))
      return(plot)
    }

calling the function for making four different plots of hurrican
landfalls from 1851 till year 1920,1950,1990,2020 on world map

    library(ggpubr) # calling necessary library to combine plots
    ggarrange(landfall_till(1920),landfall_till(1950), landfall_till(1990), landfall_till(2020),
              labels = c("A", "B", "C","D"),
              ncol = 2, nrow = 2)

![](Rcode_files/figure-markdown_strict/unnamed-chunk-25-1.png)

**It is seen in the graph that over the years,storms making landfall are
expanding across eastern coastal regions of USA towards Mexico. Number
of landfalls in Caribbean counties is also significantly increased over
the years.**

**Data Quality: It looks quiet strange that hardly any hurricane hit any
Caribbean country or Mexico from year 1851 to 1920. This raises a
question whether data of hurricane landfalls before 1920 is included in
this data set or not.**

#### c. Is peak wind speed increasing over time? Answer this using whatever kind of plot or table you think is most helpful.

Plotting Maximum wind speed over the years

    hurrs %>% group_by(year) %>% 
      summarise(max.wind=max(windspeed,na.rm = TRUE)) %>% 
      ggplot() +
    geom_smooth(aes(x=year,y=max.wind,color=year),color='blue',se=F) + geom_line(aes(x=year,y=max.wind),color='red')+
      labs(title = "Maximum wind speed over the years", x="years", y="max wind speed")

    ## `geom_smooth()` using method = 'loess' and formula 'y ~ x'

![](Rcode_files/figure-markdown_strict/unnamed-chunk-26-1.png)

***The plot shows that maximum wind speed of Hurricanes each year is
increasing over the years***

#### d. What about the seasonal timing of hurricanes? First, make a histogram of storms by month over the entire dataset. This should show that there is a distinct hurricane season—they tend to happen at a certain time of year. Use a bin width of 1 month.

Seasonal Timings of Hurricanes

    hurrs %>% group_by(month) %>% ggplot(aes(x=month)) + scale_x_discrete(limits=1:12) + geom_histogram(binwidth = 1,color="yellow",fill="blue") +theme_minimal() +labs(title = "No. of storms in each Month ")

![](Rcode_files/figure-markdown_strict/unnamed-chunk-27-1.png)

***This plot shows that 8th, 9th,10th month of the year is seasonal
timing of Hurricanes***

#### e. add a “decade” column to the hurrs dataframe.

adding “decade” column to the hurrs dataframe

    hurrs<- mutate(hurrs,decade=year-year%%10)

#### f. make a plot that shows whether “hurricane season” is expanding into earlier and later months of the year over time.

Is “hurricane season” is expanding into earlier and later months of the
year over time?

    hurrs %>% group_by(month) %>% ggplot() + geom_count(aes(x=year,y=month)) + labs(title = "Hurricane occurances in different months of each year")+ scale_y_discrete(limits=1:12)

![](Rcode_files/figure-markdown_strict/unnamed-chunk-29-1.png)

***The plot indicates that the hurricane season which usually observed
in 8th,9th and 10th month of the year is over the time expanding into
7th , 6th month and 5th month (earlier months of the year)***
