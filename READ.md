README.txt 

Google Map API
In this dashboard, we provide a search function that is connected to Google Map API. First of all, users can insert the content they want to look for (e.g., restaurant, café, bar etc.), then input the search radius and select the number of returned results. When users click the ‘Generate Data’ button and point/marker on the maps, the related result table will be displayed on the dashboard which includes the information in detail such as name, price, rating and so on.

Interact With Tableau in R
For the bike share dock locations map, we embedded the tableau treemap chart into R shiny dashboard and built a connection between them. After users choosing one of the suburbs in treemap, the corresponding bike share dock locations of that area will be shown on the map in R.





Design Summary:
•	what your interface does
•	how it works
•	what features of the design you would like to receive credit for
•	a justification of the design decisions made (e.g., why a particular colour scheme is used, or why a particular graph is the best visualisation of the underlying data)
Transportation 

There are three sections which are car parks, bike and train that show different vehicles in Melbourne for tourists to select according to their requirements. For car parks and bikes, there are two charts were plotted for each topic. We separated the on-street and off-street car parking so that tourists could pick the suitable one for the different availability and cost. In addition, we migrated the tableau treemap chart to the R shiny dashboard and built a connection between them which allows users can interact with the dashboard. Here are the detailed features which are well-designed:

Real-time On-street Parking Data
Since the on-street parking status data is real-time on the platform (https://data.melbourne.vic.gov.au/pages/home/), it provides the API for getting the latest data. We implemented a real-time map to show tourists the location of available parking spots which helps them reduce the time that may be spent on looking for parking places. In addition, we distinguish the marker colours to represent different meanings: red for occupied and green for available which assists the reader to understand the map clearly. We also considered that it is not reader-friendly if we generate all the parking points together, the better way is to change to the point cluster map which readers can click the cluster for the expanded details if they want to explore a specific area.


Return Current Day Information (Operation Time)
For the service information in Melbourne, it provides various detailed information such as cost, route, operation time and so on. In particular, the dataset included the operation time on different days, we designed that the label will return the operation time of the current day by considering most tourists may check the availability of the related services before they visit. Moreover, we assigned different icons for each category of service so that the readers can filter and understand the map more easily.


Interaction Between Tableau and R
In the bike share dock location map, the original data set shows the full detailed address of each spot. We split the address into suburbs and streets, then created a treemap for the suburb perspective in tableau and the street perspective marker map in R shiny. After that, we migrated the tableau to R and built a connection between them. When users click the suburb name on treemap, the filter will be applied to the map which displays the detailed location of the dock locations in that area. This interaction can enhance user experience and let them explore the dataset by themselves according to their interest.


Suitable for Travellers with Disabilities
For the train map in the dashboard, we provide the details of the stations with accessibility services which provide a better travel experience and wider customer base. By catering to travellers with diverse needs and abilities, the tourists in need can filter the stations with accessibility services to travel for their convenience. Also, we displayed the station in different colours: green and red mean with and without accessibility service respectively.



Summary:

•	some of the interesting or useful patterns or information that the interface helps you discover (standing in the shoes of the target user group(s)), and
•	a short rationale as to why your tool helped in those discoveries or use cases.

Google Map API

After implementing the car park and service section, we imagine the tourists may want to visit some landmarks or restaurants during their journeys. However, it is a heavy workload to collect and merge data for the locally famous sightseeing points. Therefore, we ended up connecting to the Google Maps API and allowed users themselves to search what places they want to visit and return related details for them to select.


Real-time On-street Parking Data API

In practice, drivers always want to find a suitable parking spot when they travel by car. According to this scenario, we implement a real-time dashboard for users to get the latest status of parking so that they can enjoy their journey and enhance their travel experience in Melbourne.

Colours and Icon on Maps

Since the transportation section may be more related to the map visualization, most of the colours and icons on those maps are considered carefully to help users gain a better insight into the charts. For example, we used different transportation icons to represent corresponding transportation methods and various colours meaning their status.


















Group Member Contribution Table:
Name / Contribution to project(max 50 words) / Percentage Contribution
Shiqi Liang

Transportation Topic: created six charts (one bar chart one treemap in Tableau and four maps in R shiny), connected with the Google Map API and got the latest data from the platform. Displayed three transportation methods (car/bike/train) and showed the detailed information for different locations (cost/operation time of current day).

25%
