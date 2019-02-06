# idp_web_app
Shiny app for IDP project

## Summary
This repository contains the code for the prototype of the web application for the Indiana Data Partnership project. The goal of this project is to experiment with different forms of visualization to represent implicit relationships between organizations in the IBRC/Polis data set. The design of the project has been directed by Brad Fulton at SPEA and implemented by Matt Hutchinson at IUNI [maahutch@iu.edu](maahutch@iu.edu).

The application is currently hosted on a Virtual Machine on a server provided by IBRC. It should be accessible at [idp.iuni.iu.edu](idp.iuni.iu.edu). Data for the visualizations comes from a graph database on a second VM also hosted by IBRC.

The application consists of three main components: app.r, savi_fun.r & db_queries.py.

## app.r
The script describes the general appearance and structure of the app as well as controlling the reactivity. The code follows the [standard structure of a shiny app](https://shiny.rstudio.com/tutorial/written-tutorial/lesson2/) and is subsequently divided into two sections: ####ui and ####server.

#### ui:
The ui section controls the appearance and layout of the web page. This sections accepts the users input and passes them to the server section. It then receives the output of the functions from the server section and displays them on the web page.

#### server:
The server section accepts the inputs from the ui and calls the functions defined by savi_fun.r. Passes the output of the functions to the ui section.



## savi_fun.r
The name of this script is a legacy of the early days of the development process. Really, it should be called 'idp_fun.r'.
The file contains a library of functions for passing arguments to the python library, processing the results and generating the visualizations. The file is broadly divided into four sections relating to the four tabs in the final web application: 'IDP Home', 'Involved Organizations Map', 'Potential Collaborators Map' and 'Pathways Map'.

#### IDP Home
`home.map` <- IDP Home only contains a single function to generate the interactive map focused on Indiana. This function accepts not inputs and is run when the app is loaded.

#### Pathways Map
`getCat` <- This function retrieves the categories associated with a chosen level and displays them in the drop down menu 'Select Organization/Service Type'

`getStart` & `getNext` <- Retrieves the data for the map.

`makeMap` <- Generates the interactive map and from the output of the `getStart` & `getnext`. Maps the organization locations and draws the lines between the organizations.

`edge.org.count` <- Reads csv created by makeMap to display the count of organizations mapped.

`createOutput` <- Accepts the organization list as well as distance and number of steps from the sliders. Calls `getStep` and `makeMap` to generate the visualization.


`simpleCap` <- Called throughout. Accepts lowercase character string and outputs a character string with the first character capitalized.


#### Potential Collaborators Map

`all.oi` <- Generates map of all organizations involved in selected topic area.

`oi.org.count` <- Reads file created by `all.oi` and prints text beneath map with count of organizations.

`get.oi.org` <- Returns a list of organization names related to the subject area. Used as options in the dropdown menu.

`get.cat.combo` <- Accepts organization name and number of matching categories from the interface. Returns all possible combinations of categories for source organization.  

`get.source.org` <- gets location data for organization selected from the drop down menu.

`org.cat.match` <- accepts category combinations and source org and outputs the map or potential organizational collaborators.

`get.match.org.oi` <- Called by `org.cat.match`. Builds the query depending on number of matching categories requested by the user and returns data for visualization.

`collab.org.count` <- Reads csv created by org.cat.match and displays count of organizations being mapped.

#### Involved Organizations Maps

`make_net` <- Creates the initial network diagram. Accepts arguments as to the issue area (Opioids/Workforce Development), whether or not to show the unconnected nodes and what algorithm should be used to size the nodes. Also merges some of the nodes from the database to clean up the visualization. For example 'Marion County Sheriff Jail' I and 'Marion County Sheriff Jail II' are merged to for 'Marion County Sheriff Jail'. This is the function that applies the colors to the nodes.


## db_queries.py

This script handles the interaction with the database sending the queries/arguments from the frontend and returning the data for the visualization. The code is written in python 3 to take advantage of the neo4j driver or python. The RNeo4j library that allows connectivity from an R environment to Neo4j is no longer supported and relies on the http protocol. The python driver uses the faster bolt protocol and is actively maintained by Neo4j.
The naming of the functions in this script corresponds to their parent functions in the savi_fun.R but with the suffix '_py'. For example, `get_cat_py` in the db_queries.py script is called by `getCat` function in the savi_fun.R script.



## Additional Files

`purple.png`, `blue.png` etc. <- These files are used a map pins in both the 'Potential Collaborators Map' and 'Pathways Map'.

`data` <- The data directory contains various files that supplement the visualizations.

    - The in_ct and in_zip directories contain shape files for the zip code and census   tracts in Marion county. Early in the app development there was an idea that the map applications would visualize economic and demographic data under the mapping of the organizations and allowing users to switch between county, zip, census tract etc. level data. However, people have lost interest in this idea. The maps still include choropleth's at the zip code data is made up (op_zip_placeholder.csv) and includes no interactivity.

    - The remaining files are those written by the application and includes counts of organizations being mapped. The numbers in these files are included below the visualization.

`www` <- This directory contains a number of images used in the application. Includes the map and network diagram legends and the IUNI favicon that appears on the web browser.
