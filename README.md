# idp_web_app
Shiny app for IDP project

## Summary
This repository contains the code for the prototype of the web application for the Indiana Data Partnership project. The goal of this project is to experiment with different forms of visualization to represent implicit relationships between organizations in the IBRC/Polis data set. The design of the project has been directed by Brad Fulton at SPEA and implemented by Matt Hutchinson at IUNI [maahutch@iu.edu](maahutch@iu.edu).

The application is currently hosted on a Virtual Machine on a server provided by IBRC. It should be accessible at [idp.iuni.iu.edu](idp.iuni.iu.edu). Data for the visualizations comes from a graph database on a second VM also hosted by IBRC.

The application consists of three main components: app.r, savi_fun.r & db_queries.py.

### app.r
The script describes the general appearance and structure of the app as well as controlling the reactivity. The code follows the [standard structure of a shiny app](https://shiny.rstudio.com/tutorial/written-tutorial/lesson2/) and is subsequently divided into two sections: ####ui and ####server.

#### ui:
The ui section controls the appearance and layout of the web page. This sections accepts the users input and passes them to the server section. It then receives the output of the functions from the server section and displays them on the web page.

#### server:
The server section accepts the inputs from the ui and calls the functions defined by savi_fun.r. Passes the output of the functions to the ui section.

### savi_fun.r
The name of this script is a legacy of the early days of the development process. Really, it should be called 'idp_fun.r'.
The file contains a library of functions for passing arguments to the python library, processing the results and generating the visualizations. The file is broadly divided into four sections relating to the four tabs in the final web application: 'IDP Home', 'Involved Organizations Map', 'Potential Collaborators Map' and 'Pathways Map'.

#### IDP Home
`home.map` -> IDP Home only contains a single function to generate the interactive map focused on Indiana. This function accepts not inputs and is run when the app is loaded.
