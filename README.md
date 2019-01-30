# idp_web_app
Shiny app for IDP project

## Summary
This repository contains the code for the prototype of the web application for the Indiana Data Partnership project. The goal of this project is to experiment with different forms of visualization to represent implicit relationships between organizations in the IBRC/Polis data set. The design of the project has been directed by Brad Fulton at SPEA and implemented by Matt Hutchinson at IUNI [maahutch@iu.edu](maahutch@iu.edu).

The application is currently hosted on a Virtual Machine on a server provided by IBRC. It should be accessible at [idp.iuni.iu.edu](idp.iuni.iu.edu). Data for the visualizations comes from a graph database on a second VM also hosted by IBRC.

The application consists of three main components: app.r, savi_fun.r & db_queries.py.

### app.r
The script describes the general appearance and structure of the app as well as controlling the reactivity. The code follows the standard structure of a shiny app and is subsequently divided into two sections: ####ui and ####server.

    ####ui: 
