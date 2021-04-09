# R.Project
School project in R Programming (Shiny App)

This app works as a simple Student's loan calculator with sensitive interaction. This means that the idea is to work without a "Submit" button and the data to be automatically updated upon changing the features that the user inputs in the sidebar panel.

The main focus is on calculating loans with small numbers, but this is chosen just for the sake of simplicity. The code is easily adjustable to have longer years and all other features that the user gives.

I have put some basic CSS code in the project, just to give it some better visual influence.

The code is compiled in a single file for easier management, but if needed, it can be split in different files containing UI, Server and even CSS.

In the server part of the code, I have used some blocks taken from a couple of Loan Calculator "Shiny Apps" availabe in the web. I have analyzed each row of them and given deep explanation in the comments near each line. 


# How to use it
When you open the app, you should go to the sidebar panel in the left side and fill the details of your loan. The system will immediately calculate how much money you will need to pay in each month. Below that, you will be shown a table which gives the full specifics of each monthly payment you will make. Furthermore, there are the options to "Copy, Print and Download (some formats)" the whole dataset so you can have it for your later usage as well.
In the sidebar panel, there is a checkBox option if you will also like to see a graphical description of your yearly payments. Each bar is showed in 2 colors to tell the importance of interest and principal in each year of your loan. 

