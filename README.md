# R scripts to deal with native extractions/reports from AO3/Sage Accounting System.
"Tidying up" native extraction (mainly the analytical G/L and the chart of accounts) from AO3/Sage Accounting System. 

This project presents a series of procedures in R that takes .xls files (with atrocious blank spaces, null columns, skipped rows, etc.) directly extracted from the system/ERP's front-end and consolidates them into a "tidyR" format to assemble the data into, say, a pivot or a dashboard presenting the income statement (or statement of P&L) and the balance sheet (statement of financial position).

## Requirements
Project runs in R programming language (version 4.0.3, 2020-10-10) under R Studio IDE (version 1.3.1073). R Studio IDE is stricly necessary due rstudioapi library.

## Excel files
The excel files are actual native extractions of the general ledger from 2016 to 0219 and the chart of accounts. 

A typical [front-end user would extract](https://sageead.com.br/ajudaonline/artigo.aspx?artigo=12325) such files like this:

![Tutorial Front-end](https://github.com/Kaleckian/R_in_AO3_Sage/blob/main/Figures_MD/Tutorial_SAGE.png?raw=true)

**Obviously, G/L and the chart of accounts were anonymised. (VBA_GL_and_Chart modules)**
