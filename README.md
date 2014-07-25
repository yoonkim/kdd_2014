kdd_2014
========
Let -path be the folder with all the data 

To run: 
1. python kdd_2014_data_model1.py -path
2. Run kdd_2014_model1.R (change folder <- path) 
3. download ESLI data from http://nces.ed.gov/ccd/elsi/tableGenerator.aspx 
(Public School, Years 2011-2012, columns school id and school type) 
4. Run kdd_2014_model2.R (change folder <- path) until line 126 
5. python kdd_2014_data_model2.py -path
6. Run kdd_2014_model2.R from line 126 until the end
7. Final prediction is (0.5*model1+0.5*model2)*discount
