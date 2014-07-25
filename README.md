kdd_2014
========
Let -path be the folder with all the data \n

To run: \n
1. python kdd_2014_data_model1.py -path \n
2. Run kdd_2014_model1.R (change folder <- path) \n
3. download ESLI data from http://nces.ed.gov/ccd/elsi/tableGenerator.aspx \n
(Public School, Years 2011-2012, columns school id and school type) \n
4. Run kdd_2014_model2.R (change folder <- path) until line 126 \n
5. python kdd_2014_data_model2.py -path \n
6. Final prediction is (0.5*model1+0.5*model2)*discount
