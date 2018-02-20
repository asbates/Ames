
### About

This is a project I did for the Kaggle competition [House Prices: Advance Regression Techniques](https://www.kaggle.com/c/house-prices-advanced-regression-techniques). I had previously participated in this competition as a group final project for a statistics course at UC San Diego. I didn't do a lot of the coding the first time and there were a few things I wanted to do differently so I reworked everything myself.

### The data

The data set contains information on home sales in Ames, Iowa from 2006 to 2010. It was compiled by [Dean De Cock](https://ww2.amstat.org/publications/jse/v19n3/decock.pdf) as an alternative to the famous Boston Housing Data Set. The goal of the competition is to predict the price of a home given various information about it.

### Analysis

The bulk of the work was in cleaning the data. In particular, there were a lot of missing values and these needed to be taken care of to facilitate modeling. I tried three models: elastic net, random forest, and boosted regression trees. The boosted trees gave the best results but I was unable to improve on the scores from the previous analysis. There were a few issues that came up and a few more things I would like to try but that will have to wait until I have some more free time.
