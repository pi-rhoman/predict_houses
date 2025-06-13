Fortran House Price Predictor
=============================
This is a personal project developed to learn Fortran programming. It has no use realworld use case. TThis is a program that uses linear regression to predict the price of a house based on it's floor area and the number of bedrooms and bathrooms it has. It demonstrates use of modules, libraries and basic linear algebra. 

## Features
- Reads data from a CSV and stores it in a multidimensonal array
- Uses OLS to learn a linear model based on the data
- Uses this model to predict the price of a house given a set of features

## Prerequisites
- GNU Fortran (gfortran) installed
- LAPAK 3.12.1 

## Building the program
1. Clone this repository
2. `cd` into the directory containing the source code
3. Compile the program using the command

```bash
gfortran util.f90 predict_houses.f90 -o predict_houses -L <path to lapack> -llapack -lrefblas -ltmglib
```

## Running the Program
To run the program provide the path to the CSV file as a command line argument

```bash
./predict_houses example.csv
```

## Limitations
- The program assumes a simple CSV format with no null fields
- Error handling for malformed files is minimal

## Acknowledgements
The example dataset for this project was taken from the [lecture notes of Derek Bridge](https://github.com/derekgbridge/artificial_intelligence_2024_2025/blob/main/ai1/datasets/dataset_corkA.csv)
