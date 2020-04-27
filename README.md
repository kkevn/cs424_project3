# cs424_project3

## Project Information

This project is the third project for CS 424 at the University of Illinois at Chicago during the Spring 2020 semester.

*Created by: Kevin Kowalski, Samuel Kajah, Vijay Vemu*

More information can be found at the following project link: < project page here >

---

## 1. Jupiter Notebook Code Instructions (Python) - Data Preparation:

1. Load the included `p3_python_notebook.ipynb` file into Jupyter Notebook using Anaconda with the default Python environment
2. Extract each of the downloaded files from their archives
3. Ensure that the `.list` files are in a `\data_files\old_files\` directory from the root of the notebook file
4. Install the necessary packages 
5. Select the top cell of the notebook file and click the Run button
6. Observe outputs from each cell and continue to click run for each cell

## 2. Jupiter Notebook Code Instructions (R) - Data Cleaning:

1. Load the included `p3_notebook.ipynb` file into Jupyter Notebook ([using Anaconda with an R environment](https://docs.anaconda.com/anaconda/navigator/tutorials/r-lang/))
2. Ensure that the `.csv` files output from the Python notebook are present in the `\data_files\new_files\` directory from the root of the notebook file
3. Install the necessary packages 
4. Select the top cell of the notebook file and click the Run button
5. Observe outputs from each cell and continue to click run for each cell

## 3. Jupiter Notebook Code Instructions (R) - Data Plotting:

1. Load the included `p3_notebook-combined.ipynb` file into Jupyter Notebook ([using Anaconda with an R environment](https://docs.anaconda.com/anaconda/navigator/tutorials/r-lang/))
2. Ensure that both the `combined-data.rds` and `keywords_subset.csv` files output from the other R notebook are present in the same directory as this notebook file
3. Install the necessary packages 
4. Select the top cell of the notebook file and click the Run button
5. Observe outputs from each cell and continue to click run for each cell

## 4. RStudio Code Instructions - Shiny Application:

1. Create a new project or load the included files found in `/p3_shiny`
2. Ensure that the `combined_data.rds` and `keywords_subset.csv` files are in the same directory as the project
3. Copy or load the code file `app.R` and click the Run button
4. If RStudio gives issues, install the necessary packages it may prompt you for
5. Otherwise, go to the RStudio package manager and install the packages by name as seen in the library list at the top of `app.R`
6. Click the Run button
7. If RStudio prompts you for a Shiny account, make and connect one using [this tutorial](https://shiny.rstudio.com/tutorial/)
8. The visualization should be up and running and ready to be used

---

For a clean copy of the base files, download the following files from: [ftp://ftp.fu-berlin.de/pub/misc/movies/database/frozendata/](ftp://ftp.fu-berlin.de/pub/misc/movies/database/frozendata/)
* release-dates.list
* running-times.list
* certificates.list
* genres.list
* keywords.list
* movies.list
* ratings.list
