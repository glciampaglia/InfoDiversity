# InfoDiversity

## 1. Installation and Data

The notebook was written in R version 3.6.2 and Python 3. A number of dependencies 
are to be installed before running it. A list of package requirements is provided. The
simplest method is to run a package manager such as pip or conda. It is
recommended that all dependencies are installed in a separate
environment.

The best way to do so is to use the Miniconda, which is a small bootstrap
version of the largest Anaconda Python distribution (R is also available). 
Miniconda is available for most major operative systems (Windows, Linux, MacOS).
Please note that installing R is not a prerequisite while using Miniconda. It 
will automatically install R and its essential libraries in the virtual environment
through the environment_infodiversity.yml.

1. Download Miniconda from here: https://conda.io/miniconda.html
2. Follow the installation instruction for your platform:
   https://https://conda.io/projects/conda/en/latest/user-guide/install/index.html
3. Clone or download this repository
```
git clone https://github.com/glciampaglia/InfoDiversity.git
```
4. Open a terminal and run:
```
    cd InfoDiversity
    conda env create --file environment_infodiversity.yml
    conda activate infordiversity_results
```

This should install all needed packages and activate the environment. 
And it will also download the data which is included in the data/ folder.

## 3. Execution

To open the notebooks, follow the instructions above for installing the Python
environment and for obtaining the dataset files. From the location
where you downloaded this repository launch Jupyter Notebook with:

```
    conda activate twitch-overload
    jupyter notebook
```

This will open the notebook interface in your browser. It will show the
contents of this repository. Click on the entry called _Fig_1_to_3_R.ipynb_ 
for Figures 1 to 3 (R) and _Fig_4_to_6_suppl_python.ipynb_ for Figures 4 to 6 
and supplementary figures (Python).