# Code and data for [Bhadani et al. (2022)](https://doi.org/10.1038/s41562-021-01276-5)

This repository contains Jupyter notebooks that replicate the main results of the following paper:

    Bhadani, S., Yamaya, S., Flammini, A. et al. Political audience diversity
    and news reliability in algorithmic ranking. _Nat Hum Behav_ (2022).
    https://doi.org/10.1038/s41562-021-01276-5


The following BibTex code can be imported into a citation manager for reference:

```bibtex
@Article{bhadani2022political,
  author   = {Bhadani, Saumya and Yamaya, Shun and Flammini, Alessandro and Menczer, Filippo and Ciampaglia, Giovanni Luca and Nyhan, Brendan},
  journal  = {Nature Human Behaviour},
  title    = {Political audience diversity and news reliability in algorithmic ranking},
  year     = {2022},
  issn     = {2397-3374},
  abstract = {Newsfeed algorithms frequently amplify misinformation and other low-quality content. How can social media platforms more effectively promote reliable information? Existing approaches are difficult to scale and vulnerable to manipulation. In this paper, we propose using the political diversity of a website’s audience as a quality signal. Using news source reliability ratings from domain experts and web browsing data from a diverse sample of 6,890 US residents, we first show that websites with more extreme and less politically diverse audiences have lower journalistic standards. We then incorporate audience diversity into a standard collaborative filtering framework and show that our improved algorithm increases the trustworthiness of websites suggested to users--especially those who most frequently consume misinformation--while keeping recommendations relevant. These findings suggest that partisan audience diversity is a valuable signal of higher journalistic standards that should be incorporated into algorithmic ranking decisions.},
  doi      = {10.1038/s41562-021-01276-5},
  url      = {https://doi.org/10.1038/s41562-021-01276-5},
}
```

## Installation and Data

The notebook was written in R version 3.6.2 and Python 3. A number of
dependencies are to be installed before running it. A list of package
requirements is provided in a separate file. The simplest method is to run a
package manager such as **pip** or **conda**. It is recommended that all
dependencies are installed in a separate environment.

The best way to do so is to use the Miniconda, which is a small bootstrap
version of the largest Anaconda Python distribution (R is also available).
Miniconda is available for most major operative systems (Windows, Linux,
MacOS). Please note that installing R is not a prerequisite while using
Miniconda. It will automatically install R and its essential libraries in the
virtual environment through the `environment_infodiversity.yml`.

## Step-by-step installation instructions

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
    conda activate infodiversity_results
```

This should install all needed packages and activate the environment. 
And it will also download the data which is included in the `data/` folder.

## Execution

To open the notebooks, follow the instructions above for installing the Python
environment and for obtaining the dataset files. From the location
where you downloaded this repository launch Jupyter Notebook with:

```
    conda activate infodiversity_results
    jupyter notebook
```

This will open the notebook interface in your browser. It will show the
contents of this repository. Click on the entry called `Fig_1_to_3_R.ipynb` 
to reproduce Figures 1 to 3 (this notebook is written in R) and
`Fig_4_to_6_suppl_python.ipynb` to reproduce Figures 4 to 6 and all the
supplementary figures (this notebook is written in Python).
