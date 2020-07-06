# statlearn
Repository to host basic notes and simple implementations of common methods 
related to Statistical Learning.

It will host implementations on both Python and R, but focusing on Python.
Expect the R code to be really disordered and not that user friendly.

## Setting up environment

To set up and environment with the python dependencies to run the Python
code, I recommend using/configuring `pipenv`. And once that tool is installed
you can run the following in the repository root directory:

```bash
pipenv install
pipenv shell
```

This will create a Python virtual environment with all the dependencies 
(beware that it may take a while to create it) and give you a shell inside that
virtual environment.

## Notebooks

There will be notebooks depicting some basic examples on
fundamentals of Statistical Learning elements.

* `about_flexibility.ipynb`: Shows the effect of sample size and flexibility in
a model/prediction.
