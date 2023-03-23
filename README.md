[![Documentation Status](https://readthedocs.org/projects/atlasapprox/badge/?version=latest)](https://apidocs.atlasapprox.org/en/latest/?badge=latest)

Cell Atlas Approximations - API
===============================
Cell atlases such as Tabula Muris and Tabula Sapiens are multi-organ single cell omics data sets describing entire organisms. A cell atlas approximation is a lossy and lightweight compression of a cell atlas that can be streamed via the internet.

This project enables biologists, doctors, and data scientist to quickly find answers for questions such as:

- *What is the expression of a specific gene in human lung?*
- *What are the marker genes of a specific cell type in mouse pancreas*?
- *What fraction of cells (of a specific type) express a gene of interest?*

These questions can be asked in Python or R using the provided packages (see below), or in a language agnostic manner using the REST API.

Version
-------
The latest API version is `v1`.

We support several organs and organisms: human, mouse, lemur (a type of monkey), zebrafish, C. elegans. More organisms and organs are planned for the near future.

Documentation
-------------
Documentation for the API is available at: https://atlasapprox.readthedocs.io

Usage (REST)
------------
The RESTful API can be queried using any HTTP request handler, e.g. Python's `requests`:
```python

import requests

# Get a list of human organs covered by the API
requests.get(
    'http://api.atlasapprox.org/v1/organs',
    organism='h_sapiens',
)
```

Usage (Python)
--------------
The Python API is currently undocumented. If you are impatient:

```python
import atlasapprox

api = atlasapprox.API()
print(api.organisms())
print(api.celltypes(organism="c_elegans", organ="whole"))
```


Usage (R)
--------------
The R API is currently in pre-alpha and undocumented.


Documentation
-------------
Documentation of the RESTful API and its Python interface is available at https://apidocs.atlasapprox.org.


Repo contents
-------------
- `data`: files required for the compression of current cell atlases
- `preprocess`: scripts used for the approximations
- `web`: webserver code in Flask that implements the RESTful API
- `Python`: package code providing a Python interface for the RESTful API
- `R`: package code providing an R interface for the RESTful API
- `docs`: user documentation for the API
