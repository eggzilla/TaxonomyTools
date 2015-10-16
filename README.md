TaxonomyTools   [![Build Status](https://travis-ci.org/eggzilla/TaxonomyTools.svg)](https://travis-ci.org/eggzilla/TaxonomyTools)
=============

Taxonomy Tool utilizes functions from the Taxonomy library to provide
several commandline utilities for routine-tasks with taxonomy data.
Currently some tools accept either the NCBI taxonomy dump as input,
while other retrieve the data directly from the NCBI Entrez REST interface.
It is planned to support both datasources with each tool.

Currently following Tools are included:

Accessions2TaxIds
Converts a list of NCBI accession numbers into NCBI taxonomy ids
Ids2Tree
List of taxonomy ids is converted into a graphical tree representation
either as .svg (via graphviz) or as .json (via d3js)
Ids2TreeCompare
Multiple lists of taxonomy ids are converted into a visualisation of the
taxonomic tree highlighting the input nodes corresponding to their list. 
TaxIds2Text
List of taxonomy ids is converted in a short text summary for each node.
TreeDistance
Computes the distance between two input nodes on the given tree.
