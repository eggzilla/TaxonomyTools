![TaxonomyTools](http://www.tbi.univie.ac.at/~egg/TaxonomyTools.png "TaxonomyTools")

[![GitHub](https://img.shields.io/github/tag/eggzilla/TaxonomyTools.svg)](https://github.com/eggzilla/TaxonomyTools) [![Build Status](https://travis-ci.org/eggzilla/TaxonomyTools.svg)](https://travis-ci.org/eggzilla/TaxonomyTools) [![Hackage](https://img.shields.io/hackage/v/TaxonomyTools.svg)](https://hackage.haskell.org/package/TaxonomyTools) [![Bioconda](https://anaconda.org/bioconda/taxonomytools/badges/version.svg)](https://anaconda.org/bioconda/taxonomytools) [![Docker Repository on Quay](https://quay.io/repository/biocontainers/taxonomytools/status "Docker Repository on Quay")](https://quay.io/repository/repository/biocontainers/taxonomytools)
=============

Taxonomy Tool utilizes functions from the Taxonomy library to provide
several commandline utilities for routine-tasks with taxonomy data.
Currently some tools accept either the [NCBI taxonomy dump]() as input,
while other retrieve the data directly from the [NCBI Entrez REST]() interface.
It is planned to support both datasources with each tool.

TaxonomyTools can be install via the Haskell package management system:

    cabal install TaxonomyTools

Currently following Tools are included and linked with their usage instructions:

###[Accessions2TaxIds](accessions2taxids.md)
Converts a list of Refseq accession numbers into NCBI taxonomy ids
###[TaxIds2Tree](taxids2tree.md)
List of taxonomy ids is converted into a graphical tree representation
either as .svg (via graphviz) or as .json (via d3js)
###[TaxIds2TreeCompare](taxids2treecompare.md)
Multiple lists of taxonomy ids are converted into a visualisation of the
taxonomic tree highlighting the input nodes corresponding to their list. 
###[TaxIds2Text](taxids2text.md)
List of taxonomy ids is converted in a short text summary for each node.
###[TreeDistance](taxis2distance.md)
Computes the distance between two input nodes on the given tree.

