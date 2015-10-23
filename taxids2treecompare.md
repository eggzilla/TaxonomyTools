##TaxIds2TreeCompare
Multiple lists of taxonomy ids are converted into a
visualisation of the taxonomic tree highlighting the input nodes corresponding
to their list.

options [OPTIONS]

Common flags:
* -i --taxdumpdirectorypath=ITEM  Path to input NCBI taxonomy dump files directory
* -t --taxnodecsvfilepath=ITEM    Path to input taxonomy csv, each column with comma separated taxids represents one tree
* -l --levels=INT                 Number defining maximum distance from root for nodes in subtree.
* -o --outputdirectorypath=ITEM   Path to output directory
* -? --help                       Display help message
* -V --version                    Print version information
* -v --verbose                    Loud verbosity
* -q --quiet                      Quiet verbosity

###Example call:

    TaxIds2TreeCompare -i /taxdumpPath/ -l 4 -t input2.csv -o /outputPath/    

###Example input:
[Input taxonomy csv](http://www.tbi.univie.ac.at/~egg/TaxonomyTools/input2.csv), each column with comma separated taxids represents one tree

    3562,561
    9606,9607
    28048,85004
    144051,232795

###Example output:
This [dot output](http://www.tbi.univie.ac.at/~egg/TaxonomyTools/comparison.dot) can be visualised via the [graphviz](http://www.graphviz.org/) tool [dot](http://www.graphviz.org/pdf/dotguide.pdf)

    dot -Tsvg comparison.dot -o comparison.svg

This yields following svg output

![Output](http://www.tbi.univie.ac.at/~egg/TaxonomyTools/comparison.svg)
