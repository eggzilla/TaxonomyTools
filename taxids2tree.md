##TaxIds2Tree
List of taxonomy ids is converted into a graphical tree
representation either as .svg (via graphviz) or as .json (via d3js)

options [OPTIONS]

Common flags:
* -i --taxdumpdirectorypath=ITEM  Path to input NCBI taxonomy dump files directory
* -t --taxnodelistfilepath=ITEM   Path to input taxonomy id list without header
* -r --aliencsvfilepath=ITEM      Path to RNAlienResult CSV. Alternative to input taxonomy id list
* -l --levels=INT                 Number defining maximum distance from root for nodes in subtree.
* -f --outputformat=ITEM          Requested output format (json,dot). Default: dot
* -o --outputdirectorypath=ITEM   Path to output directory
* -? --help                       Display help message
* -V --version                    Print version information
* -v --verbose                    Loud verbosity
* -q --quiet                      Quiet verbosity

###Example call 1:

    Ids2Tree -i /taxdumppath/ -o /outputpath/ -t input.tax -f json -l 3

###Example input:
RefSeq accessions for Escherichia coli and Sulfolobus solfataricus

    562
    2287

###Example output:
This json output can be visualised via [d3js](http://d3js.org/)

    {"children":[{"children":[{"children":[{"children":[],"name":"Proteobacteria"}],"name":"Bacteria"},{"children":[{"children":[],"name":"Crenarchaeota"}],"name":"Archaea"}],"name":"cellular organisms"}],"name":"root"}

###Example call 2:

    Ids2Tree -i /taxdumppath/ -o /outputpath/ -t input.tax -f svg -l 3

###Example input:
RefSeq accessions for Escherichia coli and Sulfolobus solfataricus

    562
    2287

###Example output:
This dot output can be visualised via the [graphviz](http://www.graphviz.org/) tool [dot](http://www.graphviz.org/pdf/dotguide.pdf)

    dot -Tsvg input.dot -o taxonomy.svg

This yields following svg output

![Output](http://www.tbi.univie.ac.at/~egg/TaxonomyTools/taxonomy.svg)
