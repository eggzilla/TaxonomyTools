
##TaxIds2Text
List of taxonomy ids is converted in a short text summary for each node.

options [OPTIONS]

Common flags:
* -i --taxdumpdirectorypath=ITEM  Path to input NCBI taxonomy dump files directory
* -r --taxonomicrank=ITEM         Requested taxonomic rank - default Class
* -t --taxnodelistfilepath=ITEM   Path to input taxonomy id list without header
* -? --help                       Display help message
* -V --version                    Print version information
* -v --verbose                    Loud verbosity
* -q --quiet                      Quiet verbosity

###Example call:

    TaxIds2Text -t input.tax -r Phylum -i /taxdumppath/ > out.csv

###Example input:
Taxonomy ids for Escherichia coli and Sulfolobus solfataricus

    562
    2287

###Example output:

    562,Phylum,Proteobacteria
    2287,Phylum,Crenarchaeota
