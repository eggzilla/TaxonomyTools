##TreeDistance
Computes the distance between two input nodes on the given tree.

options [OPTIONS]

Common flags:
* -i --taxdumpdirectorypath=ITEM  Path to input NCBI taxonomy dump files directory
* -a --organism1=INT              NCBI Taxonomy Id of the first organism
* -b --organism2=INT              NCBI Taxonomy Id of the second organism
* -? --help                       Display help message
* -V --version                    Print version information
* -v --verbose                    Loud verbosity
* -q --quiet                      Quiet verbosity

###Example call:
The two nodes of interest are denoted by -a and -b. In this case Escherichia coli and Sulfolobus solfataricus 

    TreeDistance -i /scratch/egg/data/taxdump/ -a 562 -b 2287

###Example output:
Tree distance of both taxonomy node ids

    15
