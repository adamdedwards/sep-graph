# SEP Graph

A graphical representation and analysis of the Stanford Encyclopedia of Philosophy.

## Project Description

The [Stanford Encyclopedia of Philosophy][SEP] (hereafter, SEP) is a "dynamic reference work" that has a significant influence in philosophy. This project is an attempt to represent the data in the SEP graphically. The methodology of the representation is as follows:

+ Each article in the SEP is a node.
+ A directed edge connects a source and target node iff the article represented by the source contains a hyperlink to the article represented by the target.
+ Otherwise, nodes are not directly connected.

## Example Graph

See [this example graph of the Fall 2000 SEP archive][1] for an example of a visualization in threejs.


## Thanks

Thanks to the developers of [threejs][2], [statnet][3], [d3][4], and [igraph][5] (and everyone working on R I forgot to mention). 

[SEP]: https://plato.stanford.edu
[1]: https://adamdedwards.github.io/sep-graph/pages/fall2000
[2]: https://github.com/bwlewis/rthreejs
[3]: http://statnet.csde.washington.edu/
[4]: https://github.com/d3
[5]: https://github.com/igraph
