# SEP Graph

A graphical representation and analysis of the Stanford Encyclopedia of Philosophy.

## Project Description

The [Stanford Encyclopedia of Philosophy][SEP] (hereafter, SEP) is a "dynamic reference work" that has a significant influence in philosophy. This project is an attempt to represent the data in the SEP graphically. The methodology of the representation is as follows:

+ Each article in the SEP is a node.
+ A directed edge connects a source and target node iff the article represented by the source contains a hyperlink to the article represented by the target.
+ Otherwise, nodes are not directly connected.

## Example Graph

See these links ([1][2],[2][2],[3][2]) for an example graph of the SEP Winter 1997 archive.

[SEP]: https://plato.stanford.edu
[1]: https://adamdedwards.github.io/sep-graph/Net1.html
[2]: https://adamdedwards.github.io/sep-graph/Net2.html
[3]: https://adamdedwards.github.io/sep-graph/Net3.html
