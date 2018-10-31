# SEP Graph

A graphical representation and analysis of the Stanford Encyclopedia of Philosophy.

## Project Description

The [Stanford Encyclopedia of Philosophy][1] (hereafter, SEP) is a "dynamic reference work" that has a significant influence in philosophy. This project is an attempt to represent the data in the SEP graphically. The methodology of the representation is as follows:

+ Each article in the SEP is a node.
+ A directed edge connects a source and target node iff the article represented by the source contains a hyperlink to the article represented by the target.
+ Otherwise, nodes are not directly connected.

## Example Graph

See [this link][2] for an example graph of the SEP Winter 1997 archive.

[1]: http://plato.stanford.edu
[2]: /examplegraph.html
