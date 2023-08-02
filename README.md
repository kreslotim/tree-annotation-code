# Source code repository for the tree annotation tool

by [Daniel Harasim](https://people.epfl.ch/daniel.harasim),
[Christoph Finkensiep](https://people.epfl.ch/christoph.finkensiep),
and the [Digital and Cognitive Musicology Lab (DCML)](https://dcml.epfl.ch)

Find the web app [here](https://dcmlab.github.io/tree-annotation-code/).
New version available [here](https://kreslotim.github.io/tree-annotation-code/).

## Changelog
### Version 0.2.0 (August 2, 2023)

- Controls enhancements:
	* Added `Elaborate` functionality, allowing top-down tree expansion with new generated node quantities determined by `Split arity`. Accessible using the `e` shortcut key on the selected node(s).
	* Added `Unelaborate` functionality that serves as the counter-action to `Elaborate`, designed to eliminate descendants of the applied node. Accessible using the `u` or `Ctrl+E` shortcut keys on the selected node(s).
	* Added `Uncombine` functionality  that serves as the counter-action to `Combine`, effectively eliminating selected nodes and their ancestors, barring the leaves. Accessible via the `Backspace` or `Ctrl+U` shortcut keys on the selected node(s).
	* Improved `Delete` functionality, to permit deletion of all node types in the tree, including leaves or roots as well. Accessible via `delete` shortcut key on the selected node(s).
	* Added `Undo` & `Redo` functionalities to manipulate the history of forest modifications. Accessible through `Ctrl+Z` and `Ctrl+Y` shortcut keys.
	* Added `⬅` and `➡` buttons to create new root nodes on the corresponding side of the tree, accessible via respective arrow keys.
	
* Visual interpretation
	* Enhanced node aesthetics, ensuring nodes extend to the edges of the viewbox, always forming a rectangular forest shape.
	* Added `Reverse tree` option to render the forest in an upside-down format, placing leaves at the top and roots at the bottom, akin to the previous version.
	* Added `Math tree` option to render all forest nodes and preview tree labels in $\LaTeX$ format.
	* Included the ability to manually enter $\LaTeX$-formatted input into a specific node, achieved by renaming the node and enclosing the input text with either `$` or `$$` symbols.
	
* User interaction
	* Resolved issues with renaming nodes, which can now be confirmed by clicking outside of the node, in addition to pressing `Enter` or `r` while editing.
	* Introduced a scrolling feature to navigate the webpage in all directions, useful when the tree dimensions exceed screen size.
	
* Output saving
	* Added `Save forest` feature to enable saving the forest image in .png format.
	* Added `Save preview` feature to enable saving the image of the preview tree in .svg format.

![A screenshot of the tool](screenshot.jpg)

The [tree annotation tool](https://dcmlab.github.io/tree-annotation-code/) is a simple and easy-to-use web app for creating trees.
The user provides a sequence of symbols and then creates a tree from bottom to top by successively combining elements.
The tool includes functionality for loading and exporting trees
in JSON or [qtree](https://ctan.org/pkg/tikz-qtree) format (useful for LaTeX),
as well as a preview visualization of the current tree.
Trees can be easily shared using special links that encode a tree in the URL,
[like this](https://dcmlab.github.io/tree-annotation-code/?tree=eyJsYWJlbCI6IkMiLCJjaGlsZHJlbiI6W3sibGFiZWwiOiJDIiwiY2hpbGRyZW4iOltdfSx7ImxhYmVsIjoiQyIsImNoaWxkcmVuIjpbeyJsYWJlbCI6Ikc3IiwiY2hpbGRyZW4iOlt7ImxhYmVsIjoiRG0iLCJjaGlsZHJlbiI6W119LHsibGFiZWwiOiJHNyIsImNoaWxkcmVuIjpbXX1dfSx7ImxhYmVsIjoiQyIsImNoaWxkcmVuIjpbXX1dfV19).

## Attribution
If you use this application or its source code in any way, please cite the the following paper:

D. Harasim, C. Finkensiep, P. Ericson, T. J. O'Donnell, and M. Rohrmeier (2020). The Jazz Harmony Treebank. In *Proceedings of the 21th International Society for Music Information Retrieval Conference*. Montréal, Canada.

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.4245406.svg)](https://doi.org/10.5281/zenodo.4245406)

## Funding 
This project has received funding from the European Research Council
(ERC) under the European Union's Horizon 2020 research and innovation
program under grant agreement No 760081 – PMSB. We gratefully
acknowledge the support of the Natural Sciences and Engineering
Research Council of Canada (NSERC), the Fonds de Recherche du
Qu\'{e}bec, Soci\'{e}t\'{e} et Culture (FRQSC), and the Canada CIFAR
AI Chairs program. We thank Claude Latour for supporting this research
through the Latour Chair in Digital Musicology. The authors
additionally thank the anonymous referees for their valuable comments
and the members of the Digital and Cognitive Musicology Lab (DCML) for
fruitful discussions.
