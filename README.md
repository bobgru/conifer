Conifer
=======

![Travis CI status](https://travis-ci.org/bobgru/conifer.png?branch=master;raw=true)

The Conifer library models an ideal conifer as an expression of a
genome, that is, a set of parameters that affect growth over time.
There are about a dozen parameters, some of which incorporate small
variations to mimic environmental effects. It is a hand-compiled
L-system, and one of the future directions is to implement an L-system
parser and compiler and to abstract the conifer as an example.

The tree is modeled in 3D as a collection of whorls of branches rising
above an origin. A year corresponds to a level of recursive expansion, 
which means a new level of branching. Within a year, there are possibly
multiple whorls, and partial growth on the existing branches.

Each year the trunk adds an increment of height, with whorls evenly
spaced vertically and rotated slightly one from the next. Branching from
the trunk happens at varying angles within a whorl. Branching within a 
branch is three-way: continuing straight, and at an angle to each side.
The straight branch and side branches grow at different ratios of length
with respect to the length of their parent twig.

The tree is created in relative coordinates, where the origin represents the
branching point. It is converted to absolute coordinates and projected onto
a plane for rendering.

Example programs show how to draw an individual tree, and an array of them.
There is also an example to draw a legend, shown below, to describe the meaning
of the parameters, inspired by an image from _The Self-Made Tapestry_ by Philip Ball.
The SVG file looks much better than the PNG file shown below, mainly because the
fonts were unfortunately converted from serif to sans-serif.

A comparison of nine different trees:

![Comparison](https://github.com/bobgru/conifer/blob/master/images/comparison-needles.png?raw=true "Comparison")

An individual tree, without needles:

![Individual without needles](https://github.com/bobgru/conifer/blob/master/images/individual-no-needles.png?raw=true "Individual without needles")

The same tree with needles:

![Individual with needles](https://github.com/bobgru/conifer/blob/master/images/individual-needles.png?raw=true "Individual with needles")

A legend describing six of the parameters:

![Legend](https://github.com/bobgru/conifer/blob/master/images/legend.png?raw=true "Legend")

While working on the computer-drawn legend, I did a study of drawing elliptical arcs that
produced the following picture:

![Ellipse](https://github.com/bobgru/conifer/blob/master/images/ellipse.png?raw=true "Ellipse")
