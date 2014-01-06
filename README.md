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
There is also an example (under development) to draw a legend to describe
the meaning of the parameters. To show what I'm looking for, include a hand-drawn
legend below, inspired by an image from _The Self-Made Tapestry_ by Philip Ball.

Sadly, the trees appear dead, because I have yet to figure out how to use
scale-invariance to put needles on the twigs. Even so, they served my purpose
of generating images for a homemade Christmas card, below.

A comparison of nine different trees:

![Comparison](https://github.com/bobgru/conifer/blob/master/images/card-comparison.png?raw=true "Comparison")

A larger rendering of the tree from the lower left corner above:

![Individual](https://github.com/bobgru/conifer/blob/master/images/card-individual.png?raw=true "Individual")

A hand-drawn legend describing six of the parameters:

![Legend](https://github.com/bobgru/conifer/blob/master/images/hand-drawn-legend.png?raw=true "Legend")
