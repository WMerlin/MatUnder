# Project B
#### *Roguski Mikołaj*
### progress so far
I've read the more applicable parts of Mukherjee et al[1],
Belghazi, Mohamed Ishmael, et al. "Mine: mutual information neural estimation." arXiv preprint arXiv:1801.04062 (2018), and  skimmed through Runge
### plan
* estimate p(y|z) using kNN based permutations and CVAE
* use neural network to estimate KL divergence using both DV and NWJ representations
* use DV and NWJ in MINE for I(X,Z;Y) and I(X;Y)
* maybe use *f*-divergence in last 2 steps
* find some other estimator as baseline