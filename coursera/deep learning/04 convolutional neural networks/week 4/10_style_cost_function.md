# Style cost function

The overall cost function is: `J(G) = \alpha * J[content](C,G) + \beta J[style](S, G)`. We will see now `J[style]`.

What does the style of an image mean?

Let's say you are using layer l's activation to measure "style." Define style as correlation between activation across channels.

So we're going to ask how correlated are the activations across different channels?

We will look at channels that are close together and see how correlated they're together in different possitions with respect of n[h] and n[w]. The closer the correlation between two images, the more similar the style would be.

For any two channels to be highly correlated it means how often different features (such as texture) occur together in both channels.

With this, we grab the degree in which channels are correlated in the style image S and compare it with the correlations in the generated images to see how similar they are. We will do so using a Style matrix:

Let `a[l][i][j][k] = a(i,j,k)`, `G[l][k][k'] = sum(1, i, nh[l], sum(1, j, nw[l], a[l][i][j][k] * a[l][i][j][k']))`. This is also called Gram matrix.

Once you computed both matrixes (G[G], G[S]), we can define `J[style][l](S,G) = || G[l][S] - G[l][G] ||^2 * 1 / (2 n[l][h] n[l][w] n[l][c])`. And also: `J[style](S,G) = sum(1, l, L, \alpha[l] J[style][l](S,G))`.
