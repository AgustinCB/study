# Implementation detail: Mean normalization

In this lesson we will cover one last implementation detail, the mean normalization.

Let's consider an example in which there's a user that hasn't rated any movies. Our collaborative filter algorithm will return \theta[5] = [0 0] because the first term doesn't affect \theta[5] and the second just minimizes the regularization term. This doesn't seem very useful, our system will suppose that every new user will rate every movie as zero and it'll be difficult to recommend them any new movie.

To solve this, we will use the matrix Y. We will then create \mu, a vector of n[m] with the average of all the movie ratings. Then we will substract the mean from all the ratings in Y, to normalize each movie to have an average rating of zero.

Then we're going to use that to pass to the collaborative filtering algorithm and we learn \theta[j] and x[i] from it and modify our prediction algorithm to `\theta[j]'x[i] + \mu[i]`. Now, each new user, will have an initial score equals to the mean rating of the movies!
