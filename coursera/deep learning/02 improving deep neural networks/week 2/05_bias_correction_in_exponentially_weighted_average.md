# Bias correction in exponentially weighted average

In this lesson, we will use bias correction to make the computation of weighted averages more accurate.

By using the formula that we gave previously, the initialization to zero makes the first values be slightly off at the begining. There's a way to modify this estimate to make it more accurate by dividing by `(1-\beta^i)`. While this i is small it will bump the value, but as it gets bigger, it will have less impact.
