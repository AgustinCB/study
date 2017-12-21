# Logistic regression cost function

We said that our cost function would be:

y' = sigmoid(w'x + b) = p(y=1|x)
sigmoid(x) = 1 / (1+e^(-x))
J(w, b) = 1/m sum(1, m, i, l(y'[i], y[i])) = -1/m sum(1, m, i, (y[i] log(y'[i]) + (1-y[i]) log(1-y'[i])))

If y'=1, then p(y|x) y', if y=0 then p(y|x) = 1 - y.

Then we can say: `p(y|x) = y'^y (1-y')^(1-y)`.

Because log(p(y|x)) = log(y'^y (1-y')^(1-y)) = y log y' + (1-y) log(1-y') = -l(y', y).

## Cost on m examples

p (labels in training set) = prod(1, m, i, p(y[i], x[i]))

Maximizing that is the same as maximizing the log of that. So we can say:

log(p(...)) = sum(1, m, i, log(p(y[i]|x[i]))) = - sum(1, m, i, l(y'[i], y[i]))
