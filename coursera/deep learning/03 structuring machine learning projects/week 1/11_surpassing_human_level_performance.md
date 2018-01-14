# Surpassing human-level performance

In this lesson we will see when you surpass human-level performance.

Let's consider a problem with this error:

1. Team of humans: 0.5% error
2. One human: 1% error.
3. Training error: 0.6% error.
4. Dev error: 0.8%

What's the avoidable bias? In this case, it would be 0.1%.

Now let's consider this:

1. Team of humans: 0.5% error
2. One human: 1% error.
3. Training error: 0.3% error.
4. Dev error: 0.4%

Here you don't really have information about the avoidable bias, so it's difficult to know how to improve the performance of your algorithm. Your options are now less clear.

There're many problems where this happens:

- Online advertising.
- Product recommendations.
- Logistics (predicting transit time).
- Loan approvals.

All this are problems on which machine learning performs much better. In all this cases, we have structured data and not natural perception problems (computer vision, speech recognition, natural language processing) and when we have lots of data to look at.

There're some speech recognition systems that surpass human level performances and image recognition, but those required much more research and work and it's constricted to some cases.
