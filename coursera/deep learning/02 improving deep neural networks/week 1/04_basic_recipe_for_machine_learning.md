# Basic recipe for machine learning

In this lesson, we will see what is the basic recipe of machine learning to get a high performance out of our algorithms.

We would ask ourselves some questions:

- Is there high bias? (training set performance)
  - Try a bigger network.
  - Train it longer (more iterations).
  - Try different neural networks architecture.
- Is there high variance? (dev set performance)
  - Get more data.
  - Regularization.
  - Try different neural networks architecture.
- Done!

In the earlier era of machine learning, there was a discussion around bias/variance tradeoff. However, in the modern era, so long you can keep training a bigger network and you can keep getting more data, you can reduce both bias and variance without hurting the other one.
