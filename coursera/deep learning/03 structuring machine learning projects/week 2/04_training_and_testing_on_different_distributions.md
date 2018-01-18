# Training and testing on different distributions

In this lesson, we will see how to train on data that comes from different distributions than your test and dev data.

Let's say that you have a mobile app that lets user upload photos and tells them if they're cats or not. You can then get data from the mobile app (probably blurry or with bad resolution) or by crawling the web (high quality and resolution). The first one has about 10000 samples and the later 200000. However, you want your algorithm to do well on the first case.

You can put both of those datasets together and randomly shuffle them into a train, dev and test set, with a proportion of 98%, 1%, 1%. This has some advantages:

- All sets come from the same distributions

But some disadvantages:

- In this case, the dev set contains most of its samples from the crawl distribution rather than what you care about, the mobile app.

So this is really not a good option.

Instead, you can have the test and dev set all mobile app images and your training set the remaning plus the the rest of the images. The advantage of this is that you're now really aiming where you want to go (i.e. your dev set contains what you want to optimize). The disadvantage is that now you have different distributions. However, this will give you better performance in the long term.
