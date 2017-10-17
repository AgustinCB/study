# Prioritizing what to work on

In the next lessons we will try to cover the are of machine learning system design. We will cover advices for how to put together machine learning system.

We will start by prioritizing what to work on. To do so, we will use a project example.

## Spam classifier

We what to get a model to classify a mail as spam (1) or non-spam (0).

In order to apply supervised learning, we should decidew how to represent hte features of the email.

For example, we can come up with a list of 100 words that are indicative of being spam or not. We can then take a piece of email and encoded in this way:

We will sort the list of feature words, for each word in that list, we'll set 1 if it exists or 0 if it doesn't in a new list and use this new list as the input for the problem.

In practice, we won't pick the words manually: We will go over a training set and pick the most common N words and use those as our features.

How to spend your time to make it have low error? One common approach is to spend time collecting tons of data. This will often help, but not all the time.

You can also develop sophisticated features based on email routing information, for example. If there's a fake header or an unusual route in the email header, then we can develop a feature based on that.

Another option is to look at the image message body and be smarter. Should "discount" be considered the same as "discounts", for example?

Or we could develop sophisticated algorithm to detect misspellings and don't allow spammers to trick us by not using the correct syntaxis of a word.

It's easy to brainstorm this kind of ideas, but not to validate them. Randomly trying some of these options is not a good way to approach this problem. It's possible to do better and try to have a more systematic way to chose what's a good way to spend your time.
