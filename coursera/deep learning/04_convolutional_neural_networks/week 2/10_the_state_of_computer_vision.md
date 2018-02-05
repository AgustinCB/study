# The state of computer vision

There're a few things that are unique about deep learning for computer vision. Let's see some of them.

Most machine learning problems fall somewhere in a spectrum from "little data" to "lots of data." For image recognition, even though we have millions of data available, it still doesn't feel enough. If we go to some more specific problems such as object detection, we have even less daata. On average, when you have a lot of data you find people getting away with simpler algorithms and less hand-engineering. When you don't have that much data, you see people engaging on things such as hand-engineering or different hacks.

We usually have two sources of knowledge:
- Labeled data.
- Hand engineered features/network architecture/other components.

Computer vision tries to learn a very complex problem and we often don't have as much data as we need. This is why we have relied more in hand engineering.

One thing that helps a lot when you have little data is transfer learning and is often the basis of relatively small datasets.

There's a lot of attention also on doing well on benchmarks and/or winning competitions.

How to do well on benchmarks (not necessarily useful when creating production software):

- Ensembling: Train several networks (between 3-15) independently and average their outputs.
- Multi-crop at test time: Run multiple versions of a test image through a classifier and average the results (usually using something called 10-crop classifier).

Some general advice:

- Use architectures of networks published in the literature.
- Use an open source implementation if possible.
- Use pretrained models and fine-tune on your dataset.
