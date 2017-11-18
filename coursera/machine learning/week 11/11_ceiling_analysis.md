# Ceiling analysis: What part of the pipeline to work on next

One of the most valuable resources is your time and you want to avoid spending a lot of time developing some component only to realise that it doesn't make a huge difference. In this lesson we will cover a concept called ceiling analysis to decide what part of the pipeline we want to work on next.

Coming back to the OCR pipeline:

Image -> Text detection -> Character segmentation -> character recognition

Where should you allocate resources?

In order to make decisions on how your system performs, we need a single level to measure our performance. Let's say that we have an accuracy of 72% in the whole system.

The idea behind ceiling analysis is to simulate what would happen with the accuracy if for every step of the pipeline you had perfect performance. You want to make a table such as:

```
Component | Accuracy
--------------------
Overall   | 72%
T. detect.| 89%
c. segmen.| 90%
c. recogn.| 100%
```

As you see, having a perfect text detection improved our pipeline by 17%. Having perfect character segmentation, improved it by only 1%. And having perfect character recognition, the performance went up 10%.

So now you have a good idea of the upside potential of improving any of this components.
