# What's end-to-end deep learning

In this lesson we will see how to take multiple stages of a data pipeline and replace them with one big system.

Let's take speech recognition as an example, where you have an audio and you want a transcript of that audio. Traditionally, you'd make a system like this:

Audio -> Features -> Phonemes -> Words -> Transcript

With end-to-end neural networks you can do this simply by doing audio -> transcript. To do so, you may need a lot of data to make it work (from 10000h to 100000h of data). With smaller datasets, a traditional approach works as well and usually better.

Often, though, you will break this into steps, but less bigger, usually two. Mostly because you don't have enough data to do the end-to-end approach.
