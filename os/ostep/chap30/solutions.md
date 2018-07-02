1. Our first question focuses on main-two-cvs-while.c (the working solution). First, study the code. Do you think you have an understanding of what should happen when you run the program?

Yeah, I do.


2. Now run with one producer and one consumer, and have the producer produce a few values. Start with a buffer of size 1, and then increase it. How does the behavior of the code change when the buffer is larger? (or does it?) What would you predict num full to be with different buffer sizes (e.g., -m 10) and different numbers of produced items (e.g., -l 100), when you change the consumer sleep string from default (no sleep) to -C 0,0,0,0,0,0,1?

It doesn't change much with the addition of only buffer size.

I'd predict it to be full and almost full most of the time.

3. If possible, run the code on different systems (e.g., a Mac and Linux). Do you see different behavior across these systems?

Ran it in Linux and Windows. Noticed no difference.

4. Let’s look at some timings of different runs. How long do you think the following execution, with one producer, three consumers, a single-entry shared buffer, and each consumer pausing at point c3 for a second, will take?

I think it'd take approximately `l + c - 1` seconds.

5. Now change the size of the shared buffer to 3 (-m 3). Will this make any difference in the total time?

No, it won't.

6. Now change the location of the sleep to c6 (this models a consumer taking something off the queue and then doing something with it for a while), again using a single-entry buffer. What time do you predict in this case?

Roughly l/2.

7. Finally, change the buffer size to 3 again (-m 3). What time do you
predict now?

Same time, it's not a function of buffer size.

8. Now let’s look at main-one-cv-while.c. Can you configure a sleep string, assuming a single producer, one consumer, and a buffer of size 1, to cause a problem with this code?

No, I'll need another consumer to cause a problem.
