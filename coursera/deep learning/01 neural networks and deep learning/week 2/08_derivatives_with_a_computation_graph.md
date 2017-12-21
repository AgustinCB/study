# Derivatives with a computation graph

In this lesson, we will use our computation graph to figure out the derivative of J.

Given our computation graph:

a\
  -----\
b\     v=a+u -> J=3v
 u = bc/
c/

Given a=5, b=3 and c=2:

How do we calculate the derivative of J with respect to v?

If we increase v by 0.001, then J will increase by 0.003. In general, is easy to see why J will increase x3 times the increment of v, so the derivative is 3.

How do we calculate the derivative of J with respect to a?

If we increase a by 0.001, then v get's increased to 11.001 and therefore J increases to 33.003. The derivative is three again. We can also see that (dv)/(da) is 1.

The derivative of J with respect to u is the same as (dJ)/(dv) * (dv)/(du), which is 3.

The derivative of J with respect to b is equal to (dJ)/(du) * (du)/(db), because (du)/(db) is 2 (because c=2), then we have that the derivative is 6.

Similarly, the derivative of J with respect to u, will be 9.
