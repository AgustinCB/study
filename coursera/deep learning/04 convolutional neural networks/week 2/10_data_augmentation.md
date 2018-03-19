# Data augmentation

In this lesson we will see how to use data augmentation to improve the performance of computer vision systems. As it will always help when you're training a computer vision system.

The simplest data augmentation method is mirroring on the vertical axis. For most computer vision tasks, a mirror is still the cathegory.

Another common technique is random cropping: Take an image and crop it in random ways to generate new subimages. This isn't a perfect random augmentation technique as long as your images are large enough.

Other things like rotation, shearing or local warping may be worth, but in practice are used less.

A second type of data augmentation technique is called color shifting: We add to the RGB channels different distortions: For example, add 20 to read, substract 20 to green and add 20 to blue. By distorting the color channels you get new images with different colors.

There are different ways to implement color distortion, one is called PCA (talked in the machine learning course), given in the AlexNet paper and often referred as "PCA color augmentation." In practice it grabs two of the three channels and augments them a lot leaving the third one almost intact.

You may have a your data stored in a hard disk. With an small dataset you may be ok. With big datasets, you may have a CPU thread that would be constantly loading images from the disk and apply a distortion to it forming mini-batches of data that are passed to some other process that implements training, often in another CPU thread (or on the GPU).
