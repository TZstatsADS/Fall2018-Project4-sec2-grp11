# Project: OCR (Optical Character Recognition) 

![image](figs/intro.png)

### [Full Project Description](doc/project4_desc.md)

Term: Fall 2018

+ Team # 11
+ Team members
	+ Chen, Yen-Hsiang yc3528@columbia.edu
	+ Dong, Xiaojing xd2195@columbia.edu
	+ Lin, Shaolong sl4095@columbia.edu
	+ Xie, Huiming hx2238@columbia.edu

+ Project summary: In this project, we created an OCR post-processing procedure to enhance Tesseract OCR output. First, for error detection, we used SVM model with RBF kernel to classify error and correct tokens. We performed the cross validation using recall as evaluation metric, since the undetected errors will not get into the correction phase. As for correction, we used Bayesian probability scoring method with contextual constraints, which utilizes three types of information sources: prior, channel and context probabilities. ELE and Good-Turing estimation methods were applied for frequency adjustment. Finally, we evaluate the performance the algorithms by comparing the word-level and character-level precision and recall. In general, adopting the idea of SVM garbage detection and probability scoring with contextual constraints can improve the Tesseract OCR output. However, these algorithms are not designed specifically for OCR errors and therefore has certain limitations.
	
**Contribution statement**:
	+ Major contributors: Dong, Xiaojing and Xie, Huiming. Xiaojing and Huiming together completed the SVM model for error detection (including feature extraction and model fitting), developed the probability scoring for error correction, and generated the main reports. Xiaojing carried out the computation of prior, channel and context probabilities, and developed the evaluation measures in the correction part. Huiming carried out the preparation of data set, developed the evaluation measures for detection and overall procedure, and prepared the presentation.

Following [suggestions](http://nicercode.github.io/blog/2013-04-05-projects/) by [RICH FITZJOHN](http://nicercode.github.io/about/#Team) (@richfitz). This folder is orgarnized as follows.

```
proj/
├── lib/
├── data/
├── doc/
├── figs/
└── output/
```

Please see each subfolder for a README file.
