# Project 4: Who Is Who -- Entity Resolution

### [Project Description](doc/project4_desc.md)

Term: Spring 2017

+ Team # 1
+ Projec title: Name Disambiguation Techniques on Paper 2(SVM) and Paper 6(HMRF-EM with constraints C2 and C6)
+ Team members
	+ Mei, Yuan (UNI:ym2583)
	+ Ruscassie, Raphael (UNI:rlr2166)
	+ Wang, Zhishan (UNI:zw2362)
	+ Zhou, Yuxi (UNI:yz3048)
	+ Zhu, He (UNI:hz2429)
+ Project summary: 
In paper 2, we utilize SVM model to solve the name disambiguation in author citations. In SVM, coauthor names appear to be the most robust attribute for name disambiguation; using coauthor information alone performs consistently well in all the datasets. Using journal title words usually gives better performance than using paper title words. In paper 6, we make attempts to vectorize each loop to reduce the running time, and use permutation to test the clustring sequence which gives the most accurate result.
	
**Contribution statement**:   
+ Team members
	+ Mei, Yuan (UNI:ym2583): Built the main structure of the HMRF-EM algorithm, the main coding person of Paper 6.
	+ Ruscassie, Raphael (UNI:rlr2166):Helped in building EM algorithm of Paper 6.
	+ Wang, Zhishan (UNI:zw2362): Assisted in setting up the SVM model of Paper 2.
	+ Zhou, Yuxi (UNI:yz3048) : Built constraints and evaluation of Paper 6, help with other parts and presenter.
	+ Zhu, He (UNI:hz2429): Built the main structure of the SVM model of Paper 2 and helped in coding of Paper 6 .


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
