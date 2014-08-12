
NICTA t3as NER CoNLL-2003 evaluation
====================================

To compare the NICTA t3as NER library with other NER tools we will be doing a CoNLL-2003 evaluation of it. This gives an overview of the CoNLL-2003 task:

<http://www.cnts.ua.ac.be/conll2003/ner/>

By carrying out the same evaluation we can then compare ourselves to the results of the other teams that participated, as given on the web page linked above.


## Performing the CoNLL 2003 evaluation

Follow the instructions at the URL until you have a working `baseline`, then run:

    ./conll2003 eng.testb | $CONLL_HOME/bin/conlleval

To investigate where things have gone wrong, do something like the following to see each word followed by the manual human ground truth classification followed by the NICTA NER classification:

    ./conll2003 eng.testb > nicta.eng.testb
    cat nicta.eng.testb | cut -d\  -f1,4,5 | less
    

## NICTA t3as NER scores

### 2014-08-11 - Current score
#### testa
    processed 51578 tokens with 5942 phrases; found: 6051 phrases; correct: 3129.
    accuracy:  90.96%; precision:  51.71%; recall:  52.66%; FB1:  52.18
                  LOC: precision:  60.72%; recall:  66.14%; FB1:  63.31  2001
                 MISC: precision:  21.23%; recall:  43.06%; FB1:  28.44  1870
                  ORG: precision:  48.61%; recall:  10.44%; FB1:  17.19  288
                  PER: precision:  72.78%; recall:  74.76%; FB1:  73.75  1892
#### testb
    processed 46666 tokens with 5648 phrases; found: 5750 phrases; correct: 2729.
    accuracy:  88.97%; precision:  47.46%; recall:  48.32%; FB1:  47.89
                  LOC: precision:  55.91%; recall:  65.23%; FB1:  60.21  1946
                 MISC: precision:  17.37%; recall:  43.02%; FB1:  24.74  1739
                  ORG: precision:  48.04%; recall:  11.08%; FB1:  18.00  383
                  PER: precision:  68.67%; recall:  71.43%; FB1:  70.02  1682


## CoNLL-2003 Baseline results

The baseline results that anybody that was looking to participate in CoNLL-2003 should beat are:

### testa
    processed 51578 tokens with 5942 phrases; found: 4948 phrases; correct: 3876.
    accuracy:  84.76%; precision:  78.33%; recall:  65.23%; FB1:  71.18
                  LOC: precision:  78.26%; recall:  82.91%; FB1:  80.52  1946
                 MISC: precision:  88.38%; recall:  79.18%; FB1:  83.52  826
                  ORG: precision:  69.70%; recall:  63.46%; FB1:  66.43  1221
                  PER: precision:  80.84%; recall:  41.91%; FB1:  55.20  955

### testb
    processed 46666 tokens with 5648 phrases; found: 3998 phrases; correct: 2875.
    accuracy:  83.18%; precision:  71.91%; recall:  50.90%; FB1:  59.61
                  LOC: precision:  74.19%; recall:  78.42%; FB1:  76.25  1763
                 MISC: precision:  76.09%; recall:  67.09%; FB1:  71.31  619
                  ORG: precision:  71.43%; recall:  52.08%; FB1:  60.24  1211
                  PER: precision:  57.04%; recall:  14.29%; FB1:  22.85  405


## Full CoNLL-2003 results
                +-----------+---------+-----------+
     English    | precision |  recall |     F     |
    +------------+-----------+---------+-----------+
    | [FIJZ03]   |  88.99%   |  88.54% | 88.76±0.7 |
    | [CN03]     |  88.12%   |  88.51% | 88.31±0.7 |
    | [KSNM03]   |  85.93%   |  86.21% | 86.07±0.8 |
    | [ZJ03]     |  86.13%   |  84.88% | 85.50±0.9 |
    | [CMP03b]   |  84.05%   |  85.96% | 85.00±0.8 |
    | [CC03]     |  84.29%   |  85.50% | 84.89±0.9 |
    | [MMP03]    |  84.45%   |  84.90% | 84.67±1.0 |
    | [CMP03a]   |  85.81%   |  82.84% | 84.30±0.9 |
    | [ML03]     |  84.52%   |  83.55% | 84.04±0.9 |
    | [BON03]    |  84.68%   |  83.18% | 83.92±1.0 |
    | [MLP03]    |  80.87%   |  84.21% | 82.50±1.0 |
    | [WNC03]*   |  82.02%   |  81.39% | 81.70±0.9 |
    | [WP03]     |  81.60%   |  78.05% | 79.78±1.0 |
    | [HV03]     |  76.33%   |  80.17% | 78.20±1.0 |
    | [DD03]     |  75.84%   |  78.13% | 76.97±1.2 |
    | [Ham03]    |  69.09%   |  53.26% | 60.15±1.3 |
    +------------+-----------+---------+-----------+
    | baseline   |  71.91%   |  50.90% | 59.61±1.2 |
    +------------+--------- -+---------+-----------+
