
NICTA t3as NER CoNLL-2003 evaluation
====================================

To compare the NICTA t3as NER library with other NER tools we will be doing a CoNLL-2003 evaluation of it. This gives an overview of the CoNLL-2003 task:

<http://www.cnts.ua.ac.be/conll2003/ner/>

By carrying out the same evaluation we can then compare ourselves to the results of the other teams that participated, as given on the web page linked above.


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

### testa without B- and I- prefix (`conlleval -r`)
    processed 51578 tokens with 8603 phrases; found: 6206 phrases; correct: 1230.
    accuracy:  84.76%; precision:  19.82%; recall:  14.30%; FB1:  16.61
                B-LOC: precision:   0.00%; recall:   0.00%; FB1:   0.00  1946
               B-MISC: precision:   0.24%; recall:  50.00%; FB1:   0.48  826
                B-ORG: precision:   0.00%; recall:   0.00%; FB1:   0.00  1221
                B-PER: precision:   0.00%; recall:   0.00%; FB1:   0.00  955
                I-LOC: precision:  89.15%; recall:   9.03%; FB1:  16.39  212
               I-MISC: precision:  98.56%; recall:  16.22%; FB1:  27.85  208
                I-ORG: precision:  98.60%; recall:  13.48%; FB1:  23.72  286
                I-PER: precision: 100.00%; recall:  17.53%; FB1:  29.83  552

### testb without B- and I- prefix (`conlleval -r`)
    processed 46666 tokens with 8112 phrases; found: 4758 phrases; correct: 738.
    accuracy:  83.18%; precision:  15.51%; recall:   9.10%; FB1:  11.47
                B-LOC: precision:   0.17%; recall:  50.00%; FB1:   0.34  1763
               B-MISC: precision:   0.97%; recall:  66.67%; FB1:   1.91  619
                B-ORG: precision:   0.41%; recall: 100.00%; FB1:   0.82  1211
                B-PER: precision:   0.00%; recall:   0.00%; FB1:   0.00  405
                I-LOC: precision:  89.07%; recall:   8.49%; FB1:  15.51  183
               I-MISC: precision:  90.00%; recall:   9.90%; FB1:  17.84  100
                I-ORG: precision:  98.17%; recall:  12.89%; FB1:  22.78  327
                I-PER: precision: 100.00%; recall:   5.41%; FB1:  10.26  150


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
