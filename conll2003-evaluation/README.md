
NICTA t3as NER CoNLL-2003 evaluation
====================================

To compare the NICTA t3as NER library with other NER tools we will be doing a CoNLL-2003 evaluation of it. This gives an overview of the CoNLL-2003 task:

<http://www.cnts.ua.ac.be/conll2003/ner/>

By carrying out the same evaluation we can then compare ourselves to the results of the other teams that participated, as given on the web page linked above.


## Performing the CoNLL 2003 evaluation

Follow the instructions at the URL until you have a working `baseline`, then run:

    ./conll2003 eng.testb 2> disagreements.list | $CONLL_HOME/bin/conlleval

When the `conll2003` tool is run it prints out all the disagreements on `stderr` between the ground truth and the NICTA NER classification for tokens in the sentences. In the command line above this is redirected to the file `disagreements.list`. This file can then be analysed to try to understand how the NICTA NER library can be improved.
    

## NICTA t3as NER scores

The `testa + testb` results are simply the two test files concatenated together after one of the empty lines at the end of `eng.testa` has been removed (to make the `DOCSTART` break correct). The `all data` is the `eng.train + eng.testa + eng.testb` files concatenated together (no `baseline` can be computed for this dataset). 

### 2014-08-24 - Current score
#### testa
    processed 51578 tokens with 5942 phrases; found: 6065 phrases; correct: 3805.
    accuracy:  92.80%; precision:  62.74%; recall:  64.04%; FB1:  63.38
                  LOC: precision:  71.14%; recall:  68.15%; FB1:  69.61  1760
                 MISC: precision:  59.11%; recall:  50.33%; FB1:  54.36  785
                  ORG: precision:  37.88%; recall:  48.62%; FB1:  42.59  1721
                  PER: precision:  79.88%; recall:  78.01%; FB1:  78.93  1799

#### testb
    processed 46666 tokens with 5648 phrases; found: 5767 phrases; correct: 3521.
    accuracy:  91.59%; precision:  61.05%; recall:  62.34%; FB1:  61.69
                  LOC: precision:  65.73%; recall:  64.39%; FB1:  65.05  1634
                 MISC: precision:  54.66%; recall:  43.45%; FB1:  48.41  558
                  ORG: precision:  48.27%; recall:  55.33%; FB1:  51.56  1904
                  PER: precision:  73.19%; recall:  75.63%; FB1:  74.39  1671

#### testa + testb
    processed 98244 tokens with 11590 phrases; found: 11832 phrases; correct: 7326.
    accuracy:  92.23%; precision:  61.92%; recall:  63.21%; FB1:  62.56
                  LOC: precision:  68.53%; recall:  66.36%; FB1:  67.43  3394
                 MISC: precision:  57.26%; recall:  47.35%; FB1:  51.84  1343
                  ORG: precision:  43.34%; recall:  52.33%; FB1:  47.41  3625
                  PER: precision:  76.66%; recall:  76.90%; FB1:  76.78  3470

#### all data
    processed 302811 tokens with 35089 phrases; found: 35986 phrases; correct: 22110.
    accuracy:  92.41%; precision:  61.44%; recall:  63.01%; FB1:  62.22
                  LOC: precision:  68.98%; recall:  66.71%; FB1:  67.83  10294
                 MISC: precision:  56.15%; recall:  46.56%; FB1:  50.91  4198
                  ORG: precision:  43.66%; recall:  52.69%; FB1:  47.75  11251
                  PER: precision:  75.56%; recall:  76.95%; FB1:  76.25  10243


### Old scores

Older scores can be found [here](OldResults.md).


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
                 
### testa + testb
    processed 98244 tokens with 11590 phrases; found: 8946 phrases; correct: 6751.
    accuracy:  84.01%; precision:  75.46%; recall:  58.25%; FB1:  65.75
                  LOC: precision:  76.33%; recall:  80.77%; FB1:  78.49  3709
                 MISC: precision:  83.11%; recall:  73.95%; FB1:  78.27  1445
                  ORG: precision:  70.56%; recall:  57.16%; FB1:  63.16  2432
                  PER: precision:  73.75%; recall:  29.00%; FB1:  41.63  1360


## Full CoNLL-2003 results (testb)
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
