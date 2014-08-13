
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

### 2014-08-12 - Current score
#### testa
    processed 51578 tokens with 5942 phrases; found: 6065 phrases; correct: 3305.
    accuracy:  91.29%; precision:  54.49%; recall:  55.62%; FB1:  55.05
                  LOC: precision:  60.66%; recall:  77.14%; FB1:  67.91  2336
                 MISC: precision:  27.91%; recall:  40.56%; FB1:  33.07  1340
                  ORG: precision:  37.89%; recall:  10.96%; FB1:  17.00  388
                  PER: precision:  68.32%; recall:  74.21%; FB1:  71.14  2001

#### testb
    processed 46666 tokens with 5648 phrases; found: 5767 phrases; correct: 2952.
    accuracy:  89.50%; precision:  51.19%; recall:  52.27%; FB1:  51.72
                  LOC: precision:  56.12%; recall:  76.92%; FB1:  64.90  2286
                 MISC: precision:  25.76%; recall:  42.31%; FB1:  32.02  1153
                  ORG: precision:  42.27%; recall:  12.34%; FB1:  19.11  485
                  PER: precision:  63.32%; recall:  72.17%; FB1:  67.46  1843

#### testa + testb
    processed 98244 tokens with 11590 phrases; found: 11832 phrases; correct: 6257.
    accuracy:  90.44%; precision:  52.88%; recall:  53.99%; FB1:  53.43
                  LOC: precision:  58.42%; recall:  77.03%; FB1:  66.45  4622
                 MISC: precision:  26.92%; recall:  41.32%; FB1:  32.60  2493
                  ORG: precision:  40.32%; recall:  11.73%; FB1:  18.17  873
                  PER: precision:  65.92%; recall:  73.26%; FB1:  69.40  3844

#### all data
    processed 302811 tokens with 35089 phrases; found: 35986 phrases; correct: 18995.
    accuracy:  90.65%; precision:  52.78%; recall:  54.13%; FB1:  53.45
                  LOC: precision:  59.72%; recall:  78.45%; FB1:  67.81  13984
                 MISC: precision:  25.63%; recall:  39.59%; FB1:  31.12  7818
                  ORG: precision:  47.68%; recall:  14.32%; FB1:  22.02  2800
                  PER: precision:  64.17%; recall:  72.62%; FB1:  68.13  11384


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
