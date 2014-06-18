/*
 * #%L
 * NICTA Named Entity Recogniser library
 * %%
 * Copyright (C) 2010 - 2014 NICTA
 * %%
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as
 * published by the Free Software Foundation, either version 3 of the
 * License, or (at your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public
 * License along with this program.  If not, see
 * <http://www.gnu.org/licenses/gpl-3.0.html>.
 * #L%
 */
package nicta.ner.classifier;

import nicta.ner.NERResultSet;
import nicta.ner.classifier.feature.FeatureMap;
import nicta.ner.data.NameType;
import nicta.ner.data.Phrase;
import nicta.ner.resource.Configuration;

import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;

/**
 * The Classifier get the phrase input and classifies the phrase
 * according to the features and the weight array.
 * @author William Han
 */
public class NameClassifier {

    /** contains feature and name_type information */
    Configuration _config = null;

    /** feature map */
    FeatureMap _fm = null;

    /**
     * Constructor builds the classifier instance.
     * @param _conf
     */
    public NameClassifier(Configuration _conf) {
        _config = _conf;
        _fm = _config.getFeatureMap();
    }

    /**
     * This method process the whole input result set
     * and gives all _phrases in the set a name type.
     * @param resultSet
     * @return
     */
    public NERResultSet process(NERResultSet resultSet) {
        // store the relationship of _phrases in memory for further use
        Map<Phrase, HashSet<Phrase>> phraseInMemory =
                new HashMap<Phrase, HashSet<Phrase>>();

        // scoring
        List<List<Phrase>> _phrases = resultSet.phrases;
        for (int si = 0; si < _phrases.size(); si++) {
            // for each sentence
            List<Phrase> phrasesInSentence = _phrases.get(si);
            for (int pi = 0; pi < phrasesInSentence.size(); pi++) {
                // for each phrase in the sentence
                Phrase p = phrasesInSentence.get(pi);
                if (p.isDate) continue;
                for (int scoreIndex = 0; scoreIndex < _config.getNameTypes().size(); scoreIndex++) {
                    // score all the dimensions
                    p.score[scoreIndex] = _fm.score(p, scoreIndex);
                }

                boolean flag = false;
                for (Phrase keyPhrase : phraseInMemory.keySet()) {
                    if (p.isSubPhraseOf(keyPhrase)) {
                        phraseInMemory.get(keyPhrase).add(p);
                        flag = true;
                        break;
                    }
                }
                if (!flag || p.phrase.length > 1) {
                    HashSet<Phrase> newSet = new HashSet<Phrase>();
                    newSet.add(p);
                    phraseInMemory.put(p, newSet);
                }
            }
        }

        // copy the score of _phrases that have relationships:
        for (Phrase key : phraseInMemory.keySet()) {
            HashSet<Phrase> aSet = phraseInMemory.get(key);
            /*
            double[] score = new double[config.name_type.length];
            for (Phrase phrase : aSet) {
                for (int i = 0; i < config.name_type.length; i++)
                    score[i] += phrase.score[i];
                phrase.score = score;
            }
            */
            double[] score = key.score;
            for (Phrase phrase : aSet) {
                phrase.classify(_config.getNameTypes());
                if (phrase.phraseType == NameType.NULL_TYPE) {
                    phrase.score = score;
                    phrase.classify(_config.getNameTypes());
                }
            }
            for (Phrase phrase : aSet) {

            }
        }
        return resultSet;
    }

}
