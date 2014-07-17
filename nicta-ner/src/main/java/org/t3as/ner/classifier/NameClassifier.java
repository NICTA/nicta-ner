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
package org.t3as.ner.classifier;

import org.t3as.ner.NerResultSet;
import org.t3as.ner.classifier.feature.FeatureMap;
import org.t3as.ner.data.NameType;
import org.t3as.ner.data.Phrase;
import org.t3as.ner.resource.Configuration;

import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

/**
 * The Classifier get the phrase input and classifies the phrase
 * according to the features and the weight array.
 */
public class NameClassifier {

    private final Configuration conf;

    public NameClassifier(final Configuration _conf) { conf = _conf; }

    /**
     * This method process the whole input result set and gives all _phrases in the set a name type - modifies the data
     * of the passed in NerResultSet!
     * @param resultSet set to process - modifies the data of the result set!
     */
    public void process(final NerResultSet resultSet) {
        final FeatureMap featureMap = conf.getFeatureMap();
        final List<NameType> nameTypes = conf.getNameTypes();

        // store the relationship of _phrases in memory for further use
        final Map<Phrase, Set<Phrase>> phraseInMemory = new HashMap<>();

        // scoring
        for (final List<Phrase> phrasesInSentence : resultSet.phrases) {
            // for each sentence
            for (final Phrase phrase : phrasesInSentence) {
                // for each phrase in the sentence
                if (phrase.isDate) continue;

                for (int scoreIndex = 0; scoreIndex < nameTypes.size(); scoreIndex++) {
                    // score all the dimensions
                    phrase.score[scoreIndex] = featureMap.score(phrase, scoreIndex);
                }

                boolean isSubPhrase = false;
                for (final Map.Entry<Phrase, Set<Phrase>> inMemoryEntry : phraseInMemory.entrySet()) {
                    if (phrase.isSubPhraseOf(inMemoryEntry.getKey())) {
                        inMemoryEntry.getValue().add(phrase);
                        isSubPhrase = true;
                        break;
                    }
                }
                if (!isSubPhrase || phrase.phrase.size() > 1) {
                    final Set<Phrase> newSet = new HashSet<>();
                    newSet.add(phrase);
                    phraseInMemory.put(phrase, newSet);
                }
            }
        }

        // copy the score of _phrases that have relationships
        for (final Map.Entry<Phrase, Set<Phrase>> inMemoryPhrase : phraseInMemory.entrySet()) {
            final Set<Phrase> aSet = inMemoryPhrase.getValue();
            final double[] score = inMemoryPhrase.getKey().score;
            for (final Phrase phrase : aSet) {
                phrase.classify(nameTypes);
                if (phrase.phraseType == NameType.UNKNOWN) {
                    phrase.score = score;
                    phrase.classify(nameTypes);
                }
            }
        }
    }

}
