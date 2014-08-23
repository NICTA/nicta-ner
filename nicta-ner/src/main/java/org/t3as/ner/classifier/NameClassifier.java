/*
 * #%L
 * NICTA t3as Named-Entity Recognition library
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

import org.t3as.ner.EntityType;
import org.t3as.ner.NerResultSet;
import org.t3as.ner.Phrase;
import org.t3as.ner.classifier.feature.FeatureMap;
import org.t3as.ner.resource.Configuration;

import java.util.ArrayList;
import java.util.Collection;
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
    public Collection<String> trace;

    public NameClassifier(final Configuration _conf) { conf = _conf; }

    /**
     * This method process the whole input result set and gives all _phrases in the set a name type - modifies the data
     * of the passed in NerResultSet!
     * @param resultSet set to process - modifies the data of the result set!
     */
    public void process(final NerResultSet resultSet) {
        final FeatureMap featureMap = conf.getFeatureMap();
        if (conf.tracing) trace = new ArrayList<>();

        // store the relationship of _phrases in memory for further use
        final Map<Phrase, Set<Phrase>> phraseInMemory = new HashMap<>();

        // scoring
        for (final List<Phrase> phrasesInSentence : resultSet.phrases) {
            // for each sentence
            for (final Phrase phrase : phrasesInSentence) {
                // for each phrase in the sentence
                if (phrase.isDate) continue;

                // score the phrase
                phrase.score = featureMap.score(phrase);
                if (conf.tracing) {
                    trace.addAll(featureMap.trace);
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
            final Map<EntityType, Double> score = inMemoryPhrase.getKey().score;
            for (final Phrase phrase : aSet) {
                phrase.classify();
                if (EntityType.UNKNOWN.equals(phrase.phraseType)) {
                    phrase.score = score;
                    phrase.classify();
                }
            }
        }
    }
}
