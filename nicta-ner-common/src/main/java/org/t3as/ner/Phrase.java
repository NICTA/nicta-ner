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
package org.t3as.ner;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.google.common.collect.ImmutableList;

import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import static org.t3as.ner.EntityType.UNKNOWN;

/**
 * Instance of this class indicates a Phrase.
 * A phrase can have one or more than one word.
 */
public class Phrase {
    // TODO: getters and setters for these
    /** phrase text array */
    public final List<Token> phrase;
    /** corresponding name type */
    public EntityType phraseType;
    /** the start position of the phase in a sentence */
    public final int phrasePosition;
    /** the length of the phrase */
    public final int phraseLength;
    /** the start position of the phrase stub in a sentence */
    public final int phraseStubPosition;
    /** the length of the phrase stub */
    public final int phraseStubLength;
    /** score array, dimension equals to name type array */
    public Map<EntityType, Double> score = new LinkedHashMap<>();
    /** attached word map */
    public Map<String, String> attachedWordMap;
    /** true if the phrase is a date; false if not */
    public boolean isDate;

    @JsonCreator
    public Phrase(@JsonProperty("phrase") final List<Token> tokens,
                  @JsonProperty("phrasePosition") final int _phrasePos,
                  @JsonProperty("phraseLength") final int _phraseLen,
                  @JsonProperty("phraseStubPosition") final int _stubPos,
                  @JsonProperty("phraseType") final EntityType type) {
        phrasePosition = _phrasePos;
        phraseLength = _phraseLen;
        phrase = ImmutableList.copyOf(tokens);
        phraseType = type;
        attachedWordMap = new HashMap<>();
        phraseStubPosition = _stubPos;
        phraseStubLength = phrase.size();
    }

    public String phraseString() {
        final StringBuilder sb = new StringBuilder();
        for (final Token aPhrase : phrase) sb.append(aPhrase.text).append(" ");
        return sb.toString().trim();
    }

    /** Test if the phrase is a sub phrase of the input phrase. */
    public boolean isSubPhraseOf(final Phrase other) {
        if (phrase.isEmpty()) return false;

        // TODO: this should be refactored - the intent is not clear, implementation is sketchy
        boolean is = false;
        for (int i = 0; i < other.phrase.size() - phrase.size() + 1; i++) {
            boolean flag = true;
            for (int j = 0; j < phrase.size(); j++) {
                if (!phrase.get(j).text.equalsIgnoreCase(other.phrase.get(i + j).text)) {
                    flag = false;
                    break;
                }
            }
            if (flag) {
                is = true;
                break;
            }
        }
        return is;
    }

    /** This method will do the classification of a Phrase with a EntityType. */
    public void classify() {
        EntityType type = null;
        double s = 0;
        boolean ambiguous = false;
        for (final Map.Entry<EntityType, Double> e : score.entrySet()) {
            if (type == null) {
                type = e.getKey();
                s = e.getValue();
            }
            else {
                if (Double.compare(e.getValue(), s) > 0) {
                    type = e.getKey();
                    s = e.getValue();
                    ambiguous = false;
                }
                else if (Double.compare(s, e.getValue()) == 0) {
                    ambiguous = true;
                }
            }
        }
        this.phraseType = ambiguous ? UNKNOWN : type;
    }

    @Override
    public String toString() {
        return "Phrase{" +
               "phrase=" + phrase +
               ", phraseType=" + phraseType +
               ", phrasePosition=" + phrasePosition +
               ", phraseLength=" + phraseLength +
               ", phraseStubPosition=" + phraseStubPosition +
               ", phraseStubLength=" + phraseStubLength +
               ", score=" + score.toString() +
               ", attachedWordMap=" + attachedWordMap +
               ", isDate=" + isDate +
               '}';
    }
}

