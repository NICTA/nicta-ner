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
package nicta.ner.data;

import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * Instance of this class indicates a Phrase.
 * A phrase can have one or more than one word.
 */
public class Phrase {
    // TODO: getters and setters for these, or investigate making the entire class immutable?
    /** phrase string array */
    public final String[] phrase;
    /** corresponding name type */
    public NameType phraseType;
    /** the start position of the phase in a sentence */
    public final int phrasePosition;
    /** the length of the phrase */
    public final int phraseLength;
    /** the start position of the phrase stub in a sentence */
    public final int phraseStubPosition;
    /** the length of the phrase stub */
    public final int phraseStubLength;
    /** score array, dimension equals to name type array */
    public double[] score;
    /** attached word map */
    public Map<String, String> attachedWordMap;
    /** true if the phrase is a date; false if not */
    public boolean isDate;

    /** Constructor */
    public Phrase() {
        phrasePosition = 0;
        phraseLength = 0;
        phrase = new String[0];
        phraseType = NameType.NULL_TYPE;
        phraseStubPosition = 0;
        phraseStubLength = 0;
        attachedWordMap = Collections.emptyMap();
    }

    /** Constructor with param input */
    public Phrase(final List<String> _phrase, final int _phrasePos, final int _phraseLen, final int _stubPos,
                  final int _typeDimension) {
        phrasePosition = _phrasePos;
        phraseLength = _phraseLen;
        phrase = _phrase.toArray(new String[_phrase.size()]);
        phraseType = NameType.NULL_TYPE;
        score = new double[_typeDimension];
        attachedWordMap = new HashMap<>();
        phraseStubPosition = _stubPos;
        phraseStubLength = phrase.length;
    }

    public String phraseString() {
        final StringBuilder sb = new StringBuilder();
        for (final String aPhrase : phrase) sb.append(aPhrase).append(" ");
        return sb.toString().trim();
    }

    /** Test if the phrase is a sub phrase of the input phrase. */
    public boolean isSubPhraseOf(final Phrase _p) {
        final int len_this = phrase.length;
        if (len_this == 0) return false;
        final int len_targ = _p.phrase.length;
        boolean is = false;
        for (int i = 0; i < len_targ - len_this + 1; i++) {
            boolean flag = true;
            for (int j = 0; j < len_this; j++) {
                if (!phrase[j].equalsIgnoreCase(_p.phrase[i + j])) {
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

    /** This method will do the classification. */
    public void classify(final List<NameType> _nta) {
        int argmaxIndex = 0;
        double argmaxValue = this.score[argmaxIndex];
        boolean ambious = false;
        for (int scoreIndex = 1; scoreIndex < _nta.size(); scoreIndex++) {
            if (this.score[scoreIndex] > argmaxValue) {
                argmaxValue = this.score[scoreIndex];
                argmaxIndex = scoreIndex;
                ambious = false;
            }
            else if (Double.compare(this.score[scoreIndex], argmaxValue) == 0) {
                ambious = true;
            }
        }
        this.phraseType = ambious ? NameType.NULL_TYPE : _nta.get(argmaxIndex);
    }
}

