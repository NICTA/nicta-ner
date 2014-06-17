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

import java.util.HashMap;
import java.util.List;

/**
 * Instance of this class indicates a Phrase.
 * A phrase can have one or more than one word.
 * @author William Han
 */
public class Phrase {
    /** phrase string array */
    public String[] phrase = null;
    /** corresponding name type */
    public NameType phraseType;
    /** the start position of the phase in a sentence */
    public int phrasePosition;
    /** the length of the phrase */
    public int phraseLength;
    /** the start position of the phrase stub in a sentence */
    public int phraseStubPosition;
    /** the length of the phrase stub */
    public int phraseStubLength;
    /** score array, dimension equals to name type array */
    public double[] score;
    /** attached word map */
    public HashMap<String, String> attachedWordMap = null;
    /** true if the phrase is a date; false if not */
    public boolean isDate = false;

    /**
     * Constructor
     */
    public Phrase() {
        phrasePosition = 0;
        phraseLength = 0;
        phrase = null;
        phraseType = NameType.NULL_TYPE;
        phraseStubPosition = 0;
        phraseStubLength = 0;
    }

    ;

    /**
     * Constructor with param input
     * @param _phrase
     * @param _phrasePos
     * @param _phraseLen
     * @param _stubPos
     * @param _typeDimension
     */
    public Phrase(String[] _phrase, int _phrasePos, int _phraseLen, int _stubPos, int _typeDimension) {
        phrasePosition = _phrasePos;
        phraseLength = _phraseLen;
        phrase = _phrase;
        phraseType = NameType.NULL_TYPE;
        score = new double[_typeDimension];
        attachedWordMap = new HashMap<String, String>();
        phraseStubPosition = _stubPos;
        phraseStubLength = phrase.length;
    }

    /**
     * returns a standard string
     */
    public String toString() {
        StringBuilder sb = new StringBuilder();
        for (int i = 0; i < phrase.length; i++) {
            sb.append(phrase[i]).append(" ");
        }
        return sb.toString().trim();
    }

    /**
     * Test if the phrase is a sub phrase of the input phrase.
     * @param _p
     * @return
     */
    public boolean isSubPhraseOf(Phrase _p) {
        int len_this = phrase.length;
        if (len_this == 0) return false;
        int len_targ = _p.phrase.length;
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

    /**
     * This method will do the classification.
     * @param _nta
     */
    public void classify(List<NameType> _nta) {
        int argmaxIndex = 0;
        double argmaxValue = this.score[argmaxIndex];
        boolean ambious = false;
        for (int scoreIndex = 1; scoreIndex < _nta.size(); scoreIndex++) {
            if (this.score[scoreIndex] > argmaxValue) {
                argmaxValue = this.score[scoreIndex];
                argmaxIndex = scoreIndex;
                ambious = false;
            }
            else if (this.score[scoreIndex] == argmaxValue) {
                ambious = true;
            }
        }
        if (!ambious)
            this.phraseType = _nta.get(argmaxIndex);
        else
            this.phraseType = NameType.NULL_TYPE;
    }
}

