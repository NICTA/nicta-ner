/*
 * #%L
 * NICTA t3as NER CoNLL 2003 evaluation
 * %%
 * Copyright (C) 2014 NICTA
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
package org.t3as.ner.conll2003;

import org.t3as.ner.NerResultSet;
import org.t3as.ner.Phrase;
import org.t3as.ner.Token;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

public final class Util {

    private Util() {}

    public static Map<Integer, NerClassification> positionClassificationMap(final NerResultSet nerResultSet) {
        final Map<Integer, NerClassification> m = new HashMap<>();

        for (final List<Phrase> sentence : nerResultSet.phrases) {
            for (final Phrase p : sentence) {
                int stubPos = p.phraseStubPosition;
                for (final Token t : p.phrase) {
                    if (m.put(stubPos, new NerClassification(t.text, p.phraseType)) != null) {
                        throw new IllegalStateException("Tried to add a Token to the position classification map " +
                                                        "that is already there!");
                    }
                    stubPos++;
                }
            }
        }

        return m;
    }

    public static String translateClassification(final NerClassification nerClas, final NerClassification previous) {
        if (nerClas == null) return "O";

        final String prefix = previous == null ? "I-" : "B-";

        switch (nerClas.type) {
            case LOCATION:
                return prefix + "LOC";
            case ORGANIZATION:
                return prefix + "ORG";
            case PERSON:
                return prefix + "PER";
            case UNKNOWN:
                return prefix + "MISC";

            // DATE and anything else new we do should return null, since CoNLL only do the 4 types above
            case DATE:
            default:
                //noinspection ReturnOfNull
                return null;
        }
    }
}
