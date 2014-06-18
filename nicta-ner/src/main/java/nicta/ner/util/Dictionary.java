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
package nicta.ner.util;

import java.io.IOException;
import java.util.Map;

public final class Dictionary {

    private static final Map<String, String> dict;

    static {
        try { dict = IO.dictionary(Dictionary.class, "DICT"); }
        catch (final IOException e) { throw new RuntimeException(e); }
    }

    /**
     * Private constructor which creates the only dictionary instance.
     * Call getDict() method to get the shared dictionary.
     */
    private Dictionary() {}

    public static String checkup(final String word) {
        return dict.get(word);
    }

    /** This method checks if the word is a plural form. */
    public static boolean isPlural(final String _word) {
        final String word = _word.toLowerCase();

        // word + s
        if (word.endsWith("s")) {
            final String wordStub = word.substring(0, word.length() - 1);
            if (checkup(wordStub) != null) return true;
        }

        // word + ed
        if (word.endsWith("ed")) {
            final String wordStub = word.substring(0, word.length() - 2);
            if (checkup(wordStub) != null) return true;
        }

        // word(-y) + ied
        if (word.endsWith("ied")) {
            //noinspection StringConcatenationMissingWhitespace
            final String wordStub = word.substring(0, word.length() - 3) + "y";
            if (checkup(wordStub) != null) return true;
        }

        return false;
    }

    /** This method checks if the word is a past tense word. */
    public static boolean isPastTense(final String _word) {
        final String word = _word.toLowerCase();

        // word(e) + d
        if (word.endsWith("d")) {
            final String wordStub = word.substring(0, word.length() - 1);
            if (checkup(wordStub) != null) return true;
        }

        // word + ed
        if (word.endsWith("ces") || word.endsWith("ses")) {
            final String wordStub = word.substring(0, word.length() - 2);
            if (checkup(wordStub) != null) return true;
        }

        // word(-y) + ies
        if (word.endsWith("ies")) {
            //noinspection StringConcatenationMissingWhitespace
            final String wordStub = word.substring(0, word.length() - 3) + "y";
            if (checkup(wordStub) != null) return true;
        }

        return false;
    }
}
