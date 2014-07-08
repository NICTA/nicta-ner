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

import static java.lang.Character.isUpperCase;

public final class Strings {

    private Strings() {}

    public static boolean equalss(final String word, final String... words) {
        if (word == null || words == null) return false;
        for (final String s : words) {
            if (word.equals(s)) return true;
        }
        return false;
    }

    public static boolean equalsIgnoreCase(final String word, final String... words) {
        if (word == null || words == null) return false;
        for (final String s : words) {
            if (word.equalsIgnoreCase(s)) return true;
        }
        return false;
    }

    public static boolean startsWith(final String word, final String... prefixes) {
        if (word == null || prefixes == null) return false;
        for (final String s : prefixes) {
            if (word.startsWith(s)) return true;
        }
        return false;
    }

    public static boolean endsWith(final String word, final String... suffixes) {
        if (word == null || suffixes == null) return false;
        for (final String s : suffixes) {
            if (word.endsWith(s)) return true;
        }
        return false;
    }

    public static boolean isSingleUppercaseChar(final String word) {
        return word.length() == 1 && isUpperCase(word.charAt(0));
    }

    public static char initChar(final String s) {
        return s.charAt(0);
    }

    public static char lastChar(final String s) {
        return s.charAt(s.length() -1);
    }
}
