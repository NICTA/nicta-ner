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
package org.t3as.ner.data;

import com.google.common.collect.ImmutableCollection;
import com.google.common.collect.ImmutableList;

import java.util.regex.Pattern;

/**
 * @author kgawande
 */
@SuppressWarnings("HardcodedFileSeparator")
public final class DateMatcher {

    private static final ImmutableCollection<Pattern> PATTERNS;

    static {
        // TODO: rewrite regex date/time matching using some existing lib, maybe joda-time
        final String dd = "([1-9]|0[1-9]|[1-2][0-9]|3[01])";

        final String mm1 = "((January|February|March|April|May|June|July|August|September|October|November|December)" +
                           "|(Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sept|Oct|Nov|Dec))";

        final String mm = "(([1-9]|0[1-9]|1[0-2])|" + mm1 + ")";

        final String yyyy = "\\d{4}";

        final String sep = "(/|\\s|-|.)";

        PATTERNS = ImmutableList.of(
                // dd/mm/yyyy  or dd mm yyyy  or dd-mm-yyyy or dd January yyyy or dd Jan 2009
                Pattern.compile(".*" + dd + sep + mm + sep + yyyy + ".*"),

                // mm/dd/yyyy  or mm dd yyyy  or mm-dd-yyyy or January dd yyyy or Jan dd 2009
                Pattern.compile(".*" + mm + sep + dd + sep + yyyy + ".*"),

                // yyyy/mm/dd or 2009 January 1 or 2009 Jan 1
                Pattern.compile(".*" + yyyy + sep + mm + sep + dd + ".*"),

                //January 2, 2008 or January 2 2008
                Pattern.compile(".*" + mm1 + sep + dd + "(?:,)" + "\\s" + yyyy + ".*"),

                //2 January, 2008 or 2 January 2008
                Pattern.compile(".*" + dd + sep + mm1 + "(?:,)" + "\\s" + yyyy + ".*")
        );
    }

    private DateMatcher() {}

    public static boolean isDate(final String text) {
        for (final Pattern pattern : PATTERNS) {
            if (pattern.matcher(text).matches()) return true;
        }
        return false;
    }
}
