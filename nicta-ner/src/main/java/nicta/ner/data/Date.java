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

import java.util.List;
import java.util.regex.Pattern;

public class Date extends Phrase {

    private static final Pattern COLON = Pattern.compile(":");

    private static final int DATE_DD = 0;
    private static final int DATE_MM = 1;
    private static final int DATE_YY = 2;
    //private static final int DATE_YEAR_AD_BC = 5;
    private static final int DATE_WEEKDAY = 8;
    private static final int DATE = 7; // satisfies Kishor's DateMatcher

    private static final int TIME = 3;
    private static final int TIME_AM_PM = 4;

    private static final int MAX_MONTH_DAYS = 31;
    private static final int MIN_YEAR = 1900;
    private static final int MAX_YEAR = 2200;

    private static final String[] STRING_MONTHS = {
            "January", "February", "March",
            "April", "May", "June",
            "July", "August", "September",
            "October", "November", "December"
    };

    private static final String[] STRING_WEEKDAYS = {
            "Sunday", "Monday", "Tuesday", "Wednesday",
            "Thursday", "Friday", "Saturday"
    };

    public Date(final List<String> phrase, final int pos, final int len, final int stubPos, final int typeDimension) {
        super(phrase, pos, len, stubPos, typeDimension);
        this.phraseType = NameType.DATE_TYPE;
        this.isDate = true;
    }

    public static int getDateType(final String _word) {
        // TIME_AM_PM
        final String clean_word = _word.replace(".", "");
        if ("am".equalsIgnoreCase(clean_word) || "pm".equalsIgnoreCase(clean_word))
            return TIME_AM_PM;
        // YEAR_AD_BC
        if ("ad".equalsIgnoreCase(clean_word) || "bc".equalsIgnoreCase(clean_word))
            return TIME_AM_PM;

        // YY
        try {
            final int year = Integer.parseInt(_word);
            if (year > MIN_YEAR && year < MAX_YEAR) return DATE_YY;
        }
        catch (final NumberFormatException ignored) {}

        // MM
        for (final String month_word : STRING_MONTHS)
            if (month_word.equals(_word)) return DATE_MM;

        // DD
        try {
            final int dd = Integer.parseInt(_word.substring(0, _word.length() - 2));
            if (_word.equalsIgnoreCase("1st")
                || _word.equalsIgnoreCase("21st")
                || _word.equalsIgnoreCase("31st")
                || _word.equalsIgnoreCase("2nd")
                || _word.equalsIgnoreCase("22nd")
                || _word.equalsIgnoreCase("3rd")
                || _word.equalsIgnoreCase("23rd")
                || (_word.toLowerCase().endsWith("th")
                    && dd > 0
                    && dd <= 31))
                return DATE_DD;
        }
        catch (StringIndexOutOfBoundsException | NumberFormatException ignored) {}
        try {
            final int dd = Integer.parseInt(_word);
            if (dd > 0 && dd <= MAX_MONTH_DAYS) return DATE_DD;
        }
        catch (StringIndexOutOfBoundsException | NumberFormatException ignored) {}

        // WEEKDAY
        for (final String day_word : STRING_WEEKDAYS)
            if (day_word.equals(_word)) return DATE_WEEKDAY;

        // DATE_TIME
        try {
            final String[] time_array = COLON.split(_word);
            Integer h = null, m = null, s = null;
            if (time_array.length >= 2) {
                h = Integer.parseInt(time_array[0]);
                m = Integer.parseInt(time_array[1]);
            }
            if (time_array.length == 3) {
                s = Integer.parseInt(time_array[2]);
            }
            if (h >= 0
                && m >= 0
                && m < 60
                && (s == null
                    || (s > 0
                        && s < 60)))
                return TIME;
        }
        catch (NumberFormatException | NullPointerException ignored) {}

        // Kishor's DateMatcher
        final DateMatcher dm = new DateMatcher();
        if (dm.isDate(_word)) return DATE;

        return -1;
    }
}
