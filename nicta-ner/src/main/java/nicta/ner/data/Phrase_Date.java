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

public class Phrase_Date extends Phrase {

    public static int DATE_DD = 0;
    public static int DATE_MM = 1;
    public static int DATE_YY = 2;
    public static int DATE_YEAR_AD_BC = 5;
    public static int DATE_WEEKDAY = 8;
    public static int DATE = 7; // satisfies Kishor's DateMatcher

    public static int TIME = 3;
    public static int TIME_AM_PM = 4;

    private boolean yyHaveValue = false;
    private boolean ddHaveValue = false;
    private boolean mmHaveValue = false;

    private boolean timeHaveValue = false;

    public static String[] STRING_MONTHS = {
            "January", "February", "March",
            "April", "May", "June",
            "July", "August", "September",
            "October", "November", "December"
    };

    public static String[] STRING_WEEKDAYS = {
            "Sunday", "Monday", "Tuesday", "Wednesday",
            "Thursday", "Friday", "Saturday"
    };

    public Phrase_Date() {
        super();
        this.phraseType = NameType.DATE_TYPE;
        this.isDate = true;
    }

    public Phrase_Date(String[] _phrase, int _phrasePos, int _phraseLen, int _stubPos, int _typeDimension) {
        super(_phrase, _phrasePos, _phraseLen, _stubPos, _typeDimension);
        this.phraseType = NameType.DATE_TYPE;
        this.isDate = true;
    }

    public static int getDateType(String _word) {
        int returnValue = -1;

        // TIME_AM_PM
        String clean_word = _word.replace(".", "");
        if (clean_word.equalsIgnoreCase("am") || clean_word.equalsIgnoreCase("pm"))
            return TIME_AM_PM;
        // YEAR_AD_BC
        if (clean_word.equalsIgnoreCase("ad") || clean_word.equalsIgnoreCase("bc"))
            return TIME_AM_PM;

        // YY
        try {
            int year = Integer.parseInt(_word);
            if (year > 1900 && year < 2200) return DATE_YY;
        }
        catch (NumberFormatException e0) {}

        // MM
        for (String month_word : STRING_MONTHS)
            if (_word.equals(month_word)) return DATE_MM;

        // DD
        try {
            int dd = Integer.parseInt(_word.substring(0, _word.length() - 2));
            if (
                    _word.equalsIgnoreCase("1st") ||
                    _word.equalsIgnoreCase("21st") ||
                    _word.equalsIgnoreCase("31st") ||
                    _word.equalsIgnoreCase("2nd") ||
                    _word.equalsIgnoreCase("22nd") ||
                    _word.equalsIgnoreCase("3rd") ||
                    _word.equalsIgnoreCase("23rd") ||
                    (_word.toLowerCase().endsWith("th") && dd > 0 && dd <= 31)
                    )
                return DATE_DD;
        }
        catch (StringIndexOutOfBoundsException e0) {
        }
        catch (NumberFormatException e1) {}
        try {
            int dd = Integer.parseInt(_word);
            if (dd > 0 && dd <= 31) return DATE_DD;
        }
        catch (StringIndexOutOfBoundsException e0) {
        }
        catch (NumberFormatException e1) {}

        // WEEKDAY
        for (String month_word : STRING_WEEKDAYS)
            if (_word.equals(month_word)) return DATE_WEEKDAY;

        // DATE_TIME
        try {
            String[] time_array = _word.split(":");
            Integer h = null, m = null, s = null;
            if (time_array.length >= 2) {
                h = Integer.parseInt(time_array[0]);
                m = Integer.parseInt(time_array[1]);
            }
            if (time_array.length == 3) {
                s = Integer.parseInt(time_array[2]);
            }
            if (h >= 0 && m >= 0 && m < 60 && (s == null || (s > 0 && s < 60)))
                return TIME;
        }
        catch (NumberFormatException e0) {
        }
        catch (NullPointerException e1) {
        }

        // Kishor's DateMatcher
        DateMatcher dm = new DateMatcher();
        if (dm.isDate(_word)) return DATE;

        return returnValue;
    }
}
