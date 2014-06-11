/* Copyright (c) 2010, National ICT Australia
 * All rights reserved.
 *
 * The contents of this file are subject to the Mozilla Public License Version
 * 1.1 (the 'License'); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 * http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an 'AS IS' basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
 * for the specific language governing rights and limitations under the
 * License.
 *
 * Authors: William Han (william.han@nicta.com.au)
 * Created: 2010-11-05
 * Last Updated: --
 */

package nicta.ner.data;

import java.util.HashSet;

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
		if(clean_word.equalsIgnoreCase("am") || clean_word.equalsIgnoreCase("pm"))
			return TIME_AM_PM;
		// YEAR_AD_BC
		if(clean_word.equalsIgnoreCase("ad") || clean_word.equalsIgnoreCase("bc"))
			return TIME_AM_PM;
		
		// YY
		try {
			int year = Integer.parseInt(_word);
			if(year > 1900 && year < 2200) return DATE_YY;
		} catch(NumberFormatException e0) {}
		
		// MM
		for(String month_word : STRING_MONTHS)
			if(_word.equals(month_word)) return DATE_MM;
		
		// DD
		try {
			int dd = Integer.parseInt(_word.substring(0, _word.length() - 2));
			if(
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
		} catch(StringIndexOutOfBoundsException e0) {
		} catch(NumberFormatException e1) {}
		try {
			int dd = Integer.parseInt(_word);
			if(dd > 0 && dd <= 31) return DATE_DD;
		} catch(StringIndexOutOfBoundsException e0) {
		} catch(NumberFormatException e1) {}
		
		// WEEKDAY
		for(String month_word : STRING_WEEKDAYS)
			if(_word.equals(month_word)) return DATE_WEEKDAY;
		
		// DATE_TIME
		try {
			String[] time_array = _word.split(":");
			Integer h = null, m = null, s = null;
			if(time_array.length >= 2) {
				h = Integer.parseInt(time_array[0]);
				m = Integer.parseInt(time_array[1]);
			}
			if(time_array.length == 3) {
				s = Integer.parseInt(time_array[2]);	
			}
			if(h >= 0 && m >= 0 && m < 60 && (s == null || (s > 0 && s < 60)))
				return TIME;
		} catch(NumberFormatException e0) {
		} catch(NullPointerException e1) {
		}
		
		// Kishor's DateMatcher
		DateMatcher dm = new DateMatcher();
		if(dm.isDate(_word)) return DATE;
		
		return returnValue;
	}
}
