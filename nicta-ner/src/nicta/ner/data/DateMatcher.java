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

import java.util.regex.Pattern;
import java.util.regex.Matcher;

import java.util.List;
import java.util.Vector;


/**
 *
 * @author kgawande
 */
public class DateMatcher {

    private List<Pattern> patterns = new Vector<Pattern>();

    // Constructor - initalize all date patterns
    public DateMatcher() {
        Pattern p;

        String dd = "([1-9]|0[1-9]|[1-2][0-9]|3[01])";
        String dds = "((^[1-9]|0[1-9]|[1-2][0-9]|3[01]))";
        String mm = "(([1-9]|0[1-9]|1[0-2])|(January|February|March|April|May|June|July|August|September|October|November|December)|(Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sept|Oct|Nov|Dec))";
        String mms = "((^[1-9]|0[1-9]|1[0-2])|(January|February|March|April|May|June|July|August|September|October|November|December)|(Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sept|Oct|Nov|Dec))";
        String mm1 = "((January|February|March|April|May|June|July|August|September|October|November|December)|(Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sept|Oct|Nov|Dec))";
        String yyyy = "(\\d\\d\\d\\d)";
        String sep = "(/|\\s|-|.)";

        // dd/mm/yyyy  or dd mm yyyy  or dd-mm-yyyy or dd January yyyy or dd Jan 2009
        p = Pattern.compile(".*" + dd + sep + mm + sep + yyyy + ".*");
        patterns.add(p);

        // yyyy/mm/dd or 2009 January 1 or 2009 Jan 1
        p = Pattern.compile(".*" + yyyy + sep + mm + sep + dd + ".*");
        patterns.add(p);

        // mm dd yyyy
        p = Pattern.compile(".*" + mms + sep + dd + sep + yyyy + ".*");
        patterns.add(p);

        //January 2, 2008 or January 2 2008
        p = Pattern.compile(".*" + mm1 + "(\\s)" + dd + "(,\\s|\\s)" + yyyy + ".*");
        patterns.add(p);
        


    }

    public boolean isDate(String text) {
        for (Pattern pattern : patterns) {
            Matcher m = pattern.matcher(text);
            //System.out.println(pattern.pattern());
            if (m.matches()) {
                return true;
            }
        }
        return false;
    }
    
    public static void main(String[] args) {
    	DateMatcher dm = new DateMatcher();
    	System.out.println(dm.isDate("13/12/07"));
    }
}
