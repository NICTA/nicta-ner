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
 * Authors: Kishor Gawande
 * Created: --
 * Last Updated: --
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