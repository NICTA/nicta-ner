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

/**
 * This class is a name type class.
 * 
 * Each instance of this class represents a name type
 * such as:
 * ORGANIZATION, LOCATION, PERSON, DATE...
 * 
 * @author William Han
 *
 */
public class NameType {
	public String typeName = "";
	
	public static NameType NULL_TYPE = new NameType("UNKNOWN");
	public static NameType DATE_TYPE = new NameType("DATE");
	
	public NameType(String _name) {
		typeName = _name;
	}
	
	public String toString() {
		return typeName;
	}
}
