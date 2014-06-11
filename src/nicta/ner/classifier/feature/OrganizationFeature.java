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

package nicta.ner.classifier.feature;

import nicta.ner.data.Phrase;
import nicta.ner.util.Dictionary;

public class OrganizationFeature extends Feature {
	
	Dictionary dict = null;

	public OrganizationFeature() {
		dict = Dictionary.getSharedDictionary();
	}
	
	public double score(Phrase _p) {
		for(String word : _p.phrase) {
			if(hasMultiUppercase(word)) {
				return 1.0f;
			}
		}
		return 0;
	}
	
	private boolean hasMultiUppercase(String _word) {
		if(_word.length() <= 1) return false;
		int capCharCount = 0;
		for(int i = 0; i < _word.length(); i++) {
			if(Character.isUpperCase(_word.charAt(i))) capCharCount++;
			if(capCharCount == 2) return true;
		}
		return false;

	}
	
	/*
	private String de_plural(String _word) {
		String word = "";
		if(_word.endsWith("ies")) {
			word = _word.substring(0, _word.length() - 3) + "y";
		} else if(_word.endsWith("ces") || _word.endsWith("ses")) {
			word = _word.substring(0, _word.length() - 2);
		} else if(_word.endsWith("s")) {
			word = _word.substring(0, _word.length() - 1);
		}
		return _word;
	}
	*/
}
