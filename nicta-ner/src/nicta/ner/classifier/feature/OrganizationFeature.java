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
