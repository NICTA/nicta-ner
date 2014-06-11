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

import java.util.HashSet;

import nicta.ner.data.Phrase;

public class RuledWordFeature extends Feature {
	
	HashSet<String> wordSet = null;
	
	public RuledWordFeature(String filename) {
		wordSet = Feature.createSingleWordSet(filename, true);
	}
	
	@Override
	public double score(Phrase _p) {
		double score = 0.0f;
		double weight = 0.75f;	// weight increases 0.2 every word backward till the word "of" appears.
		for(int i = 0; i < _p.phrase.length; i++) {
			String word = _p.phrase[i];
			if(word.equalsIgnoreCase("of")) break;
			double x = (wordSet.contains(word)) ? 1.0f : 0.0f;
			score += weight * x;
			weight += 0.25;
		}
		return score;
	}

}
