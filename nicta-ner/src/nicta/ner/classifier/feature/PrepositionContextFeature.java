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

public class PrepositionContextFeature extends Feature {
	
	HashSet<String> wordSet = null;
	
	public PrepositionContextFeature(String filename) {
		wordSet = Feature.createSingleWordSet(filename, false);
	}

	@Override
	public double score(Phrase _p) {
		return wordSet.contains(_p.attachedWordMap.get("prep")) ? 1.0f : 0.0f;
	}
}
