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
import nicta.ner.resource.Configuration;

public class FeatureMap {
	
	Feature[] feature_array = null;
	double[][] w = null;
	
	public FeatureMap(Feature[] _fa, double[][] _w) {
		feature_array = _fa;
		w = _w;
	}
	
	public double score(Phrase _p, int wi) {
		double score = 0.0f;
		for(int i = 0; i < feature_array.length; i++) {
			if(w[wi][i] == 0) continue;
			score += feature_array[i].score(_p) * w[wi][i];
		}
		return score;
	}
}
