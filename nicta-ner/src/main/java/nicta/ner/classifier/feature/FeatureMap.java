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

import java.util.List;

public class FeatureMap {
	
	private final Feature[] feature_array;
	private final double[][] w;
	
	public FeatureMap(final List<Feature> features, final double[][] _w) {
		feature_array = features.toArray(new Feature[features.size()]);
		w = _w;
	}
	
	public double score(final Phrase _p, final int wi) {
		double score = 0.0f;
		for(int i = 0; i < feature_array.length; i++) {
			if(w[wi][i] == 0) continue;
			score += feature_array[i].score(_p) * w[wi][i];
		}
		return score;
	}
}
