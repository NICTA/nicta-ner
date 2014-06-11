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

import java.util.HashSet;

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
