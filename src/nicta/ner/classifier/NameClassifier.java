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

package nicta.ner.classifier;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;


import nicta.ner.NERResultSet;
import nicta.ner.classifier.feature.FeatureMap;
import nicta.ner.data.NameType;
import nicta.ner.data.Phrase;
import nicta.ner.resource.Configuration;

/**
 * The Classifier get the phrase input and classifies the phrase
 * according to the features and the weight array.
 * 
 * 
 * 
 * @author William Han
 *
 */
public class NameClassifier {
	
	/** contains feature and name_type information */
	Configuration _config = null;
	
	/** feature map */
	FeatureMap _fm = null;
	
	/**
	 * Constructor builds the classifier instance.
	 * 
	 * @param _conf
	 */
	public NameClassifier(Configuration _conf) {
		_config = _conf;
		_fm = _config.feature_map;
	}
	
	/**
	 * This method process the whole input result set
	 * and gives all _phrases in the set a name type.
	 * 
	 * @param resultSet
	 * @return
	 */
	public NERResultSet process(NERResultSet resultSet) {
		// store the relationship of _phrases in memory for further use
		HashMap<Phrase, HashSet<Phrase>> phraseInMemory =
			new HashMap<Phrase, HashSet<Phrase>>();
		
		// scoring
		ArrayList<ArrayList<Phrase>> _phrases = resultSet.phrases;
		for(int si = 0; si < _phrases.size(); si++) {
			// for each sentence
			ArrayList<Phrase> phrasesInSentence = _phrases.get(si);
			for(int pi = 0; pi < phrasesInSentence.size(); pi++) {
				// for each phrase in the sentence
				Phrase p = phrasesInSentence.get(pi);
				if(p.isDate) continue;
				for(int scoreIndex = 0; scoreIndex < _config.name_type.length; scoreIndex++) {
					// score all the dimensions
					p.score[scoreIndex] = _fm.score(p, scoreIndex);
				}
				
				boolean flag = false;
				for(Phrase keyPhrase : phraseInMemory.keySet()) {
					if(p.isSubPhraseOf(keyPhrase)) {
						phraseInMemory.get(keyPhrase).add(p);
						flag = true;
						break;
					}
				}
				if(!flag || p.phrase.length > 1) {
					HashSet<Phrase> newSet = new HashSet<Phrase>();
					newSet.add(p);
					phraseInMemory.put(p, newSet);
				}
			}
		}
		
		// copy the score of _phrases that have relationships:
		for(Phrase key : phraseInMemory.keySet()) {
			HashSet<Phrase> aSet = phraseInMemory.get(key);
			/*
			double[] score = new double[config.name_type.length];
			for(Phrase phrase : aSet) {
				for(int i = 0; i < config.name_type.length; i++)
					score[i] += phrase.score[i];
				phrase.score = score;
			}
			*/
			double[] score = key.score;
			for(Phrase phrase : aSet) {
				phrase.classify(_config.name_type);
				if(phrase.phraseType == NameType.NULL_TYPE) {
					phrase.score = score;
					phrase.classify(_config.name_type);
				}
			}
			for(Phrase phrase : aSet) {
				
			}
		}
		return resultSet;
	}
	
}
