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

package nicta.ner;

import java.util.ArrayList;
import java.util.HashMap;

import nicta.ner.data.Phrase;

/**
 * This class encapsulated the two result set used in the NameExtractor class
 * into one single class.
 * 
 * The map result can be got from this class as well.
 * 
 * @author William Han
 *
 */
public class NERResultSet {
	public ArrayList<ArrayList<String>> tokens;
	public ArrayList<ArrayList<Phrase>> phrases;
	
	private HashMap<String, String> _result = null;
	
	/**
	 * This method returns a map format set of the result.
	 * @return
	 */
	public HashMap<String, String> getMappedResult() {
		if(_result != null) return _result;
		_result = new HashMap<String, String>();
		for(ArrayList<Phrase> pa : phrases) {
			for(Phrase p : pa) {
				String ps = p.toString();
				_result.put(ps, p.phraseType.toString());
			}
		}
		return _result;
	}
	
	public String toString() {
		return NERResultSet.resultToString(this);
	}
	
	/**
	 * Convert the result set into a standard String output.
	 * 
	 * @param _ner
	 * @return
	 */
	public static String resultToString(NERResultSet _ner) {
		StringBuilder sb = new StringBuilder();
		
		for(int si = 0; si < _ner.tokens.size(); si++) {
			ArrayList<String> sentence = _ner.tokens.get(si);
			ArrayList<Phrase> phrases = _ner.phrases.get(si);
			for(int wi = 0; wi < sentence.size(); wi++)
				sb.append(sentence.get(wi) + " ");
			sb.append("\n===============================================\n");
			for(int pi = 0; pi < phrases.size(); pi++) {
				Phrase np = phrases.get(pi);
				String ptext = "";
				for(int wi = 0; wi < phrases.get(pi).phrase.length; wi++) {
					ptext += (phrases.get(pi).phrase[wi] + " ");
				}
				ptext = ptext.trim();
				
				///*
				String stext = "";
				for(int sci = 0; sci < np.score.length; sci ++) {
					if(sci != 0) stext += ", ";
					stext += np.score[sci];
				}
				sb.append(np.phrasePosition + ": " + ptext + "\t" + np.phraseType.toString() + "\t"
						+ stext + "\t"
						+ np.attachedWordMap.get("prep") + "\t"
						+ np.phrasePosition + ":" + np.phraseStubPosition + ":" + np.phraseStubLength + ":" + np.phraseLength
						+ "\n");
				//*/
				//sb.append(ptext + "=" + np.phraseType + "\n");
			}
			sb.append("\n\n");
		}
		return sb.toString();
	}
}
