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

import java.util.HashMap;
/**
 * Instance of this class indicates a Phrase.
 * A phrase can have one or more than one word.
 * 
 * @author William Han
 *
 */
public class Phrase {
	/** phrase string array */
	public String[] phrase = null;
	/** corresponding name type */
	public NameType phraseType;
	/** the start position of the phase in a sentence */
	public int phrasePosition;
	/** the length of the phrase */
	public int phraseLength;
	/** the start position of the phrase stub in a sentence */
	public int phraseStubPosition;
	/** the length of the phrase stub */
	public int phraseStubLength;
	/** score array, dimension equals to name type array */
	public double[] score;
	/** attached word map */
	public HashMap<String, String> attachedWordMap = null;
	/** true if the phrase is a date; false if not */
	public boolean isDate = false;
	
	/**
	 * Constructor
	 */
	public Phrase() {
		phrasePosition = 0;
		phraseLength = 0;
		phrase = null;
		phraseType = NameType.NULL_TYPE;
		phraseStubPosition = 0;
		phraseStubLength = 0;
	};
	
	/**
	 * Constructor with param input
	 * 
	 * @param _phrase
	 * @param _phrasePos
	 * @param _phraseLen
	 * @param _stubPos
	 * @param _typeDimension
	 */
	public Phrase(String[] _phrase, int _phrasePos, int _phraseLen, int _stubPos, int _typeDimension) {
		phrasePosition = _phrasePos;
		phraseLength = _phraseLen;
		phrase = _phrase;
		phraseType = NameType.NULL_TYPE;
		score = new double[_typeDimension];
		attachedWordMap = new HashMap<String, String>();
		phraseStubPosition = _stubPos;
		phraseStubLength = phrase.length;
	}
	
	/**
	 * returns a standard string
	 */
	public String toString() {
		StringBuilder sb = new StringBuilder();
		for(int i = 0; i < phrase.length; i++) {
			sb.append(phrase[i]).append(" ");
		}
		return sb.toString().trim();
	}
	
	/**
	 * Test if the phrase is a sub phrase of the input phrase.
	 * 
	 * @param _p
	 * @return
	 */
	public boolean isSubPhraseOf(Phrase _p) {
		int len_this = phrase.length;
		if(len_this == 0) return false;
		int len_targ = _p.phrase.length;
		boolean is = false;
		for(int i = 0; i < len_targ - len_this + 1; i++) {
			boolean flag = true;
			for(int j = 0; j < len_this; j++) {
				if(!phrase[j].equalsIgnoreCase(_p.phrase[i + j])) {
					flag = false;
					break;
				}
			}
			if(flag) {
				is = true;
				break;
			}
		}
		return is;
	}
	
	/**
	 *  This method will do the classification.
	 */
	public void classify(NameType[] _nta) {
		int argmaxIndex = 0;
		double argmaxValue = this.score[argmaxIndex];
		boolean ambious = false;
		for(int scoreIndex = 1; scoreIndex < _nta.length; scoreIndex++) {
			if(this.score[scoreIndex] > argmaxValue) {
				argmaxValue = this.score[scoreIndex];
				argmaxIndex = scoreIndex;
				ambious = false;
			} else if(this.score[scoreIndex] == argmaxValue) {
				ambious = true;
			}
		}
		if(!ambious)
			this.phraseType = _nta[argmaxIndex];
		else
			this.phraseType = NameType.NULL_TYPE;
	}
}

