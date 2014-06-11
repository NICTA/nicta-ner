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

package nicta.ner.util;

import java.util.*;
import java.text.*;
import java.io.*;

/**
 * This class utilizes a Java standard class to token the input sentence.
 * 
 * @author William Han
 *
 */
public class JTokenizer {
	
	private static InputStream ABBREVIATION_EXCEPTION = null;
	
	private static int _MODE_WITH_PUNCTUATE = 0;
	private static int _MODE_WITHOUT_PUNCTUATE = 1;
	
	private int mode = _MODE_WITH_PUNCTUATE;
	
	private HashSet<String> _abbreException = null;
	
	public enum TOKENIZER_MODE {
		WITH_PUNCTUATE,
		WITHOUT_PUNCTUATE
	}
	
	public JTokenizer(TOKENIZER_MODE _m) throws Exception {
		if(_abbreException == null) {
			ABBREVIATION_EXCEPTION = this.getClass().getResourceAsStream("TokenizerAbbreviation");
			try {
				_abbreException = loadAbbreException(ABBREVIATION_EXCEPTION);
			} catch(IOException e) {
				System.out.println("ERROR: Abbrevation Exception file cannot be found.");
				e.printStackTrace();
			}
		}
		if(_m == TOKENIZER_MODE.WITH_PUNCTUATE)
			mode = _MODE_WITH_PUNCTUATE;
		else if(_m == TOKENIZER_MODE.WITHOUT_PUNCTUATE)
			mode = _MODE_WITHOUT_PUNCTUATE;
	}
	
	private HashSet<String> loadAbbreException(InputStream _filename) throws IOException {
		HashSet<String> returnValue = new HashSet<String>();
		
		BufferedReader br = new BufferedReader(new InputStreamReader(_filename));
		
		String line = null;
		while((line = br.readLine()) != null) {
			if(line.startsWith("#")) continue;
			returnValue.add(line.trim());
		}
		
		br.close();
		
		return returnValue;
	}
	
	public ArrayList<ArrayList<String>> process(String text) {
		
		ArrayList<ArrayList<String>> paragraph = new ArrayList<ArrayList<String>>();
		
		Locale currentLocale = new Locale ("en","US");
		BreakIterator wordIterator = BreakIterator.getWordInstance(currentLocale);
		wordIterator.setText(text);
		
		int sPtr = wordIterator.first();
		int ePtr = wordIterator.next();
		
		ArrayList<String> currentSentence = new ArrayList<String>();
		
		while(ePtr != BreakIterator.DONE) {
			String word = text.substring(sPtr,ePtr);
			if (
					!word.trim().equals("")
					&& ((mode == _MODE_WITH_PUNCTUATE) || (mode == _MODE_WITHOUT_PUNCTUATE && (Character.isLetter(word.charAt(0)) || Character.isDigit(word.charAt(0)))))
				) {
				//System.out.println(word);
				boolean canBreakSentence = true;
				if(word.indexOf("'") != -1) {
					if(word.endsWith("n't")) {
						String w1 = word.substring(0, word.length() - 3);
						if(!w1.equals(""))
							currentSentence.add(w1);
						currentSentence.add("n't");
					} else if(word.endsWith("'s") || word.endsWith("'ll") ||
							word.endsWith("'re") || word.endsWith("'m") ||
							word.endsWith("'ve") || word.endsWith("'d")) {
						int p = word.indexOf("'");
						String w1 = word.substring(0, p);
						String w2 = word.substring(p);
						if(!w1.equals(""))
							currentSentence.add(w1);
						if(!w2.equals(""))
							currentSentence.add(w2);
					} else {
						currentSentence.add(word);
					}
					/* else {
						int p = word.indexOf("'");
						String w1 = word.substring(0, p);
						String w2 = word.substring(p);
						if(!w1.equals(""))
							currentSentence.add(w1);
						if(!w2.equals(""))
							currentSentence.add(w2);
					}*/
				} else if(word.trim().equals(".")) {
					int formerIndex = currentSentence.size() - 1;
					if(formerIndex == -1) {
						currentSentence.add(word);
					} else {
						String formerWord = currentSentence.get(formerIndex);
						if(_abbreException.contains(formerWord) || (formerWord.length() == 1 && Character.isUpperCase(formerWord.charAt(0)))
								|| formerWord.indexOf(".") >= 0) {
							currentSentence.remove(formerIndex);
							currentSentence.add(formerWord + ".");
							// do not break the sentence
							canBreakSentence = false;
						} else {
							currentSentence.add(word);
						}
					}
				} else if(word.trim().equals(":")) {
					try {
					if(text.charAt(ePtr) != ' ' && text.charAt(sPtr - 1) != ' ' && currentSentence.size() > 0) {
						int formerIndex = currentSentence.size() - 1;
						String formerWord = currentSentence.get(formerIndex);
						sPtr = ePtr;
						ePtr = wordIterator.next();
						if(ePtr == BreakIterator.DONE) {
							currentSentence.add(word);
							continue;
						} else {
							String nextWord = text.substring(sPtr, ePtr);
							if(Character.isLetterOrDigit(nextWord.charAt(0)) &&
									Character.isLetterOrDigit(formerWord.charAt(0))) {
								// merge
								currentSentence.remove(formerIndex);
								currentSentence.add(formerWord + word + nextWord);
							} else {
								currentSentence.add(word);
								currentSentence.add(nextWord);
							}
						}
					} else {
						currentSentence.add(word);
					}
					} catch(StringIndexOutOfBoundsException e1) {
						currentSentence.add(word);
					}
				} else {
					currentSentence.add(word);
				}
				// handling the end of a sentence
				if((word.trim().equals(".") || word.trim().equals(";")
						|| word.trim().equals("?") || word.trim().equals("!"))
						&& canBreakSentence) {
					paragraph.add(currentSentence);
					currentSentence = new ArrayList<String>();
				}
			}
			sPtr = ePtr;
			ePtr = wordIterator.next();
		}
		
		if(!currentSentence.isEmpty()) paragraph.add(currentSentence);
		return paragraph;
	}
}
