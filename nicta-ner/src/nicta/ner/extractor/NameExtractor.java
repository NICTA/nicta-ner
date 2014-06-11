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

package nicta.ner.extractor;

import java.util.*;
import java.io.*;
import java.net.URLDecoder;


import nicta.ner.NERResultSet;
import nicta.ner.classifier.feature.Feature;
import nicta.ner.data.DatePhraseModel;
import nicta.ner.data.Phrase;
import nicta.ner.data.Phrase_Date;
import nicta.ner.data.Phrase_Name;
import nicta.ner.resource.Configuration;
import nicta.ner.util.Dictionary;
import nicta.ner.util.JTokenizer;

/**
 * Rule-based expert system.
 * 
 * by William Han, 2010-10-26
 * 
 * @author William Han
 * @version 1
 */
public class NameExtractor {
	
	static JTokenizer TOKENIZER = null;
	static HashSet<String> NON_NAME_WORDS = null;
	Dictionary dict = Dictionary.getSharedDictionary();
	
	private ArrayList<ArrayList<Phrase>> phrases = null;
	private ArrayList<ArrayList<String>> tokens = null;
	
	private Configuration config = null;
	
	int nameTypeScoreDimension = -1;
	
	/**
	 * Constructor to build the Named Entity Extractor instance.
	 * 
	 * @param scoreDimension
	 */
	public NameExtractor(Configuration conf) throws Exception {
		if(TOKENIZER == null) TOKENIZER = new JTokenizer(JTokenizer.TOKENIZER_MODE.WITH_PUNCTUATE);
		if(NON_NAME_WORDS == null) NON_NAME_WORDS = generateWordSet(NameExtractor.class.getResourceAsStream("NON_NAME_WORDS"));
		config = conf;
		nameTypeScoreDimension = config.name_type.length;
	}
	
	/**
	 * Call this method to return the name phrase result <b>after</b> the process(String _text) method had been called.
	 * 
	 * @return
	 */
	public NERResultSet getResult() {
		NERResultSet ner = new NERResultSet();
		ner.phrases = phrases;
		ner.tokens = tokens;
		return ner;
	}
	
	/**
	 * This method will parse the text input into tokens and name phrases.
	 * Call getResult() method to get the processed result.
	 * 
	 * @param _text
	 */
	public void process(String _text) {
		// handling the illegal characters:
		_text = _text.replace("”", "\"");
		_text = _text.replace("“", "\"");
		_text = _text.replace("’", "'");
		//_text = _text.replace("", "");
		
		// tokenization
		tokens = TOKENIZER.process(_text);
		int sentenceCount = tokens.size();
		
		// extract name phrase:
		phrases = new ArrayList<ArrayList<Phrase>>();
		
		for(int si = 0; si < sentenceCount; si++) {
			// for each sentence
			ArrayList<Phrase> sentencePhrase = new ArrayList<Phrase>();
			ArrayList<String> sentenceToken = tokens.get(si);
			int wordPtr = 0;
			int phraseStubStartPtr = 0;
			int phraseStubLength = 0;
			while(wordPtr < sentenceToken.size()) {
				
				// iterate through every words in the sentence
				ArrayList<String> currentPhrase = new ArrayList<String>();
				
				// get the phrase stub
				while(true) {
					boolean mergeAndFlag = false;	// we merge the names combined with the word 'and'
					// if the second phrase is much shorter than the first phrase
					// example: Canberra Institution of Science and Technology
					if(!sentenceToken.get(wordPtr).equalsIgnoreCase("and")) {
						//mergeAndFlag = false;
					} else if(currentPhrase.isEmpty()) {
						//mergeAndFlag = false;
					} else if(!(wordPtr + 1 < sentenceToken.size() && detectNameWordInSentenceByPosition(sentenceToken, wordPtr + 1))) {
						//mergeAndFlag = false;
					} else {
						if(wordPtr + currentPhrase.size() - 1 >= sentenceToken.size()) {
							mergeAndFlag = true;
						} else {
							for(int i = wordPtr + 1; i <= wordPtr + currentPhrase.size() - 1; i++) {
								if(!detectNameWordInSentenceByPosition(sentenceToken, i)) {
									mergeAndFlag = true;
									break;
								}
							}
						}
					}
					if
					(detectNameWordInSentenceByPosition(sentenceToken, wordPtr) ||
							(
									(sentenceToken.get(wordPtr).equalsIgnoreCase("of") || sentenceToken.get(wordPtr).equalsIgnoreCase("the") ||
											sentenceToken.get(wordPtr).equalsIgnoreCase("'s"))
									&&
									!currentPhrase.isEmpty() &&
									wordPtr + 1 < sentenceToken.size() && detectNameWordInSentenceByPosition(sentenceToken, wordPtr + 1)
							) ||
							(
									mergeAndFlag
							)
					) {
						if(currentPhrase.isEmpty()) phraseStubStartPtr = wordPtr; // record the index of the first word
						currentPhrase.add(sentenceToken.get(wordPtr));
						wordPtr++;
						if(wordPtr == sentenceToken.size()) break;
					} else {
						break;
					}
				}
				phraseStubLength = currentPhrase.size();
				
				//get the attached 'the' or 'a'
				int phrasePos = phraseStubStartPtr;
				int phraseLen = currentPhrase.size();
				
				if(phrasePos > 0) {	// if phrasePos is zero, then it is the first word, there won't be any 'the' or 'a' attached to this word.
					String attachedArticle = sentenceToken.get(phrasePos - 1);
					if(attachedArticle.equalsIgnoreCase("the") || attachedArticle.equalsIgnoreCase("a")) {
						phrasePos--;
						phraseLen++;
					}
				}
				
				//handling non-name single words such as: Monday, January, etc.
				if(phraseStubLength == 1 && phraseStubStartPtr == phrasePos) {
					if(NON_NAME_WORDS.contains(currentPhrase.get(0).toLowerCase())) {
						currentPhrase.clear();
						wordPtr--;
					}
				}
				
				if(!currentPhrase.isEmpty()) {
					if(!(currentPhrase.size() == 1 &&
							(Dictionary.isPastTense(currentPhrase.get(0)) || Dictionary.isPlural(currentPhrase.get(0)))
							)) {
						String[] currentPhraseArray = new String[currentPhrase.size()];
						for(int i = 0; i < currentPhrase.size(); i++)
							currentPhraseArray[i] = currentPhrase.get(i);
						sentencePhrase.add(new Phrase_Name(currentPhraseArray, phrasePos, phraseLen, phraseStubStartPtr, nameTypeScoreDimension));
					}
				} else {
					// if the current pointed word is not a name phrase
					// ESTIMATE IF IT IS A TIME/DATE
					ArrayList<String> currentDatePhrase = new ArrayList<String>();
					DatePhraseModel dpm = new DatePhraseModel();
					int tempPtr = wordPtr;
					while(true) {
						String word = sentenceToken.get(tempPtr);
						int type = Phrase_Date.getDateType(word);
						if(
								(
										type >= 0
								) ||
								(
										(word.equalsIgnoreCase("of") || word.equalsIgnoreCase(",") || word.equalsIgnoreCase("the")) &&
										!currentDatePhrase.isEmpty() && tempPtr + 1 != sentenceToken.size() &&
										Phrase_Date.getDateType(sentenceToken.get(tempPtr + 1)) >= 0 &&
										Phrase_Date.getDateType(sentenceToken.get(tempPtr - 1)) >= 0
								)
						) {
							// is date word:
							currentDatePhrase.add(word);
							dpm.addType(type);
							tempPtr++;
							if(tempPtr == sentenceToken.size()) break;
						} else {
							break;
						}
					}
					if(dpm.isDate()) {
						// add the phrase to phrase array
						String[] currentPhraseArray = new String[currentDatePhrase.size()];
						for(int i = 0; i < currentDatePhrase.size(); i++)
							currentPhraseArray[i] = currentDatePhrase.get(i);
						Phrase newPhrase = new Phrase_Date(currentPhraseArray, wordPtr, currentPhraseArray.length, wordPtr, nameTypeScoreDimension);
						sentencePhrase.add(newPhrase);
						wordPtr = tempPtr;
					}
				}
				
				wordPtr++;
			}
			
			phrases.add(sentencePhrase);
		}
		
		// find attached words(preps):
		for(int si = 0; si < tokens.size(); si++) {
			ArrayList<String> sentenceToken = tokens.get(si);
			ArrayList<Phrase> sentencePhrase = phrases.get(si);
			for(int pi = 0; pi < sentencePhrase.size(); pi++) {
				getAttachedPrep(sentenceToken, sentencePhrase, pi);
			}
		}
	}
	
	/**
	 * Detects if a particular word in a sentence is a name.
	 * 
	 * @param _text
	 * @param _pos
	 * @return
	 */
	private boolean detectNameWordInSentenceByPosition(ArrayList<String> _text, int _pos) {
		boolean isFirstWord = false;
		boolean nextWordIsName = false;
		if(_pos == 0
				|| !Character.isLetterOrDigit((_text.get(_pos - 1).charAt(0)))
				//|| (_text.get(_pos - 1).equals("\""))
				//|| (_text.get(_pos - 1).equals(":"))
				) {
			isFirstWord = true;
			nextWordIsName = _text.size() > _pos + 1 ?
					(
							_text.get(_pos + 1).equalsIgnoreCase("of") ||
							_text.get(_pos + 1).equalsIgnoreCase("'s") ? (_text.size() > _pos + 2 ? detectNameWord(_text.get(_pos + 2), false, false) : false) :
								detectNameWord(_text.get(_pos + 1), false, false)
							) : false;
		}
		boolean isName = detectNameWord(_text.get(_pos), isFirstWord, nextWordIsName);
		
		/*
		String wordType = dict.checkup(_text.get(_pos).toLowerCase());
		if(isFirstWord && !isName && wordType != null && wordType.startsWith("JJ")) {
			// if the first word is determined not to be a name but it is an adj.,
			// and if the second word is a name, we consider the first word to be a name as well.
			if(detectNameWord(_text.get(_pos + 1), false))
				return true;
		}
		*/
		
		return isName;
	}
	
	/**
	 * This method detects a word if it is a potential name word.
	 * 
	 * @param _text
	 * @param isFirstWord
	 * @return
	 */
	private boolean detectNameWord(String _text, boolean isFirstWord, boolean nextWordIsName) {
		if(detectMultiCharCapital(_text)) {
			return true;
		} else if(detectFirstCharCapital(_text)) {
			if(!isFirstWord)
				return true;
			else {
				// if the word is the first word in a sentence and
				// ends with -ly (adv) -> consider it not a name
				if(_text.endsWith("ly"))
					return false;
				// we need to deal with the first word in the sentence very carefully.
				// as we can not tell if the first word is a name by detecting upper case characters.
				String type_original = dict.checkup(_text);
				String type_lowercase = dict.checkup(_text.toLowerCase());
				if(type_original == null) {
					if(type_lowercase == null) {
						return true;	// if the word cannot be found in the dictionary, we consider it as a name entity.
					}/* else {
						return false;
					}*/
				} else if(type_original.equals("NNP"))
					return true;
				if(type_lowercase != null && (
						type_lowercase.startsWith("IN")
						|| type_lowercase.startsWith("CC")
						|| type_lowercase.startsWith("RB")
						|| type_lowercase.startsWith("WRB")
						// || type_lowercase.startsWith("JJ")
						|| type_lowercase.startsWith("."))) return false;
				else {
					if(nextWordIsName)
						return true;
					else
						return false;
				}
			}
		} else {
			return false;
		}
	}
	
	/**
	 * This method will find all the attached preps of a phrase.
	 * 
	 * @param sentenceToken
	 * @param sentencePhrase
	 * @param index
	 */
	protected void getAttachedPrep(ArrayList<String> sentenceToken, ArrayList<Phrase> sentencePhrase, int index) {
		String prep = null;
		boolean nameSequenceMeetEnd = true;
		HashSet<Phrase> phraseSequence = new HashSet<Phrase>();
		
		int phrasePtr = index;
		Phrase currentNamePhrase = sentencePhrase.get(phrasePtr);
		int phrasePtrInSentence = currentNamePhrase.phrasePosition;
		
		if(currentNamePhrase.attachedWordMap.get("prep") != null) return;
		
		// we need to find out all the name phrases in a sequence:
		// Example: Students who came from China, America and Australia are here.
		// in the example above, China, America and Australia are three name phrases all attached to the prep: from.
		// first loop, search forward to find all the names before the pointer and the attached prep
		while(prep == null) {
			currentNamePhrase = sentencePhrase.get(phrasePtr);
			phrasePtrInSentence = currentNamePhrase.phrasePosition;
			
			if(phrasePtrInSentence == 0) return;
			
			String attachedWord = sentenceToken.get(phrasePtrInSentence - 1);
			
			// if the attached word is a comma or 'and'/'or', we consider it as a conj.
			if(attachedWord.equalsIgnoreCase(",")) {
				nameSequenceMeetEnd = false;
				phraseSequence.add(currentNamePhrase);
			} else if(attachedWord.equalsIgnoreCase("and") || attachedWord.equalsIgnoreCase("or")) {
				// meet end
				phraseSequence.add(currentNamePhrase);
				nameSequenceMeetEnd = true;
			} else if(dict.checkup(attachedWord.toLowerCase()) != null && dict.checkup(attachedWord.toLowerCase()).startsWith("IN")) {
				prep = attachedWord;
				phraseSequence.add(currentNamePhrase);
				break;
			} else {
				return;
			}
			phrasePtr--;
			if(phrasePtr < 0) return;
			if(sentencePhrase.get(phrasePtr).isDate) return;
			if(sentencePhrase.get(phrasePtr).phrasePosition + sentencePhrase.get(phrasePtr).phraseLength + 1 != phrasePtrInSentence) return;
			// method terminates if the phrase before is not next to this phrase. This means the name sequence is broken.
		}
		
		//if(sentence.phrases.get(index).phrase[0].equalsIgnoreCase("Florida"))
		//	System.out.println("First time Florida: " + prep);
		
		phrasePtr = index + 1;
		
		// second loop, search backward to find the names behind the pointer
		while(!nameSequenceMeetEnd) {
			if(phrasePtr == sentencePhrase.size()) return;
			
			currentNamePhrase = sentencePhrase.get(phrasePtr);
			if(currentNamePhrase.isDate) return;
			phrasePtrInSentence = currentNamePhrase.phrasePosition;
			
			if(sentencePhrase.get(phrasePtr - 1).phrasePosition + sentencePhrase.get(phrasePtr - 1).phraseLength + 1 != currentNamePhrase.phrasePosition) return;
			// method terminates if the phrase after is not next to this phrase.
			
			String attachedWord = sentenceToken.get(phrasePtrInSentence - 1);
			// if the attached word is a comma or 'and'/'or', we consider it as a conj.
			if(attachedWord.equalsIgnoreCase(",")) {
				phraseSequence.add(currentNamePhrase);
			} else if(attachedWord.equalsIgnoreCase("and") || attachedWord.equalsIgnoreCase("or")) {
				// meet end
				phraseSequence.add(currentNamePhrase);
				nameSequenceMeetEnd = true;
				break;
			} else {
				return;
			}
			
			phrasePtr++;
		}
		
		// finally, attach the prep with the words in the phraseSequence
		for(Phrase name : phraseSequence) {
			name.attachedWordMap.put("prep", prep);
		}
	}

	
	/**
	 * This method detects if the word's first character is in capital size.
	 * Returns true if it is; false if it is not.
	 * 
	 * @param word
	 * @return
	 */
	private boolean detectFirstCharCapital(String word) {
		if(word.equalsIgnoreCase("i")) return false;
		if(!detectLegalCharacter(word.charAt(0))) return false;
		return Character.isUpperCase(word.charAt(0));
	}
	
	/**
	 * This method detects if the word has more than one character which is capital sized.
	 * Returns true if there are more than one character upper cased; return false if less than one character is upper cased.
	 * 
	 * @param word
	 * @return
	 */
	private boolean detectMultiCharCapital(String word) {
		if(word.equalsIgnoreCase("i")) return false;
		int capCharCount = 0;
		for(int i = 0; i < word.length(); i++) {
			if(Character.isUpperCase(word.charAt(i))) capCharCount++;
			if(capCharCount == 2) return true;
		}
		return false;
	}
	
	/**
	 * This method tests if the input character is a legal character (a-z || A-Z).
	 * 
	 * @param _c
	 * @return
	 */
	private boolean detectLegalCharacter(char _c) {
		char c = Character.toLowerCase(_c);
		if("abcdefghijklmnopqrstuvwxyz".indexOf(c) == -1) return false;
		else return true;
	}
	
	/**
	 * This method returns a HashSet of words by reading a text file.
	 * 
	 * @param filePath
	 * @return
	 */
	private HashSet<String> generateWordSet(InputStream filePath) {
		HashSet<String> _set = new HashSet<String>();
		//System.out.println("Creating word map: " + filePath);
		int count = 0;
		try {
			BufferedReader br = new BufferedReader(
					new InputStreamReader(
							filePath));
			
			String line = null;
			while((line = br.readLine()) != null) {
				if(line.startsWith("#")) continue;
				_set.add(line.trim().toLowerCase());
				count++;
			}
			
			br.close();
		} catch(IOException ioe) {
			System.out.println("ERROR: " + filePath + " reading error.");
		}
		//System.out.println(count + " words added.");
		
		return _set;
	}
}
