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

import java.io.*;
import java.net.URLDecoder;
import java.util.*;

import nicta.ner.resource.Configuration;



/**
 * Dictionary
 * 
 * 
 * 
 * @author William Han
 *
 */

public class Dictionary {
	
	// get the file name from configuration
	private InputStream DICT_FILE = null;
	
	/** this is the only instance that can be used of this class. */
	private static Dictionary sharedDictionary = null;
	
	/**
	 * This method returns the shared instance of this class.
	 * 
	 * @return
	 */
	public static Dictionary getSharedDictionary() {
		try {
			if(sharedDictionary == null)
				sharedDictionary = new Dictionary();
		} catch(Exception e) {
			System.out.println("Unexcepted exception in Dictionary: getSharedDictionary()");
			e.printStackTrace();
			System.exit(-1);
		}
		return sharedDictionary;
	}
	
	/** dictionary map: word maps to its type */
	private HashMap<String, String> dict = null;
	
	/**
	 * Private constructor which creates the only dictionary instance.
	 * Call getSharedDictionary() method to get the shared dictionary.
	 */
	private Dictionary() throws Exception {
		if(DICT_FILE == null) {
			DICT_FILE = this.getClass().getResourceAsStream("DICT");
			dict = new HashMap<String, String>();
			try {
				BufferedReader br = new BufferedReader(new InputStreamReader(DICT_FILE));
				//System.out.println("Creating dictionary: " + DICT_FILE);
				String line = null;
				while((line = br.readLine()) != null) {
					if(line.startsWith("#")) continue;
					String[] splited = line.split("\t");
					dict.put(splited[0], splited[1]);
				}
				//System.out.println(count + " words added.");
			} catch(IOException ioe) {
				ioe.printStackTrace();
			}
		}
	}
	
	public String checkup(String word) {
		return dict.get(word);
	}
	
	/**
	 * This method checks if the word is a plural form.
	 * 
	 * @param _word
	 * @return
	 */
	public static boolean isPlural(String _word) {
		String word = _word.toLowerCase();
		String wordStub = null;
		
		// word + s
		if(word.endsWith("s")) {
			wordStub = word.substring(0, word.length() - 1);
			if(getSharedDictionary().checkup(wordStub) != null) return true;
		}
		
		// word + ed
		if(word.endsWith("ed")) {
			wordStub = word.substring(0, word.length() - 2);
			if(getSharedDictionary().checkup(wordStub) != null) return true;
		}
		
		// word(-y) + ied
		if(word.endsWith("ied")) {
			wordStub = word.substring(0, word.length() - 3) + "y";
			if(getSharedDictionary().checkup(wordStub) != null) return true;
		}
		
		return false;
	}
	
	/**
	 * This method checks if the word is a past tense word.
	 * 
	 * @param _word
	 * @return
	 */
	public static boolean isPastTense(String _word) {
		String word = _word.toLowerCase();
		String wordStub = null;
		
		// word(e) + d
		if(word.endsWith("d")) {
			wordStub = word.substring(0, word.length() - 1);
			if(getSharedDictionary().checkup(wordStub) != null) return true;
		}
		
		// word + ed
		if(word.endsWith("ces") || word.endsWith("ses")) {
			wordStub = word.substring(0, word.length() - 2);
			if(getSharedDictionary().checkup(wordStub) != null) return true;
		}
		
		// word(-y) + ies
		if(word.endsWith("ies")) {
			wordStub = word.substring(0, word.length() - 3) + "y";
			if(getSharedDictionary().checkup(wordStub) != null) return true;
		}
		
		return false;
	}

}
