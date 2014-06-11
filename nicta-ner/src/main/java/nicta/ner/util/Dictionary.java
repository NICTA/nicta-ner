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
package nicta.ner.util;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.HashMap;



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
