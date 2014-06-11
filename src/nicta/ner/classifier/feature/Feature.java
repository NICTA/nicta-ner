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

import java.util.HashSet;
import java.util.Scanner;
import java.io.*;
import java.net.URLDecoder;

import nicta.ner.data.Phrase;
import nicta.ner.util.Dictionary;

/**
 * This abstract class is a parent of features.
 * 
 * @author William Han
 *
 */
public abstract class Feature {
	/**
	 * Returns a score of the phrase according to the particular feature.
	 * 
	 * @param _p
	 * @return
	 */
	public abstract double score(Phrase _p);
	
	/**
	 * Factory method create features by name.
	 * 
	 * @param args
	 * @return
	 * @throws Exception
	 */
	public static Feature generateFeatureByName(String[] args) throws Exception {
		Feature f = null;
		if(args[0].equalsIgnoreCase("RuledWordFeature")) {
			f = new RuledWordFeature(args[1]);
		} else if(args[0].equalsIgnoreCase("RuledPhraseFeature")) {
			f = new RuledPhraseFeature(args[1]);
		} else if(args[0].equalsIgnoreCase("PrepositionContextFeature")) {
			f = new PrepositionContextFeature(args[1]);
		} else if(args[0].equalsIgnoreCase("ExistingPhraseFeature")) {
			f = new ExistingPhraseFeature(args[1]);
		}
		if(f == null) throw new Exception("Feature not found.");
		return f;
	}
	
	/**
	 * Return a HashSet contains phrases (multi-words).
	 * 
	 * @param filename
	 * @return
	 */
	public static HashSet<String> createPhraseSet(String filename) {
		//System.out.println("Creating Phrase Set: " + filename);
		HashSet<String> _set = new HashSet<String>();
		int count = 0;
		try {
			BufferedReader br = new BufferedReader(
					new InputStreamReader(
							Feature.class.getResourceAsStream(filename)));
			
			String line = null;
			while((line = br.readLine()) != null) {
				if(line.startsWith("#")) continue;
				_set.add(line);
				count++;
			}
			
			br.close();
		} catch(IOException ioe) {
			System.out.println("ERROR: Error in reading Feature file: " + filename);
			System.out.println("       at Configuration File: Feature");
			// e1.printStackTrace();
			System.exit(-1);
		}
		//System.out.println(count + " words added.");
		return _set;
	}
	
	/**
	 * Returns a HashSet contains only single words.
	 * 
	 * @param filename
	 * @return
	 */
	public static HashSet<String> createSingleWordSet(String filename, boolean eliminatePrepAndConj) {
		//System.out.println("Creating Word Set: " + filename);
		InputStream is = Feature.class.getResourceAsStream(filename);
		Dictionary dict = Dictionary.getSharedDictionary();
		HashSet<String> set = new HashSet<String>();
		int count = 0;
		try {
			BufferedReader br = new BufferedReader(
					new InputStreamReader(is));
			
			String line = null;
			while((line = br.readLine()) != null) {
				if(line.startsWith("#")) continue;
				String[] splited = line.split(" ");
				for(String word : splited) {
					String wordType = dict.checkup(word);
					if(eliminatePrepAndConj && wordType != null && (wordType.startsWith("IN") || wordType.startsWith("CC"))) continue;
					set.add(word);
					count++;
				}
			}
			
			br.close();
		} catch(IOException e1) {
			System.out.println("ERROR: Error in reading Feature file: " + filename);
			System.out.println("       at Configuration File: Feature");
			// e1.printStackTrace();
			System.exit(-1);
		}
		//System.out.println(count + " words added.");
		return set;
	}
}
