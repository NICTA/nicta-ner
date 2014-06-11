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

import java.io.BufferedInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.util.*;

import nicta.ner.classifier.NameClassifier;
import nicta.ner.extractor.NameExtractor;
import nicta.ner.resource.Configuration;


/**
 * This is the main class of the Named Entity Recognition system.
 * 
 * This system contains two parts: named entity extraction and named entity classification.
 * 
 * @author William Han
 *
 */
public class NamedEntityAnalyser {
	
	/** The named entity extractor */
	NameExtractor extractor = null;
	/** The named entity classifier */
	NameClassifier classifier = null;
	
	/**
	 * Constructor to create a NamedEntityAnalyser. This Analyser will extract and classify
	 * the named entities in the input text.
	 */
	public NamedEntityAnalyser() throws Exception {
		// create configuration class instance:
		Configuration config = new Configuration();
		
		// create NameExtractor instance:
		extractor = new NameExtractor(config);
		
		// create Scoring instance:
		classifier = new NameClassifier(config);
	}
	
	/**
	 * Process the text, and return a NERResultSet.
	 * 
	 * See NERResultSet class.
	 * 
	 * @param text
	 * @return
	 */
	public NERResultSet process(String text) {
		// program pipeline: (Text) ------> (Phrases without classified) ------> (Phrases classified)
		//                         extractor                            classifier
		extractor.process(text);
		NERResultSet rs = extractor.getResult();
		rs = classifier.process(rs);
		return rs;
	}
	
	/**
	 * Read a file and parse it to string.
	 * 
	 * @param filePath
	 * @return
	 * @throws Exception
	 */
	public static String ReadFileAsString(String filePath) throws Exception {
	    byte[] buffer = new byte[(int) new File(filePath).length()];
	    BufferedInputStream f = null;
	    try {
	        f = new BufferedInputStream(new FileInputStream(filePath));
	        f.read(buffer);
	    } finally {
	        if (f != null) try { f.close(); } catch (IOException ignored) { }
	    }
	    return new String(buffer);
	}
	
	/**
	 * The main() method takes a file as input and output the
	 * process result on the screen.
	 * 
	 * @param args
	 * @throws Exception
	 */
	public static void main(String[] args) throws Exception {
		NamedEntityAnalyser nea = new NamedEntityAnalyser();
		String processString = null;
		String filename = null;
		if(args.length >= 1) {
			filename = args[0];
		}
		if(filename == null) {
			Scanner in = new Scanner(System.in);
			while(true) {
				System.out.print("Type in texts, -q for quit.\n> ");
				processString = in.nextLine();
				if(processString.equalsIgnoreCase("-q")) break;
				System.out.println(nea.process(processString).getMappedResult());
			}
		} else {
			System.out.println(nea.process(ReadFileAsString(filename)));
		}
	}
}
