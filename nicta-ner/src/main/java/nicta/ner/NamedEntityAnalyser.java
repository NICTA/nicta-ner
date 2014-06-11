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
package nicta.ner;

import nicta.ner.classifier.NameClassifier;
import nicta.ner.extractor.NameExtractor;
import nicta.ner.resource.Configuration;

import java.io.BufferedInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.util.Scanner;


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
