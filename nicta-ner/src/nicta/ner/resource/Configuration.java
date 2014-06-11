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

package nicta.ner.resource;

import java.io.*;
import java.net.URLDecoder;
import java.util.*;

import nicta.ner.classifier.feature.*;
import nicta.ner.data.NameType;

/**
 * This class specifies the configurations.
 * 
 * @author William Han
 *
 */
public class Configuration {
	
	// static filenames:
	/** The Configuration file path. */
	public static InputStream CONFIG_FILE = null;
	
	/** NameType array specifies the possible types of names. */
	public NameType[] name_type = null;
	
	/** Feature array specifies the features used in name type recognition. */
	private Feature[] feature_array = null;
	public FeatureMap feature_map = null;
	
	/** w is the coefficient array. */
	public double[][] w;
	
	/**
	 * Constructor. Read in the config file.
	 * 
	 * @param filename
	 */
	public Configuration() {
		try {
			if(CONFIG_FILE == null) {
				CONFIG_FILE = this.getClass().getResourceAsStream("config");
				try {
					readConfigFile(CONFIG_FILE);
				} catch(IOException e1) {
					System.out.println("ERROR: Config file \"" + CONFIG_FILE + "\" cannot be read or closed properly by the program.");
					e1.printStackTrace();
					System.exit(-1);
				} catch(NullPointerException e2) {
					System.out.println("ERROR: Config file reading error. Please check the config file syntax.");
					e2.printStackTrace();
					System.exit(-1);
				} catch(Exception e3) {
					System.out.println(e3.toString());
					System.exit(-1);
				}
			}
		} catch(Exception e) {
			System.out.println("ERROR: Configuration");
			e.printStackTrace();
			System.exit(-1);
		}
	}
	
	/**
	 * Read the information in the config file.
	 * 
	 * @param filename
	 */
	private void readConfigFile(InputStream filename) throws IOException, NullPointerException, Exception {
		System.out.println("Initializing...");
		
		// feature array texts
		ArrayList<String[]> featureTexts = new ArrayList<String[]>();
		// name type texts
		String[] nameTypeTexts = null;;
		// w texts
		String[] wTexts = null;;
		
		// throw this exception if syntax is wrong.
		Exception syntaxErrorException = new Exception("Config File Syntax Error!");
		
		BufferedReader br = new BufferedReader(new InputStreamReader(filename));
		
		// read each line from the file and put the information
		// in the temperate variables
		// need further process to extract the information
		String line = null;
		while((line = br.readLine()) != null) {
			if(line.startsWith("#")) continue;
			if(line.trim().equals("")) continue;
			int co_pos = line.indexOf(":");
			String command = line.substring(0, co_pos).trim();
			String content = line.substring(co_pos + 1).trim();
			if(command.equalsIgnoreCase("Name Types")) {
				nameTypeTexts = content.split(" ");
			} else if(command.equalsIgnoreCase("Feature")) {
				String[] c = content.split(" ");
				if(c.length != 2) throw syntaxErrorException;
				featureTexts.add(c);
			} else if(command.equalsIgnoreCase("w")) {
				content = content.replace("| ", "");
				wTexts = content.split(" ");
			}
		}
		
		br.close();
		
		if(nameTypeTexts == null || featureTexts.isEmpty() || wTexts == null)
			throw syntaxErrorException;
		
		int featureDimension = featureTexts.size();
		int nameTypeDimension = nameTypeTexts.length;
		// if syntax error, throw exception.
		if(wTexts.length != featureDimension * nameTypeDimension)
			throw syntaxErrorException;
		
		feature_array = new Feature[featureDimension];
		name_type = new NameType[nameTypeDimension];
		w = new double[nameTypeDimension][featureDimension];
		
		// feature information
		for(int i = 0; i < featureDimension; i++) {
			String[] line_splited = featureTexts.get(i);
			Feature f = Feature.generateFeatureByName(line_splited);
			feature_array[i] = f;
		}
		
		// name_type information
		for(int i = 0; i < nameTypeDimension; i++) {
			name_type[i] = new NameType(nameTypeTexts[i]);
		}
		
		// weight array
		int wi = 0;
		for(int i = 0; i < nameTypeDimension; i++) {
			for(int j = 0; j < featureDimension; j++) {
				w[i][j] = Double.parseDouble(wTexts[wi]);
				wi++;
			}
		}
		
		// create feature map
		feature_map = new FeatureMap(feature_array, w);
		
		System.out.println("Done.\n\n");
	}
}
