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
package nicta.ner.resource;

import nicta.ner.classifier.feature.Feature;
import nicta.ner.classifier.feature.FeatureMap;
import nicta.ner.data.NameType;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.ArrayList;

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
