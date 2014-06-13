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
import java.io.InputStreamReader;
import java.util.ArrayList;

/**
 * This class specifies the configurations.
 * @author William Han
 */
public class Configuration {

    /** NameType array specifies the possible types of names. */
    public NameType[] name_type = null;

    public FeatureMap feature_map = null;

    /** w is the coefficient array. */
    public double[][] w;

    /**
     * Constructor. Read in the config file.
     */
    public Configuration() {
        try { readConfigFile(); }
        catch (final IOException | NullPointerException e1) {
            System.out.println("ERROR: Config file reading error. Please check the config file syntax.");
            e1.printStackTrace();
            System.exit(-1);
        }
        catch (final Exception e3) {
            e3.printStackTrace();
            System.exit(-1);
        }
    }

    /**
     * Read the information in the config file.
     */
    private void readConfigFile() throws Exception {
        // feature array texts
        final ArrayList<String[]> featureTexts = new ArrayList<>();
        // name type texts
        String[] nameTypeTexts = null;
        // w texts
        String[] wTexts = null;

        try (final BufferedReader br = new BufferedReader(new InputStreamReader(this.getClass().getResourceAsStream("config")))) {

            // read each line from the file and put the information
            // in the temperate variables
            // need further process to extract the information
            String line;
            while ((line = br.readLine()) != null) {
                if (line.startsWith("#")) continue;
                if (line.trim().equals("")) continue;
                final int co_pos = line.indexOf(":");
                final String command = line.substring(0, co_pos).trim();
                String content = line.substring(co_pos + 1).trim();
                if (command.equalsIgnoreCase("Name Types")) {
                    nameTypeTexts = content.split(" ");
                }
                else if (command.equalsIgnoreCase("Feature")) {
                    final String[] c = content.split(" ");
                    if (c.length != 2) throw new Exception("Config File Syntax Error!");
                    featureTexts.add(c);
                }
                else if (command.equalsIgnoreCase("w")) {
                    content = content.replace("| ", "");
                    wTexts = content.split(" ");
                }
            }
        }

        if (nameTypeTexts == null || featureTexts.isEmpty() || wTexts == null)
            throw new Exception("Config File Syntax Error!");

        final int featureDimension = featureTexts.size();
        final int nameTypeDimension = nameTypeTexts.length;
        // if syntax error, throw exception.
        if (wTexts.length != featureDimension * nameTypeDimension)
            throw new Exception("Config File Syntax Error!");

        /* Feature array specifies the features used in name type recognition. */
        final Feature[] feature_array = new Feature[featureDimension];
        name_type = new NameType[nameTypeDimension];
        w = new double[nameTypeDimension][featureDimension];

        // feature information
        for (int i = 0; i < featureDimension; i++) {
            final String[] line_splited = featureTexts.get(i);
            final Feature f = Feature.generateFeatureByName(line_splited);
            feature_array[i] = f;
        }

        // name_type information
        for (int i = 0; i < nameTypeDimension; i++) {
            name_type[i] = new NameType(nameTypeTexts[i]);
        }

        // weight array
        int wi = 0;
        for (int i = 0; i < nameTypeDimension; i++) {
            for (int j = 0; j < featureDimension; j++) {
                w[i][j] = Double.parseDouble(wTexts[wi]);
                wi++;
            }
        }

        // create feature map
        feature_map = new FeatureMap(feature_array, w);
    }
}
