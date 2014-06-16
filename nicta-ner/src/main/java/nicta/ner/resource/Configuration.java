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
import java.util.Collections;
import java.util.List;

/**
 * This class specifies the configurations.
 */
public class Configuration {

    /** NameType array specifies the possible types of names. */
    private List<NameType> name_types = null;

    private FeatureMap feature_map = null;

    /**
     * Constructor. Read in the config file.
     */
    public Configuration() throws IOException {
        // name type texts
        final List<NameType> nameTypes = new ArrayList<>();
        // w texts
        String[] wTexts = null;
        // Feature array specifies the features used in name type recognition.
        final List<Feature> features = new ArrayList<>();

        try (final BufferedReader br = new BufferedReader(
                new InputStreamReader(this.getClass().getResourceAsStream("config")))) {

            // read each line from the file and put the information
            // in the temperate variables
            // need further process to extract the information
            for (String line; (line = br.readLine()) != null; ) {
                if (line.startsWith("#")) continue;
                if (line.trim().equals("")) continue;

                final String[] parts = line.split(":", 2);
                switch (parts[0]) {
                    case "Name Types":
                        final String[] types = parts[1].trim().split(" ");
                        for (final String s : types) {
                            nameTypes.add(new NameType(s));
                        }
                        break;

                    case "Feature":
                        final String[] c = parts[1].trim().split(" ");
                        if (c.length != 2) {
                            throw new IllegalArgumentException("Config File Syntax Error: '" + line + "'");
                        }
                        features.add(Feature.generateFeatureByName(c[0], c[1]));
                        break;

                    case "w":
                        wTexts = parts[1].trim().replace("| ", "").split(" ");
                        break;

                    default:
                        throw new IllegalArgumentException("Unexpected config keyword: '" + parts[0] + "'");
                }
            }
        }

        if (nameTypes.isEmpty() || features.isEmpty() || wTexts == null)
            throw new IllegalArgumentException("Config File Syntax Error, no Name Types, Features, or w params.");

        // if syntax error, throw exception.
        if (wTexts.length != features.size() * nameTypes.size())
            throw new IllegalArgumentException(
                    "Config File Syntax Error, number of w params do not equal (num Name Types * num Features)");

        /* w is the coefficient array. */
        final double[][] w = new double[nameTypes.size()][features.size()];

        // name_types information
        name_types = Collections.unmodifiableList(nameTypes);

        // weight array
        int wi = 0;
        for (int i = 0; i < name_types.size(); i++) {
            for (int j = 0; j < features.size(); j++) {
                w[i][j] = Double.parseDouble(wTexts[wi]);
                wi++;
            }
        }

        // create feature map
        feature_map = new FeatureMap(features, w);
    }

    public FeatureMap getFeatureMap() {
        return feature_map;
    }

    public List<NameType> getNameTypes() {
        return name_types;
    }
}
