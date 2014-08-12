/*
 * #%L
 * NICTA t3as Named-Entity Recognition library
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
package org.t3as.ner.resource;

import org.t3as.ner.classifier.feature.Feature;
import org.t3as.ner.classifier.feature.FeatureMap;
import org.t3as.ner.NameType;
import org.testng.annotations.DataProvider;
import org.testng.annotations.Test;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Collection;

import static org.t3as.ner.classifier.feature.Feature.generateFeatureByName;
import static org.testng.Assert.assertEquals;

public class ConfigurationTest {

    @DataProvider(name = "configTest")
    public Object[][] configTestProvider() throws IOException {
        //noinspection MagicNumber
        return new Object[][]{
                {Configuration.DEFAULT_CONFIG_RESOURCE,
                 new Result(new FeatureMap(
                         new ArrayList<Feature>() {{
                             add(generateFeatureByName("RuledWordFeature", "PERSON_NAME"));
                             add(generateFeatureByName("RuledWordFeature", "PERSON_KEYWORD"));
                             add(generateFeatureByName("RuledWordFeature", "COUNTRY_NAME"));
                             add(generateFeatureByName("RuledWordFeature", "CITY_NAME"));
                             add(generateFeatureByName("RuledWordFeature", "ORG_KEYWORD"));
                             add(generateFeatureByName("ExistingPhraseFeature", "WIKI_ORG"));
                             add(generateFeatureByName("ExistingPhraseFeature", "WIKI_PERSON"));
                             add(generateFeatureByName("ExistingPhraseFeature", "WIKI_PLACE"));
                             add(generateFeatureByName("RuledWordFeature", "PLACE_KEYWORD"));
                             add(generateFeatureByName("RuledWordFeature", "STATES"));
                             add(generateFeatureByName("PrepositionContextFeature", "PREP_LOCATION"));
                             add(generateFeatureByName("PrepositionContextFeature", "PREP_ORG"));
                             add(generateFeatureByName("RuledWordFeature", "WIKI_ORG_EXTRACTION"));
                             add(generateFeatureByName("RuledWordFeature", "WIKI_PER_EXTRACTION"));
                             add(generateFeatureByName("RuledWordFeature", "WIKI_LOC_EXTRACTION"));
                         }},
                         new double[][]{
                                 {0.0, 0.0, 15.0, 15.0, 0.0, 0.0, 0.0, 25.0, 20.0, 10.0, 10.0, 0.0, 0.0, 0.0, 5.0},
                                 {20.0, 30.0, 0.0, 0.0, 0.0, -10.0, 25.0, -10.0, 0.0, 0.0, 0.0, 0.0, 0.0, 5.0, 0.0},
                                 {0.0, 0.0, 0.0, 0.0, 25.0, 25.0, -10.0, -10.0, 0.0, 0.0, 0.0, 10.0, 5.0, 0.0, 0.0}}),
                            new ArrayList<NameType>() {{
                                add(NameType.LOCATION);
                                add(NameType.PERSON);
                                add(NameType.ORGANIZATION);
                            }}
                 )},
        };
    }

    @Test(dataProvider = "configTest")
    public void configTest(final String configResource, final Result r) throws IOException {
        final Configuration config = new Configuration(configResource);
        assertEquals(config.getFeatureMap(), r.featureMap);
        assertEquals(config.getNameTypes(), r.nameTypes);
    }

    private static class Result {
        private final FeatureMap featureMap;
        private final Collection<NameType> nameTypes;

        private Result(final FeatureMap featureMap, final Collection<NameType> nameTypes) {
            this.featureMap = featureMap;
            this.nameTypes = nameTypes;
        }
    }
}
