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

import org.t3as.ner.NameType;
import org.t3as.ner.classifier.feature.Feature;
import org.t3as.ner.classifier.feature.FeatureMap;
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
                {"test-config" +
                 "",
                 new Result(new FeatureMap(
                         new ArrayList<Feature>() {{
                             add(generateFeatureByName("RuledWordFeature",          "PERSON_NAME",          new int[]{0,   20,   0}));
                             add(generateFeatureByName("RuledWordFeature",          "PERSON_KEYWORD",       new int[]{0,   30,   0}));
                             add(generateFeatureByName("RuledWordFeature",          "CITY_NAME",            new int[]{15,   0,   0}));
                             add(generateFeatureByName("RuledWordFeature",          "ORG_KEYWORD",          new int[]{0,    0,  25}));
                             add(generateFeatureByName("ExistingPhraseFeature",     "WIKI_ORG",             new int[]{0,  -10,  25}));
                             add(generateFeatureByName("ExistingPhraseFeature",     "WIKI_PERSON",          new int[]{0,   25, -10}));
                             add(generateFeatureByName("ExistingPhraseFeature",     "WIKI_PLACE",           new int[]{25, -10, -10}));
                             add(generateFeatureByName("RuledWordFeature",          "PLACE_KEYWORD",        new int[]{20,   0,   0}));
                             add(generateFeatureByName("ExistingPhraseFeature",     "STATES",               new int[]{10,   0,   0}));
                             add(generateFeatureByName("PrepositionContextFeature", "PREP_LOCATION",        new int[]{10,   0,   0}));
                             //add(generateFeatureByName("PrepositionContextFeature", "PREP_ORG",             new int[]{0,    0,  10}));
                             add(generateFeatureByName("RuledWordFeature",          "WIKI_ORG_EXTRACTION",  new int[]{0,    0,   5}));
                             add(generateFeatureByName("RuledWordFeature",          "WIKI_PER_EXTRACTION",  new int[]{0,    5,   0}));
                             add(generateFeatureByName("RuledWordFeature",          "WIKI_LOC_EXTRACTION",  new int[]{5,    0,   0}));
                         }}, null, false),
                            new ArrayList<NameType>() {{
                                add(NameType.LOCATION);
                                add(NameType.PERSON);
                                add(NameType.ORGANIZATION);
                            }}
                 )},
        };
    }

    @Test
    public void testDataProviders() throws IOException {
        configTestProvider();
    }

    @Test(dataProvider = "configTest")
    public void configTest(final String configResource, final Result r) throws IOException {
        final Configuration config = new Configuration(configResource, false);
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
