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

import org.t3as.ner.classifier.feature.FeatureMap;
import org.testng.annotations.DataProvider;
import org.testng.annotations.Test;

import java.io.IOException;

import static com.google.common.collect.ImmutableList.of;
import static org.t3as.ner.classifier.feature.Feature.generateFeatureByName;
import static org.testng.Assert.assertEquals;

public class ConfigurationTest {

    @DataProvider(name = "configTest")
    public Object[][] configTestProvider() throws IOException {
        final FeatureMap featureMap = new FeatureMap(false);
        featureMap.addFeature("LOCATION",     generateFeatureByName("ExistingPhraseFeature", 3,
                                                                    of("PREP_LOCATION")));
        featureMap.addFeature("LOCATION",     generateFeatureByName("CaseInsensitiveWordLookup", 3,
                                                                    of("ShortCountries.dbpedia")));
        featureMap.addFeature("LOCATION",     generateFeatureByName("CaseInsensitiveWordLookup", 10,
                                                                    of("StreetTypes.txt")));
        featureMap.addFeature("LOCATION",     generateFeatureByName("ExistingCleanPhraseFeature", 5,
                                                                    of("country_names.freebase")));
        featureMap.addFeature("LOCATION",     generateFeatureByName("CaseSensitiveWordLookup", 5,
                                                                    of("country_codes.freebase")));
        featureMap.addFeature("PERSON",       generateFeatureByName("RuledWordFeature", 2,
                                                                    of("PERSON_KEYWORD")));
        featureMap.addFeature("ORGANIZATION", generateFeatureByName("CaseInsensitiveWordLookup", 7,
                                                                    of("OrganisationKeywords.dbpedia")));
        featureMap.addFeature("ORGANIZATION", generateFeatureByName("PrepositionContextFeature", 3,
                                                                    of("PREP_ORG")));
        featureMap.addFeature("ETHNIC",       generateFeatureByName("CaseSensitiveWordLookup", 15,
                                                                    of("nationalities.txt", "ethnic_groups.txt")));
        return new Object[][]{{"test-config", new Result(featureMap)}};
    }

    @Test
    public void testDataProviders() throws IOException {
        configTestProvider();
    }

    @Test(dataProvider = "configTest")
    public void configTest(final String configResource, final Result r) throws IOException, InterruptedException {
        final Configuration config = new Configuration(this.getClass().getResourceAsStream(configResource), false);
        assertEquals(config.getFeatureMap(), r.featureMap);
    }

    private static class Result {
        private final FeatureMap featureMap;

        private Result(final FeatureMap featureMap) { this.featureMap = featureMap; }

        // necessary for when we are running the tests in IDEA
        @Override
        public String toString() { return super.toString(); }
    }
}
