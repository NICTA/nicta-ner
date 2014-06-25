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
import org.testng.annotations.DataProvider;

import java.util.ArrayList;
import java.util.Collection;

import static nicta.ner.classifier.feature.Feature.generateFeatureByName;

public class ConfigurationTest {

    @DataProvider(name = "configTest")
    public Object[][] configTestProvider() throws Exception {
        //noinspection MagicNumber
        return new Object[][]{
                {new Result(new FeatureMap(
                         new Feature[]{
                             generateFeatureByName(new String[]{"RuledWordFeature", "PERSON_NAME"}),
                             generateFeatureByName(new String[]{"RuledWordFeature", "PERSON_KEYWORD"}),
                             generateFeatureByName(new String[]{"RuledWordFeature", "COUNTRY_NAME"}),
                             generateFeatureByName(new String[]{"RuledWordFeature", "CITY_NAME"}),
                             generateFeatureByName(new String[]{"RuledWordFeature", "ORG_KEYWORD"}),
                             generateFeatureByName(new String[]{"ExistingPhraseFeature", "WIKI_ORG"}),
                             generateFeatureByName(new String[]{"ExistingPhraseFeature", "WIKI_PERSON"}),
                             generateFeatureByName(new String[]{"ExistingPhraseFeature", "WIKI_PLACE"}),
                             generateFeatureByName(new String[]{"RuledWordFeature", "PLACE_KEYWORD"}),
                             generateFeatureByName(new String[]{"RuledWordFeature", "STATES"}),
                             generateFeatureByName(new String[]{"PrepositionContextFeature", "PREP_LOCATION"}),
                             generateFeatureByName(new String[]{"PrepositionContextFeature", "PREP_ORG"}),
                             generateFeatureByName(new String[]{"RuledWordFeature", "WIKI_ORG_EXTRACTION"}),
                             generateFeatureByName(new String[]{"RuledWordFeature", "WIKI_PER_EXTRACTION"}),
                             generateFeatureByName(new String[]{"RuledWordFeature", "WIKI_LOC_EXTRACTION"}),
                         },
                         new double[][]{
                                 {0.0, 0.0, 15.0, 15.0, 0.0,
                                  0.0, 0.0, 25.0, 20.0, 10.0,
                                  10.0, 0.0, 0.0, 0.0, 10.0},
                                 {20.0, 30.0, 0.0, 0.0, 0.0,
                                  -10.0, 25.0, -10.0, 0.0, 0.0,
                                  0.0, 0.0, -10.0, 10.0, -10.0},
                                 {0.0, 0.0, 0.0, 0.0, 25.0,
                                  25.0, -10.0, -10.0, 0.0, 0.0,
                                  0.0, 10.0, 10.0, -10.0, -10.0}}),
                            new ArrayList<NameType>() {{
                                add(new NameType("LOCATION"));
                                add(new NameType("PERSON"));
                                add(new NameType("ORGANIZATION"));
                            }}
                 )},
        };
    }

    // works in trunk: no point running this test at all
    //@Test(dataProvider = "configTest")
    //public void configTest(final Result r) throws IOException {
        /* works in trunk: original source can only create a single Configuration instance per life of the JVM
        final Configuration config = new Configuration();
        */
        /* works in trunk: FeatureMap and NameType graphs missing equals()
        assertEquals(config.feature_map, r.featureMap);
        assertEquals(Arrays.asList(config.name_type), r.nameTypes);
        */
    //}

    private static class Result {
        private final FeatureMap featureMap;
        private final Collection<NameType> nameTypes;

        private Result(final FeatureMap featureMap, final Collection<NameType> nameTypes) {
            this.featureMap = featureMap;
            this.nameTypes = nameTypes;
        }
    }
}
