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

import nicta.ner.data.Phrase;
import org.testng.annotations.BeforeClass;
import org.testng.annotations.DataProvider;
import org.testng.annotations.Test;

import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.Map;

import static org.testng.Assert.assertEquals;
import static org.testng.Assert.assertTrue;

public class NamedEntityAnalyserTest {

    private NamedEntityAnalyser namedEntityAnalyser;

    @DataProvider(name = "testProcess")
    public static Object[][] primeNumbers() {
        return new Object[][]{
                {"John", new LinkedHashMap<String, Result>() {{
                    // 0: John	PERSON	11.25, 40.0, -10.0	null	0:0:1:1
                    put("John", new Result("PERSON", 11.25, 40, -10));
                }}},

                {"John and Jane Doe Doe live in New Zealand in November.", new LinkedHashMap<String, Result>() {{
                    // 0: John	PERSON	11.25, 40.0, -10.0	null	0:0:1:1
                    put("John", new Result("PERSON", 11.25, 40, -10));
                    // 2: Jane Doe Doe	PERSON	0.0, 60.0, 0.0	null	2:2:3:3
                    put("Jane Doe Doe", new Result("PERSON", 0, 60, 0));
                    // 7: New Zealand	LOCATION	95.0, 5.0, 0.0	in	7:7:2:2
                    put("New Zealand", new Result("LOCATION", 95, 5, 0));
                    // 10: November	DATE	0.0, 0.0, 0.0	in	10:10:1:1
                    put("November", new Result("DATE", 0, 0, 0));
                }}},

                {"Jim bought 300 shares of Acme Corp. in 2006.", new LinkedHashMap<String, Result>() {{
                    //0: Jim	PERSON	0.0, 40.0, -10.0	null	0:0:1:1
                    put("Jim", new Result("PERSON", 0, 40, -10));
                    //5: Acme Corp	PERSON	0.0, 20.0, 0.0	of	5:5:2:2
                    put("Acme Corp", new Result("PERSON", 0, 20, 0)); // should probably be ORG or UNKNOWN
                    //1: 2006	DATE	0.0, 0.0, 0.0	in	1:1:1:1
                    put("2006", new Result("DATE", 0, 0, 0));
                }}},
        };
    }

    @BeforeClass
    public void init() throws Exception {
        // TODO: because of how the NEA reads the config file in nicta.ner.resource.Configuration you can only do this
        // once per JVM!!!
        this.namedEntityAnalyser = new NamedEntityAnalyser();
    }

    @Test(dataProvider = "testProcess")
    public void testProcess(final String phrase, final Map<String, Result> resultMap) throws Exception {
        final NERResultSet result = namedEntityAnalyser.process(phrase);

        // check that we have the correctly matched phrases and types
        final Map<String, String> mappedResult = new HashMap<>(result.getMappedResult());
        for (final Map.Entry<String, Result> e : resultMap.entrySet()) {
            // remove each result from the mappedResult
            final String type = mappedResult.remove(e.getKey());
            assertEquals(type, e.getValue().type);
        }
        // all results should now have been removed from the results map
        assertTrue(mappedResult.isEmpty());

        // now match the scores
        for (final Phrase p : result.phrases.get(0)) { // when might this be non-0?
            final Result r = resultMap.get(p.toString());// not good to depend on toString()...
            assertEquals(p.score, r.scores);
        }
    }

    private static class Result {
        final String type;
        final double[] scores;

        private Result(final String type, final double... scores) {
            this.type = type;
            this.scores = scores;
        }
    }
}
