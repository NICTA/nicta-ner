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
import nicta.ner.resource.Configuration;
import org.testng.annotations.BeforeClass;
import org.testng.annotations.DataProvider;
import org.testng.annotations.Test;

import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.Arrays;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.Map;

import static org.testng.Assert.assertEquals;
import static org.testng.Assert.assertTrue;

public class NamedEntityAnalyserTest {

    private NamedEntityAnalyser namedEntityAnalyser;

    @DataProvider(name = "testProcess")
    public static Object[][] primeNumbers() throws Exception {
        return new Object[][]{
                {"John",
                 new LinkedHashMap<String, Result>() {{
                     // 0: John	PERSON	11.25, 40.0, -10.0	null	0:0:1:1
                     put("John", new Result("PERSON", 11.25, 40, -10));
                 }}},

                {"John and Jane Doe Doe live in New Zealand in November.",
                 new LinkedHashMap<String, Result>() {{
                     // 0: John	PERSON	11.25, 40.0, -10.0	null	0:0:1:1
                     put("John", new Result("PERSON", 11.25, 40, -10));
                     // 2: Jane Doe Doe	PERSON	0.0, 60.0, 0.0	null	2:2:3:3
                     put("Jane Doe Doe", new Result("PERSON", 0, 60, 0));
                     // 7: New Zealand	LOCATION	95.0, 5.0, 0.0	in	7:7:2:2
                     put("New Zealand", new Result("LOCATION", 95, 5, 0));
                     // 10: November	DATE	0.0, 0.0, 0.0	in	10:10:1:1
                     put("November", new Result("DATE", 0, 0, 0));
                 }}},

                {"Jim bought 300 shares of Acme Corp. in 2006.",
                 new LinkedHashMap<String, Result>() {{
                     // 0: Jim	PERSON	0.0, 40.0, -10.0	null	0:0:1:1
                     put("Jim", new Result("PERSON", 0, 40, -10));
                     // 5: Acme Corp	PERSON	0.0, 20.0, 0.0	of	5:5:2:2
                     put("Acme Corp", new Result("PERSON", 0, 20, 0));
                     // 1: 2006	DATE	0.0, 0.0, 0.0	in	1:1:1:1
                     put("2006", new Result("DATE", 0, 0, 0));
                 }}},

                {"Næsby is in Denmark, as is Næsbyholm Slot, which is outside the town of Glumsø.",
                 new LinkedHashMap<String, Result>() {{
                     // 0: Næsby	ORGANIZATION	0.0, -7.5, 7.5	null	0:0:1:1
                     put("Næsby", new Result("ORGANIZATION", 0, -7.5, 7.5));
                     // 3: Denmark	LOCATION	46.25, -5.0, 25.0	in	3:3:1:1
                     put("Denmark", new Result("LOCATION", 46.25, -5, 25));
                     // 7: Næsbyholm Slot UNKNOWN 0.0, 0.0, 0.0	null	7:7:2:2
                     put("Næsbyholm Slot", new Result("UNKNOWN", 0, 0, 0));
                     // 16: Glumsø	UNKNOWN	0.0, 0.0, 0.0	of	16:16:1:1
                     put("Glumsø", new Result("UNKNOWN", 0, 0, 0));
                 }}},

                {new String(Files.readAllBytes(Paths.get("src/test/resources/test1.txt"))),
                 new LinkedHashMap<String, Result>() {{
                     // 4: UK	LOCATION	21.25, 0.0, 10.0	in	4:5:1:2
                     put("UK", new Result("LOCATION", 21.25, 0, 10));
                     // 13: 1965	DATE	0.0, 0.0, 0.0	null	13:13:1:1
                     put("1965", new Result("DATE", 0, 0, 0));
                     // 2: Eoghan	PERSON	0.0, 7.5, -7.5	null	2:2:1:1
                     put("Eoghan", new Result("PERSON", 0, 7.5, -7.5));
                     // 6: Ford Escort	PERSON	11.25, 15.0, 0.0	null	6:7:2:3
                     put("Ford Escort", new Result("PERSON", 11.25, 15, 0));
                     // 3: Toyota Camry	PERSON	0.0, 35.0, -20.0	null	3:4:2:3
                     put("Toyota Camry", new Result("PERSON", 0, 35, -20));
                     // 13: Feb	UNKNOWN	0.0, 0.0, 0.0	of	13:13:1:1
                     put("Feb", new Result("UNKNOWN", 0, 0, 0));
                     // 16: Tues	UNKNOWN	0.0, 0.0, 0.0	on	16:17:1:2
                     put("Tues", new Result("UNKNOWN", 0, 0, 0));
                     // 10: H123ABC	UNKNOWN	0.0, 0.0, 0.0	null	10:10:1:1
                     put("H123ABC", new Result("UNKNOWN", 0, 0, 0));
                     // 5: Department of Health	ORGANIZATION	0.0, 0.0, 18.75	for	5:6:3:4
                     put("Department of Health", new Result("ORGANIZATION", 0, 0, 18.75));
                     // 19: Foreign	UNKNOWN	0.0, 0.0, 0.0	for	19:20:1:2
                     put("Foreign", new Result("UNKNOWN", 0, 0, 0));
                     // 22: Commonwealth Office	UNKNOWN	0.0, 0.0, 0.0	for	22:22:2:2
                     put("Commonwealth Office", new Result("UNKNOWN", 0, 0, 0));
                 }}},

                {"Apple (Apple Inc.) is a company with the stock symbol AAPL.",
                 new LinkedHashMap<String, Result>() {{
                     // 0: Apple	PERSON	0.0, 15.0, 0.0	null	0:0:1:1
                     put("Apple", new Result("PERSON", 0, 15, 0.0));
                     // 2: Apple Inc	PERSON	0.0, 15.0, 0.0	null	2:2:2:2
                     put("Apple Inc", new Result("PERSON", 0, 15, 0));
                     // 8: AAPL	UNKNOWN	0.0, 0.0, 0.0	null	8:8:1:1
                     put("AAPL", new Result("UNKNOWN", 0, 0, 0));
                 }}},

                /*
                {"",
                new LinkedHashMap<String, Result>() {{
                     //
                     put("", new Result("", , , ));
                }}},
                */
        };
    }

    @BeforeClass
    public void init() throws Exception {
        this.namedEntityAnalyser = new NamedEntityAnalyser(new Configuration());
    }

    @Test
    public void doubleCreateNea() throws Exception {
        // check that we don't have any leaky static references
        final Configuration config = new Configuration();
        final NERResultSet result1 = new NamedEntityAnalyser(config).process("John");
        assertEquals(result1.getMappedResult().size(), 1);
        assertEquals(result1.getMappedResult().get("John"), "PERSON");
        final NERResultSet result2 = new NamedEntityAnalyser(config).process("Gwen");
        assertEquals(result2.getMappedResult().size(), 1);
        assertEquals(result2.getMappedResult().get("Gwen"), "PERSON");
    }

    @Test(dataProvider = "testProcess")
    public void testProcess(final String phrase, final Map<String, Result> resultMap) throws Exception {
        final NERResultSet result = namedEntityAnalyser.process(phrase);

        // check that we have the correctly matched phrases and types
        final Map<String, String> mappedResult = new HashMap<>(result.getMappedResult());
        for (final Map.Entry<String, Result> e : resultMap.entrySet()) {
            // remove each result from the mappedResult
            final String type = mappedResult.remove(e.getKey());
            assertEquals(type, e.getValue().type, "Entity '" + e.getKey() + "',");
        }
        // all results should now have been removed from the results map
        assertTrue(mappedResult.isEmpty());

        // now match the scores
        for (final Phrase p : result.phrases.get(0)) { // when might this be non-0?
            final Result r = resultMap.get(p.toString());// not good to depend on toString()...
            assertEquals(p.score, r.scores,
                         "Phrase '" + p + "', expected '" + Arrays.toString(p.score) + "' but found '" + Arrays
                                 .toString(r.scores) + "'");
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
