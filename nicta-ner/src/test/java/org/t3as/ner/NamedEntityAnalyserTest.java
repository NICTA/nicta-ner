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
package org.t3as.ner;

import com.google.common.collect.ImmutableMap;
import com.google.common.collect.ImmutableSet;
import com.google.common.primitives.Doubles;
import org.t3as.ner.data.NameType;
import org.t3as.ner.data.Phrase;
import org.t3as.ner.resource.Configuration;
import org.testng.annotations.BeforeClass;
import org.testng.annotations.DataProvider;
import org.testng.annotations.Test;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.Set;

import static com.google.common.collect.ImmutableList.of;
import static org.t3as.ner.data.NameType.DATE;
import static org.t3as.ner.data.NameType.LOCATION;
import static org.t3as.ner.data.NameType.ORGANIZATION;
import static org.t3as.ner.data.NameType.PERSON;
import static org.t3as.ner.data.NameType.UNKNOWN;
import static org.testng.Assert.assertEquals;
import static org.testng.Assert.assertNotNull;
import static org.testng.Assert.assertTrue;

public class NamedEntityAnalyserTest {

    private NamedEntityAnalyser namedEntityAnalyser;

    @SuppressWarnings("MagicNumber")
    @DataProvider(name = "testProcess")
    public static Object[][] primeNumbers() throws IOException {
        //noinspection HardcodedFileSeparator
        return new Object[][]{
                {"John",
                 new ArrayList<Result>() {{
                     // 0: John	PERSON	11.25, 40.0, -10.0	null	0:0:1:1
                     add(new Result("John", PERSON, of(11.25, 40., -10.), none()));
                 }}},

                {"John and Jane Doe Doe live in New Zealand in November.",
                 new ArrayList<Result>() {{
                     // 0: John	PERSON	11.25, 40.0, -10.0	null	0:0:1:1
                     add(new Result("John", PERSON, of(11.25, 40., -10.), none()));
                     // 2: Jane Doe Doe	PERSON	0.0, 60.0, 0.0	null	2:2:3:3
                     add(new Result("Jane Doe Doe", PERSON, of(0., 60., 0.), none()));
                     // 7: New Zealand	LOCATION	95.0, 5.0, 0.0	in	7:7:2:2
                     add(new Result("New Zealand", LOCATION, of(95., 5., 0.), ImmutableMap.of("prep", "in")));
                     // 10: November	DATE	0.0, 0.0, 0.0	in	10:10:1:1
                     add(new Result("November", DATE, of(0., 0., 0.), ImmutableMap.of("prep", "in")));
                 }}},

                {"Jim bought 300 shares of Acme Corp. in 2006.",
                 new ArrayList<Result>() {{
                     // 0: Jim	PERSON	0.0, 40.0, -10.0	null	0:0:1:1
                     add(new Result("Jim", PERSON, of(0., 40., -10.), none()));
                     // 5: Acme Corp	PERSON	0.0, 20.0, 0.0	of	5:5:2:2
                     add(new Result("Acme Corp", PERSON, of(0., 20., 0.), ImmutableMap.of("prep", "of")));
                     // 1: 2006	DATE	0.0, 0.0, 0.0	in	1:1:1:1
                     add(new Result("2006", DATE, of(0., 0., 0.), ImmutableMap.of("prep", "in")));
                 }}},

                {"Næsby is in Denmark, as is Næsbyholm Slot, which is outside the town of Glumsø.",
                 new ArrayList<Result>() {{
                     // 0: Næsby	ORGANIZATION	0.0, -7.5, 7.5	null	0:0:1:1
                     add(new Result("Næsby", ORGANIZATION, of(0., -7.5, 7.5), none()));
                     // 3: Denmark	LOCATION	46.25, -5.0, 25.0	in	3:3:1:1
                     add(new Result("Denmark", LOCATION, of(46.25, -5., 25.), ImmutableMap.of("prep", "in")));
                     // 7: Næsbyholm Slot UNKNOWN 0.0, 0.0, 0.0	null	7:7:2:2
                     add(new Result("Næsbyholm Slot", UNKNOWN, of(0., 0., 0.), none()));
                     // 16: Glumsø	UNKNOWN	0.0, 0.0, 0.0	of	16:16:1:1
                     add(new Result("Glumsø", UNKNOWN, of(0., 0., 0.), ImmutableMap.of("prep", "of")));
                 }}},

                {new String(Files.readAllBytes(Paths.get("src/test/resources/test1.txt"))),
                 new ArrayList<Result>() {{
                     // 4: UK	LOCATION	21.25, 0.0, 10.0	in	4:5:1:2
                     add(new Result("UK", LOCATION, of(21.25, 0., 10.), ImmutableMap.of("prep", "in")));
                     // 13: 1965	DATE	0.0, 0.0, 0.0	null	13:13:1:1
                     add(new Result("1965", DATE, of(0., 0., 0.), none()));
                     // 2: Eoghan	PERSON	0.0, 7.5, -7.5	null	2:2:1:1
                     add(new Result("Eoghan", PERSON, of(0., 7.5, -7.5), none()));
                     // 6: Ford Escort	PERSON	11.25, 15.0, 0.0	null	6:7:2:3
                     add(new Result("Ford Escort", PERSON, of(11.25, 15., 0.), none()));
                     // 3: Toyota Camry	PERSON	0.0, 35.0, -20.0	null	3:4:2:3
                     add(new Result("Toyota Camry", PERSON, of(0., 35., -20.), none()));
                     // 13: Feb	UNKNOWN	0.0, 0.0, 0.0	of	13:13:1:1
                     add(new Result("Feb", UNKNOWN, of(0., 0., 0.), ImmutableMap.of("prep", "of")));
                     // 16: Tues	UNKNOWN	0.0, 0.0, 0.0	on	16:17:1:2
                     add(new Result("Tues", UNKNOWN, of(0., 0., 0.), ImmutableMap.of("prep", "on")));
                     // 10: H123ABC	UNKNOWN	0.0, 0.0, 0.0	null	10:10:1:1
                     add(new Result("H123ABC", UNKNOWN, of(0., 0., 0.), none()));
                     // 5: Department of Health	ORGANIZATION	0.0, 0.0, 18.75	for	5:6:3:4
                     add(new Result("Department of Health", ORGANIZATION, of(0., 0., 18.75),
                                    ImmutableMap.of("prep", "for")));
                     // 19: Foreign	UNKNOWN	0.0, 0.0, 0.0	for	19:20:1:2
                     add(new Result("Foreign", UNKNOWN, of(0., 0., 0.), ImmutableMap.of("prep", "for")));
                     // 22: Commonwealth Office	UNKNOWN	0.0, 0.0, 0.0	for	22:22:2:2
                     add(new Result("Commonwealth Office", UNKNOWN, of(0., 0., 0.), ImmutableMap.of("prep", "for")));
                     //4: China	LOCATION	36.25, 30.0, -20.0	from	4:4:1:1
                     add(new Result("China", LOCATION, of(36.25, 30.0, -20.0), ImmutableMap.of("prep", "from")));
                     //6: America	LOCATION	36.25, 30.0, -20.0	from	6:6:1:1
                     add(new Result("America", LOCATION, of(36.25, 30.0, -20.0), ImmutableMap.of("prep", "from")));
                     //8: Australia	LOCATION	43.75, -20.0, 15.0	from	8:8:1:1
                     add(new Result("Australia", LOCATION, of(43.75, -20.0, 15.0), ImmutableMap.of("prep", "from")));
                 }}},

                {"Apple (Apple Inc.) is a company with the stock symbol AAPL.",
                 new ArrayList<Result>() {{
                     // 0: Apple	PERSON	0.0, 15.0, 0.0	null	0:0:1:1
                     add(new Result("Apple", PERSON, of(0., 15., 0.0), none()));
                     // 2: Apple Inc	PERSON	0.0, 15.0, 0.0	null	2:2:2:2
                     add(new Result("Apple Inc", PERSON, of(0., 15., 0.), none()));
                     // 8: AAPL	UNKNOWN	0.0, 0.0, 0.0	null	8:8:1:1
                     add(new Result("AAPL", UNKNOWN, of(0., 0., 0.), none()));
                 }}},

                {new String(Files.readAllBytes(Paths.get("src/test/resources/date1.txt"))),
                 new ArrayList<Result>() {{
                     //On the 1st of December, 2014.
                     //2: 1st of December , 2014	DATE	0.0, 0.0, 0.0	null	2:2:5:5
                     add(new Result("1st of December , 2014", DATE, of(0.0, 0.0, 0.0), none()));

                     //When it is December 7th.
                     //3: December 7th	DATE	0.0, 0.0, 0.0	null	3:3:2:2
                     add(new Result("December 7th", DATE, of(0.0, 0.0, 0.0), none()));

                     //Sometime in February.
                     //2: February	DATE	0.0, 0.0, 0.0	in	2:2:1:1
                     add(new Result("February", DATE, of(0.0, 0.0, 0.0), ImmutableMap.of("prep", "in")));

                     //It is now 2014.
                     //3: 2014	DATE	0.0, 0.0, 0.0	null	3:3:1:1
                     add(new Result("2014", DATE, of(0.0, 0.0, 0.0), none()));

                     //Some date 2014-05-21.
                     //2: 2014	DATE	0.0, 0.0, 0.0	null	2:2:1:1
                     add(new Result("2014", DATE, of(0.0, 0.0, 0.0), none()));

                     //It happened in 200 BC.
                     //4: BC	UNKNOWN	0.0, 0.0, 0.0	null	4:4:1:1
                     add(new Result("BC", UNKNOWN, of(0.0, 0.0, 0.0), none()));

                     //Around 2am, then at 4pm, and also 17:00.
                     //9: 17:00	DATE	0.0, 0.0, 0.0	null	9:9:1:1
                     add(new Result("17:00", DATE, of(0.0, 0.0, 0.0), none()));
                 }}},

                {"John Smith, John.",
                 new ArrayList<Result>() {{
                     //0: John Smith	PERSON	26.25, 60.0, -10.0	null	0:0:2:2
                     add(new Result("John Smith", PERSON, of(26.25, 60.0, -10.0), none()));
                     //3: John	PERSON	11.25, 40.0, -10.0	null	3:3:1:1
                     add(new Result("John", PERSON, of(11.25, 40.0, -10.0), none()));
                 }}},

                // TODO: add these tests
                // BC = British Columbia - this conflicts with 'years BC'...
                // TX = Texas
                // should AM/PM conflict?
                // other locations
        };
    }

    @BeforeClass
    public void init() throws IOException {
        this.namedEntityAnalyser = new NamedEntityAnalyser(new Configuration());
    }

    @Test
    public void doubleCreateNea() throws IOException {
        // check that we don't have any leaky static references
        final Configuration config = new Configuration();
        final NerResultSet result1 = new NamedEntityAnalyser(config).process("John");
        assertEquals(result1.getMappedResult().size(), 1);
        assertEquals(result1.getMappedResult().get(PERSON), ImmutableSet.of("John"));
        final NerResultSet result2 = new NamedEntityAnalyser(config).process("Gwen");
        assertEquals(result2.getMappedResult().size(), 1);
        assertEquals(result2.getMappedResult().get(PERSON), ImmutableSet.of("Gwen"));
    }

    @Test(dataProvider = "testProcess")
    public void testProcess(final String phrase, final List<Result> resultList) {
        final NerResultSet result = namedEntityAnalyser.process(phrase);

        // check that we have the correctly matched phrases and types
        final Map<NameType, Set<String>> mappedResult = result.getMappedResult();
        for (final Result r : resultList) {
            assertTrue(mappedResult.containsKey(r.type), "Could not find the phrase '" + r.phrase + "', type " + r.type
                                                         + ", in results map containing: " + mappedResult + ": ");
            // remove each result from the mappedResult
            final Set<String> phrases = mappedResult.get(r.type);
            assertNotNull(phrases.remove(r.phrase),
                          "Phrase '" + r.phrase + "' was not found in the set of values: " + phrases + ", ");

            if (phrases.isEmpty()) mappedResult.remove(r.type);
        }

        // all results should now have been removed from the results map
        assertTrue(mappedResult.isEmpty(), "Result map is not empty: " + mappedResult);

        // now match the other Phrase details
        for (int i = 0; i < result.phrases.get(0).size(); i++) {
            final Phrase p = result.phrases.get(0).get(i);
            final Result r = resultList.get(i);

            assertEquals(p.score, r.scores, "Phrase '" + p.phraseString() + "', expected '" + Arrays.toString(r.scores)
                                            + "' but found '" + Arrays.toString(p.score) + "'");

            assertEquals(p.attachedWordMap, r.attachedWordMap);
        }
    }

    private static Map<String, String> none() { return ImmutableMap.of(); }

    private static class Result {
        final String phrase;
        final NameType type;
        final double[] scores;
        final Map<String, String> attachedWordMap;

        private Result(final String phrase, final NameType type, final Collection<Double> scores,
                       final Map<String, String> attachedWordMap) {
            this.phrase = phrase;
            this.type = type;
            this.scores = Doubles.toArray(scores);
            this.attachedWordMap = attachedWordMap;
        }
    }
}
