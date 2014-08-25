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
package org.t3as.ner;

import com.google.common.collect.ImmutableMap;
import org.t3as.ner.resource.Configuration;
import org.testng.annotations.BeforeClass;
import org.testng.annotations.DataProvider;
import org.testng.annotations.Test;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Set;

import static com.google.common.collect.ImmutableMap.of;
import static org.t3as.ner.EntityClass.DATE;
import static org.t3as.ner.EntityClass.UNKNOWN;
import static org.testng.Assert.assertEquals;
import static org.testng.Assert.assertNotNull;
import static org.testng.Assert.assertTrue;

public class NamedEntityAnalyserTest {

    static final EntityClass PER = new EntityClass("PERSON");
    static final EntityClass ORG = new EntityClass("ORGANIZATION");
    static final EntityClass LOC = new EntityClass("LOCATION");
    static final EntityClass ETH = new EntityClass("ETHNIC");

    private NamedEntityAnalyser namedEntityAnalyser;

    /**
     * NOTE: as can be seen in the results of this test there are lots of places where the NER can be improved!
     */
    @SuppressWarnings("MagicNumber")
    @DataProvider(name = "testProcess")
    public static Object[][] primeNumbers() throws IOException {
        return new Object[][]{
                {"John",
                 new ArrayList<Result>() {{
                     add(new Result("John", ORG, of(LOC, 6.0, PER, 7.5, ORG, 11.0, ETH, 0.0), none()));
                 }}},

                {"John and Jane Doe Doe live in New Zealand in November.",
                 new ArrayList<Result>() {{
                     add(new Result("John", ORG, of(LOC, 6.0, PER, 7.5, ORG, 11.0, ETH, 0.0), none()));
                     add(new Result("Jane Doe Doe", PER, of(LOC, 12.0, PER, 30.0, ORG, 15.0, ETH, 0.0), none()));
                     add(new Result("New Zealand", LOC, of(LOC, 47.0, PER, 8.75, ORG, 31.0, ETH, 15.0),
                                    of("prep", "in")));
                     add(new Result("November", DATE, ImmutableMap.<EntityClass, Double>of(), of("prep", "in")));
                 }}},

                {"Jim bought 300 shares of Acme Corp. in 2006.",
                 new ArrayList<Result>() {{
                     add(new Result("Jim", PER, of(LOC, 3.0, PER, 7.5, ORG, 7.0, ETH, 0.0), none()));
                     add(new Result("Acme Corp", ORG, of(LOC, 9.0, PER, 6.5, ORG, 14.0, ETH, 0.0), of("prep", "of")));
                     add(new Result("2006", DATE, ImmutableMap.<EntityClass, Double>of(), of("prep", "in")));
                 }}},

                {"Næsby is in Denmark, as is Næsbyholm Slot, which is outside the town of Glumsø.",
                 new ArrayList<Result>() {{
                     add(new Result("Næsby", ORG, of(LOC, 0.0, PER, 1.5, ORG, 4.0, ETH, 0.0), none()));
                     add(new Result("Denmark", LOC, of(LOC, 17.0, PER, 6.0, ORG, 7.0, ETH, 0.0), of("prep", "in")));
                     add(new Result("Næsbyholm Slot", UNKNOWN, of(LOC, 3.0, PER, 5.0, ORG, 5.0, ETH, 0.0), none()));
                     add(new Result("Glumsø", UNKNOWN, of(LOC, 0.0, PER, 0.0, ORG, 0.0, ETH, 0.0), of("prep", "of")));
                 }}},

                {new String(Files.readAllBytes(Paths.get("src/test/resources/test1.txt"))),
                 new ArrayList<Result>() {{
                     add(new Result("UK", LOC, of(LOC, 8.0, PER, 3.75, ORG, 7.0, ETH, 0.0), of("prep", "in")));
                     add(new Result("1965", DATE, ImmutableMap.<EntityClass, Double>of(), none()));
                     add(new Result("Eoghan", PER, of(LOC, 0.0, PER, 3.75, ORG, 2.0, ETH, 0.0), none()));
                     add(new Result("Ford Escort", ORG, of(LOC, 9.0, PER, 7.5, ORG, 11.0, ETH, 0.0), none()));
                     add(new Result("Toyota Camry", PER, of(LOC, 3.0, PER, 5.75, ORG, 4.0, ETH, 0.0), none()));
                     add(new Result("Feb", PER, of(LOC, 0.0, PER, 3.75, ORG, 0.0, ETH, 0.0), of("prep", "of")));
                     add(new Result("Tues", UNKNOWN, of(LOC, 0.0, PER, 0.0, ORG, 0.0, ETH, 0.0), of("prep", "on")));
                     add(new Result("H123ABC", UNKNOWN, of(LOC, 0.0, PER, 0.0, ORG, 0.0, ETH, 0.0), none()));
                     add(new Result("Department of Health", ORG, of(LOC, 15.0, PER, 1.5, ORG, 67.0, ETH, 0.0),
                                    of("prep", "for")));
                     add(new Result("Foreign", ORG, of(LOC, 3.0, PER, 1.5, ORG, 4.0, ETH, 0.0), of("prep", "for")));
                     add(new Result("Commonwealth Office", LOC, of(LOC, 15.0, PER, 8.0, ORG, 14.0, ETH, 0.0),
                                    of("prep", "for")));
                     add(new Result("China", LOC, of(LOC, 17.0, PER, 7.5, ORG, 7.0, ETH, 0.0), of("prep", "from")));
                     add(new Result("America", LOC, of(LOC, 9.0, PER, 3.75, ORG, 7.0, ETH, 0.0), of("prep", "from")));
                     add(new Result("Australia", LOC, of(LOC, 14.0, PER, 3.75, ORG, 7.0, ETH, 0.0),
                                    of("prep", "from")));
                 }}},

                {"Apple (Apple Inc.) is a company with the stock symbol AAPL.",
                 new ArrayList<Result>() {{
                     add(new Result("Apple", PER, of(LOC, 6.0, PER, 7.5, ORG, 7.0, ETH, 0.0), none()));
                     add(new Result("Apple Inc", ORG, of(LOC, 9.0, PER, 12.5, ORG, 34.0, ETH, 0.0), none()));
                     add(new Result("AAPL", ORG, of(LOC, 0.0, PER, 0.0, ORG, 15.0, ETH, 0.0), none()));
                 }}},

                {new String(Files.readAllBytes(Paths.get("src/test/resources/date1.txt"))),
                 new ArrayList<Result>() {{
                     //On the 1st of December, 2014.
                     add(new Result("1st of December , 2014", DATE, ImmutableMap.<EntityClass, Double>of(), none()));
                     //When it is December 7th.
                     add(new Result("December 7th", DATE, ImmutableMap.<EntityClass, Double>of(), none()));
                     //Sometime in February.
                     add(new Result("February", DATE, ImmutableMap.<EntityClass, Double>of(), of("prep", "in")));
                     //It is now 2014.
                     add(new Result("2014", DATE, ImmutableMap.<EntityClass, Double>of(), none()));
                     //Some date 2014-05-21.
                     add(new Result("2014", DATE, ImmutableMap.<EntityClass, Double>of(), none()));
                     //It happened in 200 BC.
                     add(new Result("BC", LOC, of(LOC, 8.0, PER, 3.75, ORG, 4.0, ETH, 0.0), none()));
                     //Around 2am, then at 4pm, and also 17:00.
                     add(new Result("17:00", DATE, ImmutableMap.<EntityClass, Double>of(), none()));
                 }}},

                {"John Smith, John.",
                 new ArrayList<Result>() {{
                     add(new Result("John Smith", PER, of(LOC, 12.0, PER, 39.5, ORG, 18.0, ETH, 0.0), none()));
                     add(new Result("John", ORG, of(LOC, 6.0, PER, 7.5, ORG, 11.0, ETH, 0.0), none()));
                 }}},

                // TODO: add these tests
                // BC , British Columbia - this conflicts with 'years BC'...
                // TX , Texas
                // should AM/PM conflict?
                // other locations
        };
    }

    @BeforeClass
    public void init() throws IOException, InterruptedException {
        this.namedEntityAnalyser = new NamedEntityAnalyser(new Configuration());
    }

    @Test
    public void testDataProviders() throws IOException { primeNumbers(); }

    @Test(dataProvider = "testProcess")
    public void testProcess(final String phrase, final List<Result> resultList) {
        final NerResultSet result = namedEntityAnalyser.process(phrase);

        // check that we have the correctly matched phrases and types
        final Map<EntityClass, Set<String>> mappedResult = result.getMappedResult();
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
        // first flatten the List<List<Phrase>>
        final List<Phrase> flat = new ArrayList<>();
        for (final List<Phrase> l : result.phrases) {
            for (final Phrase p : l) {
                flat.add(p);
            }
        }
        for (int i = 0; i < flat.size(); i++) {
            final Phrase p = flat.get(i);
            final Result r = resultList.get(i);

            assertEquals(p.score, r.scores,
                         "Phrase '" + p.phraseString() + "', expected '" + r.scores
                         + "' but found '" + p.score + "'");

            assertEquals(p.attachedWordMap, r.attachedWordMap);
        }
    }

    private static Map<String, String> none() { return of(); }

    private static class Result {
        final String phrase;
        final EntityClass type;
        final Map<EntityClass, Double> scores;
        final Map<String, String> attachedWordMap;

        private Result(final String phrase, final EntityClass type, final Map<EntityClass, Double> scores,
                       final Map<String, String> attachedWordMap) {
            this.phrase = phrase;
            this.type = type;
            this.scores = scores;
            this.attachedWordMap = attachedWordMap;
        }

        @Override
        public String toString() {
            return "Result{" +
                   "phrase='" + phrase + '\'' +
                   ", type=" + type +
                   ", scores=" + scores +
                   ", attachedWordMap=" + attachedWordMap +
                   '}';
        }
    }
}
