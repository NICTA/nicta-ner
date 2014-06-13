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

import org.testng.annotations.BeforeClass;
import org.testng.annotations.DataProvider;
import org.testng.annotations.Test;

import java.util.HashMap;
import java.util.Map;

import static org.testng.Assert.assertEquals;

public class NamedEntityAnalyserTest {

    private NamedEntityAnalyser namedEntityAnalyser;

    @DataProvider(name = "testProcess")
    public static Object[][] primeNumbers() {
        return new Object[][]{
                {"John and Jane Doe Doe live in New Zealand in November.", new HashMap<String, String>() {{
                    // 0: John	PERSON	11.25, 40.0, -10.0	null	0:0:1:1
                    put("John", "PERSON");
                    // 2: Jane Doe Doe	PERSON	0.0, 60.0, 0.0	null	2:2:3:3
                    put("Jane Doe Doe", "PERSON");
                    // 7: New Zealand	LOCATION	95.0, 5.0, 0.0	in	7:7:2:2
                    put("New Zealand", "LOCATION");
                    // 10: November	DATE	0.0, 0.0, 0.0	in	10:10:1:1
                    put("November", "DATE");
                }}},

                {"John", new HashMap<String, String>() {{
                    // 0: John	PERSON	11.25, 40.0, -10.0	null	0:0:1:1
                    put("John", "PERSON");
                }}}
        };
    }

    @BeforeClass
    public void init() throws Exception {
        // TODO: because of how the NEA reads the config file in nicta.ner.resource.Configuration you can only do this
        // once per JVM!!!
        this.namedEntityAnalyser = new NamedEntityAnalyser();
    }

    @Test(dataProvider = "testProcess")
    public void testProcess(final String phrase, final Map<String, String> mappedResult) throws Exception {
        final NERResultSet result = namedEntityAnalyser.process(phrase);
        assertEquals(result.getMappedResult(), mappedResult);
        System.out.println(result);
        System.out.println(result.getMappedResult());
    }
}
