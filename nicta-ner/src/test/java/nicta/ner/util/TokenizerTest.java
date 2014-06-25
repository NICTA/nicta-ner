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
package nicta.ner.util;

import org.testng.Assert;
import org.testng.annotations.DataProvider;
import org.testng.annotations.Test;

import java.util.List;

import static com.google.common.collect.ImmutableList.of;

public class TokenizerTest {

    @DataProvider(name = "text")
    public Object[][] text() {
        return new Object[][]{
                {"A cat sat on the mat.",
                 of(of("A", "cat", "sat", "on", "the", "mat", "."))},

                {"a bee cee 12:00 dee",
                 of(of("a", "bee", "cee", "12:00", "dee"))},

                {"I am a rock.",
                 of(of("I", "am", "a", "rock", "."))},

                {"So am I.",
                 of(of("So", "am", "I."))},

                {"Punctuation! And? More! And^some more$ a@b.c :5 ab:cde foo: wibble",
                 of(of("Punctuation", "!"),
                    of("And", "?"),
                    of("More", "!"),
                    of("And", "^", "some", "more", "$", "a", "@", "b.c", ":", "5", "ab:cde", "foo", ":", "wibble"))},

                {"A can't won't don't",
                 of(of("A", "ca", "n't", "wo", "n't", "do", "n't"))},

                {"Dr Foo, Dr. Foo. Mr Bar, Miss Bar, Ms Bar.",
                 of(of("Dr", "Foo", ",", "Dr.", "Foo", "."),
                    of("Mr", "Bar", ",", "Miss", "Bar", ",", "Ms", "Bar", "."))},

                {"A day in Jan. Jan 2, 2008.",
                 of(of("A", "day", "in", "Jan.", "Jan", "2", ",", "2008", "."))},
        };
    }

    @Test(dataProvider = "text")
    public void testProcess(final String text, final List<List<String>> results) {
        final Tokenizer t = new Tokenizer(Tokenizer.Mode.WITH_PUNCTUATION);
        Assert.assertEquals(t.process(text), results, "Wrong result for the text '" + text + "'");
    }
}
