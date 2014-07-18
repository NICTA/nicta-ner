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
package org.t3as.ner.util;

import org.t3as.ner.data.Token;
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
                 of(of(t(0, "A"), t(2, "cat"), t(6, "sat"), t(10, "on"), t(13, "the"), t(17, "mat"), t(20, ".")))},

                {"a bee cee 12:00 dee",
                 of(of(t(0, "a"), t(2, "bee"), t(6, "cee"), t(10, "12:00"), t(16, "dee")))},

                {"I am a rock.",
                 of(of(t(0, "I"), t(2, "am"), t(5, "a"), t(7, "rock"), t(11, ".")))},

                {"So am I.",
                 of(of(t(0, "So"), t(3, "am"), t(6, "I.")))},

                {"Punctuation! And? More! And^some more$ a@b.c :5 ab:cde foo: wibble",
                 of(of(t(0, "Punctuation"), t(11, "!")),
                    of(t(13, "And"), t(16, "?")),
                    of(t(18, "More"), t(22, "!")),
                    of(t(24, "And"), t(27, "^"), t(28, "some"), t(33, "more"), t(37, "$"), t(39, "a"), t(40, "@"),
                       t(41, "b.c"), t(45, ":"), t(46, "5"), t(48, "ab:cde"), t(55, "foo"), t(58, ":"),
                       t(60, "wibble")))},

                {"A can't won't don't : foo",
                 of(of(t(0, "A"), t(2, "ca"), t(4, "n't"), t(8, "wo"), t(10, "n't"), t(14, "do"), t(16, "n't"),
                       t(20, ":"), t(22, "foo")))},


                {"Dr Foo, Dr. Foo. Mr Bar, Miss Bar, Ms Bar.",
                 of(of(t(0, "Dr"), t(3, "Foo"), t(6, ","), t(8, "Dr."), t(12, "Foo"), t(15, ".")),
                    of(t(17, "Mr"), t(20, "Bar"), t(23, ","), t(25, "Miss"), t(30, "Bar"), t(33, ","), t(35, "Ms"),
                       t(38, "Bar"), t(41, ".")))},

                {"A day in Jan. Jan 2, 2008.",
                 of(of(t(0, "A"), t(2, "day"), t(6, "in"), t(9, "Jan."), t(14, "Jan"), t(18, "2"), t(19, ","),
                       t(21, "2008"), t(25, ".")))},

                {"", of()},

                {":", of(of(t(0, ":")))},

                {"foo:", of(of(t(0, "foo"), t(3, ":")))},

                {"a: b", of(of(t(0, "a"), t(1, ":"), t(3, "b")))},
        };
    }

    @Test(dataProvider = "text")
    public void testProcess(final String text, final List<List<Token>> expected) {
        final Tokenizer t = new Tokenizer(Tokenizer.Mode.WITH_PUNCTUATION);
        Assert.assertEquals(t.process(text), expected, "Wrong expected results for the text '" + text + "'");
    }

    private static Token t(final int i, final String s) { return new Token(i, s); }
}
