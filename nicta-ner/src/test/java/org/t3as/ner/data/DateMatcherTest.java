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
package org.t3as.ner.data;

import org.testng.Assert;
import org.testng.annotations.DataProvider;
import org.testng.annotations.Test;

import java.io.IOException;

public class DateMatcherTest {

    @DataProvider(name = "dateStrings")
    public Object[][] configTestProvider() throws IOException {
        //noinspection HardcodedFileSeparator
        return new Object[][]{
                {"asdf 01/02/1995 asdf", true},
                {"31-12-2012", true},
                {"3 January 2012", true},
                {"40 January 2012", false},
                {"3 Undecimber 2012", false},

                {"asdf 12.31.2012 asdf", true},
                {"Jan 20 2012", true},

                {"asdf 1995-01-02 asdf", true},
                {"1979 Dec 02", true},
                {"1979 December 02", true},
                {"1995-50-50", false},

                {"Jan 2, 2008", true},

                {"2 Jan, 2008", true},
        };
    }

    @Test(dataProvider = "dateStrings")
    public void testIsDate(final String s, final boolean expected) {
        Assert.assertEquals(DateMatcher.isDate(s), expected, s);
    }
}
