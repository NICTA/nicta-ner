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

import javax.annotation.concurrent.Immutable;

@Immutable
public class Token {

    public final int startIndex;
    public final String str;

    public Token(final int startIndex, final String str) {
        this.startIndex = startIndex;
        this.str = str;
    }

    @Override
    public boolean equals(final Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        final Token token = (Token) o;
        return startIndex == token.startIndex && str.equals(token.str);
    }

    @Override
    public int hashCode() {
        int result = startIndex;
        result = 31 * result + str.hashCode();
        return result;
    }

    public String toString() { return startIndex + ":" + str; }
}
