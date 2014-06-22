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
package nicta.ner.data;

import java.util.HashSet;
import java.util.Set;

// TODO: looks like this should be turned into an EnumSet, and the types should be enums
public class DatePhraseModel {

    private final Set<Integer> contents = new HashSet<>();

    public void addType(final int _type) { contents.add(_type); }

    public boolean isDate() {
        return contents.contains(1) ||
               contents.contains(2) ||
               contents.contains(3) ||
               contents.contains(7) ||
               contents.contains(8);
    }
}
