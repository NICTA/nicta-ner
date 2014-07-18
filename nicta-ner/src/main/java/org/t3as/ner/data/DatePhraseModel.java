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
package org.t3as.ner.data;

import java.util.Collection;
import java.util.EnumSet;

import static org.t3as.ner.data.Date.DateType.DATE;
import static org.t3as.ner.data.Date.DateType.DATE_MM;
import static org.t3as.ner.data.Date.DateType.DATE_WEEKDAY;
import static org.t3as.ner.data.Date.DateType.DATE_YY;
import static org.t3as.ner.data.Date.DateType.TIME;

public class DatePhraseModel {

    // no idea why these were considered special in the original source
    private static final Collection<Date.DateType> SPECIAL = EnumSet.of(DATE_MM,
                                                                        DATE_YY,
                                                                        TIME,
                                                                        DATE,
                                                                        DATE_WEEKDAY);

    private final Collection<Date.DateType> contents = EnumSet.noneOf(Date.DateType.class);

    public void addType(final Date.DateType _type) { contents.add(_type); }

    public boolean isDate() {
        for (final Date.DateType t : contents) {
            if (SPECIAL.contains(t)) return true;
        }
        return false;
    }
}
