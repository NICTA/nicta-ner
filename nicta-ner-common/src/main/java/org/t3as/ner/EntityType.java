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

/**
 * This isn't an enum because we want to keep it possible to specify arbitrary entity classes in the config.
 */
public class EntityType {

    public static final EntityType UNKNOWN = new EntityType("UNKNOWN");
    public static final EntityType DATE = new EntityType("DATE");

    private final String type;

    public EntityType(final String type) { this.type = type; }

    @Override
    public String toString() { return type; }

    @Override
    public boolean equals(final Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        final EntityType that = (EntityType) o;
        return type.equals(that.type);
    }

    @Override
    public int hashCode() { return type.hashCode(); }
}
