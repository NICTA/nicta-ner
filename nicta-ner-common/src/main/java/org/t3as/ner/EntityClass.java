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

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;

/**
 * This isn't an enum because we want to keep it possible to specify arbitrary entity classes in the config.
 */
public class EntityClass {

    public static final EntityClass UNKNOWN = new EntityClass("UNKNOWN");
    public static final EntityClass DATE = new EntityClass("DATE");

    private final String entityClass;

    @JsonCreator
    public EntityClass(@JsonProperty("entityClass") final String entityClass) { this.entityClass = entityClass; }

    // required for JSON
    @SuppressWarnings("UnusedDeclaration")
    public String getEntityClass() { return entityClass; }

    @Override
    public String toString() { return entityClass; }

    @Override
    public boolean equals(final Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        final EntityClass that = (EntityClass) o;
        return entityClass.equals(that.entityClass);
    }

    @Override
    public int hashCode() { return entityClass.hashCode(); }
}
