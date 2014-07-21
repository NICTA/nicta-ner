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
package org.t3as.ner.classifier.feature;

import com.google.common.base.Objects;
import org.t3as.ner.Phrase;

import javax.annotation.concurrent.Immutable;
import java.io.IOException;

/** This abstract class is a parent of features. */
@Immutable
public abstract class Feature {

    private final String resource;

    protected Feature(final String resource) { this.resource = resource; }

    /** Returns a score of the phrase according to the particular feature. */
    public abstract double score(Phrase _p);

    /** Factory method create features by name. */
    public static Feature generateFeatureByName(final String feature, final String resource)
            throws IllegalArgumentException, IOException {
        final Feature f;
        switch (feature) {
            case "RuledWordFeature":
                f = new RuledWordFeature(resource);
                break;
            case "PrepositionContextFeature":
                f = new PrepositionContextFeature(resource);
                break;
            case "ExistingPhraseFeature":
                f = new ExistingPhraseFeature(resource);
                break;
            default:
                throw new IllegalArgumentException("Unknown feature: '" + feature + "'");
        }
        return f;
    }

    @Override
    public String toString() {
        // useful toString helper as it will also show class name - so we don't need an override in the subclasses
        return Objects.toStringHelper(this)
                      .add("resource", resource)
                      .toString();
    }

    @Override
    public boolean equals(final Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        final Feature feature = (Feature) o;
        return resource.equals(feature.resource);
    }

    @Override
    public int hashCode() {
        int result = getClass().hashCode();
        result = 31 * result + resource.hashCode();
        return result;
    }
}
