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
import com.google.common.collect.ImmutableList;
import org.t3as.ner.Phrase;

import javax.annotation.concurrent.Immutable;
import java.io.IOException;
import java.util.List;

/** This abstract class is a parent of features. */
@Immutable
public abstract class Feature {

    private final ImmutableList<String> resources;
    private final int weight;

    protected Feature(final List<String> resources, final int weight) {
        this.resources = ImmutableList.copyOf(resources);
        this.weight = weight;
    }

    /** Returns a score of the phrase according to the particular feature. */
    public abstract double score(final Phrase _p);

    /** Factory method create features by name. */
    public static Feature generateFeatureByName(final String featureType, final int weight, final List<String> resourceNames)
            throws IllegalArgumentException, IOException {
        switch (featureType) {
            case "RuledWordFeature":
                return new RuledWordFeature(resourceNames, weight);
            case "PrepositionContextFeature":
                return new PrepositionContextFeature(resourceNames, weight);
            case "ExistingPhraseFeature":
                return new ExistingPhraseFeature(resourceNames, weight);
            case "ExistingCleanPhraseFeature":
                return new ExistingCleanPhraseFeature(resourceNames, weight);
            case "CaseSensitiveWordLookup":
                return new CaseSensitiveWordLookup(resourceNames, weight);
            case "CaseInsensitiveWordLookup":
                return new CaseInsensitiveWordLookup(resourceNames, weight);
            default:
                throw new IllegalArgumentException("Unknown feature: '" + featureType + "'");
        }
    }

    public int getWeight() { return weight; }

    public ImmutableList<String> getResources() { return resources; }

    public abstract int getSize();

    @Override
    public String toString() {
        // useful toString helper as it will also show class name - so we don't need an override in the subclasses
        return Objects.toStringHelper(this)
                      .add("weight", weight)
                      .add("resources", resources)
                      .add("size", getSize())
                      .toString();
    }

    @Override
    public boolean equals(final Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        final Feature feature = (Feature) o;
        return weight == feature.weight && resources.equals(feature.resources);
    }

    @Override
    public int hashCode() {
        int result = resources.hashCode();
        result = 31 * result + weight;
        return result;
    }

    public String ident() { return getClass().getSimpleName() + resources.toString(); }

    public abstract void loadResources() throws IOException;
}
