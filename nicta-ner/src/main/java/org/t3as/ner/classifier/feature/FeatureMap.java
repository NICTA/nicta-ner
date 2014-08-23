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

import com.google.common.collect.ImmutableList;
import org.t3as.ner.EntityType;
import org.t3as.ner.Phrase;

import javax.annotation.concurrent.Immutable;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;

@Immutable
public class FeatureMap {

    private final Feature[] featureArray;
    private final boolean tracing;
    private final ImmutableList<EntityType> entityTypes;
    public Collection<String> trace;

    public FeatureMap(final List<Feature> features, final ImmutableList<EntityType> entityTypes, final boolean tracing) {
        featureArray = features.toArray(new Feature[features.size()]);
        //noinspection AssignmentToCollectionOrArrayFieldFromParameter
        this.entityTypes = entityTypes;
        this.tracing = tracing;
    }

    public double score(final Phrase p, final int wi) {
        if (tracing) trace = new ArrayList<>();
        double score = 0.0f;
        for (final Feature feature : featureArray) {
            final double s = feature.score(p, wi);
            score += s;
            if (tracing && s != 0) {
                trace.add("'" + p.phraseString() + "', w=" + entityTypes.get(wi) + ":" + feature .getWeight(wi)
                          + ", s=" + s + ", " + feature.ident());
            }
        }
        return score;
    }

    @Override
    public boolean equals(final Object o) {
        if (this == o) return true;
        if (!(o instanceof FeatureMap)) return false;
        final FeatureMap that = (FeatureMap) o;
        return Arrays.equals(featureArray, that.featureArray);
    }

    @Override
    public int hashCode() { return Arrays.hashCode(featureArray); }

    @Override
    public String toString() { return "FeatureMap{featureArray=" + Arrays.toString(featureArray) + '}'; }
}
