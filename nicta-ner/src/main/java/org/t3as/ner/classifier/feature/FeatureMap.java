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

import org.t3as.ner.EntityType;
import org.t3as.ner.Phrase;

import javax.annotation.concurrent.Immutable;
import java.util.ArrayList;
import java.util.Collection;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

@Immutable
public class FeatureMap {

    private final Map<EntityType, List<Feature>> featureMap = new LinkedHashMap<>();
    private final boolean tracing;
    public Collection<String> trace;

    public FeatureMap(final boolean tracing) { this.tracing = tracing; }

    public void addFeature(final String entityType, final Feature feature) {
        final EntityType type = new EntityType(entityType);
        List<Feature> features = featureMap.get(type);
        if (features == null) {
            features = new ArrayList<>();
            featureMap.put(type, features);
        }
        features.add(feature);
    }

    public Map<EntityType, Double> score(final Phrase p) {
        final Map<EntityType, Double> scores = new LinkedHashMap<>();
        if (tracing) trace = new ArrayList<>();
        for (final Map.Entry<EntityType, List<Feature>> e : featureMap.entrySet()) {
            double score = 0;
            for (final Feature f : e.getValue()) {
                final double s = f.score(p);
                score += s;
                if (tracing && s != 0) {
                    trace.add("'" + p.phraseString() + "', w=" + e.getKey() + ":" + f.getWeight()
                              + ", s=" + s + ", " + f.ident());
                }
            }
            scores.put(e.getKey(), score);
        }
        return scores;
    }

    @Override
    public String toString() {
        return "FeatureMap{" +
               "featureMap=" + featureMap +
               '}';
    }

    @Override
    public boolean equals(final Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        final FeatureMap that = (FeatureMap) o;
        return featureMap.equals(that.featureMap);
    }

    @Override
    public int hashCode() { return featureMap.hashCode(); }
}
