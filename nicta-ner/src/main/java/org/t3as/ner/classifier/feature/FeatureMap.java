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

import org.t3as.ner.Phrase;

import javax.annotation.concurrent.Immutable;
import java.util.Arrays;
import java.util.List;

@Immutable
public class FeatureMap {

    private final Feature[] featureArray;

    public FeatureMap(final List<Feature> features) {
        featureArray = features.toArray(new Feature[features.size()]);
    }

    public double score(final Phrase p, final int wi) {
        double score = 0.0f;
        for (final Feature aFeatureArray : featureArray) {
            score += aFeatureArray.score(p, wi);
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
    public int hashCode() {
        return Arrays.hashCode(featureArray);
    }

    @Override
    public String toString() {
        return "FeatureMap{featureArray=" + Arrays.toString(featureArray) + '}';
    }
}
