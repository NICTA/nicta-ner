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

import org.t3as.ner.data.Phrase;

import javax.annotation.concurrent.Immutable;
import java.util.Arrays;
import java.util.List;

@Immutable
public class FeatureMap {

    private final Feature[] feature_array;
    private final double[][] w;

    public FeatureMap(final List<Feature> features, final double[][] _w) {
        feature_array = features.toArray(new Feature[features.size()]);
        // array deep copy
        w = new double[_w.length][];
        for (int i = 0; i < _w.length; i++) {
            w[i] = Arrays.copyOf(_w[i], _w[i].length);
        }
    }

    public double score(final Phrase _p, final int wi) {
        double score = 0.0f;
        for (int i = 0; i < feature_array.length; i++) {
            if (w[wi][i] == 0) continue;
            score += feature_array[i].score(_p) * w[wi][i];
        }
        return score;
    }

    @Override
    public boolean equals(final Object o) {
        if (this == o) return true;
        if (!(o instanceof FeatureMap)) return false;
        final FeatureMap that = (FeatureMap) o;
        return Arrays.equals(feature_array, that.feature_array) && Arrays.deepEquals(w, that.w);
    }

    @Override
    public int hashCode() {
        int result = Arrays.hashCode(feature_array);
        result = 31 * result + Arrays.deepHashCode(w);
        return result;
    }

    @Override
    public String toString() {
        return "FeatureMap{" +
               "feature_array=" + Arrays.toString(feature_array) +
               ", w=" + Arrays.deepToString(w) +
               '}';
    }
}
