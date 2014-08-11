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

import com.google.common.collect.ImmutableCollection;
import com.google.common.collect.ImmutableSet;
import org.t3as.ner.Phrase;
import org.t3as.ner.util.IO;
import org.t3as.ner.util.Strings;

import javax.annotation.concurrent.Immutable;
import java.io.IOException;

@Immutable
public class RuledWordFeature extends Feature {

    private final ImmutableCollection<String> WORDS;

    public RuledWordFeature(final String filename) throws IOException {
        super(filename);
        WORDS = ImmutableSet.copyOf(IO.createSingleWordSet(getClass(), filename, true));
    }

    @SuppressWarnings("MagicNumber")
    @Override
    public double score(final Phrase _p) {
        double score = 0.0f;
        double weight = 0.75f;    // weight increases 0.2 every word backward till the word "of" appears.
        for (int i = 0; i < _p.phrase.size(); i++) {
            final String word = Strings.simplify(_p.phrase.get(i).text);
            if ("of".equalsIgnoreCase(word)) break;
            final double x = (WORDS.contains(word)) ? 1.0 : 0.0;
            score += weight * x;
            weight += 0.25;
        }
        return score;
    }
}
