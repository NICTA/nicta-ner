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

import javax.annotation.concurrent.Immutable;
import java.io.IOException;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import static org.t3as.ner.util.Strings.clean;
import static org.t3as.ner.util.Strings.simplify;
import static org.t3as.ner.util.Strings.toEngLowerCase;

@Immutable
public class RuledWordFeature extends Feature {

    private final ImmutableCollection<String> WORDS;

    public RuledWordFeature(final List<String> resources, final int weight) throws IOException {
        super(resources, weight);
        final Set<String> s = new HashSet<>();
        for (final String resource : resources) {
            s.addAll(IO.lowercaseWordSet(getClass(), resource, true));
        }
        WORDS = ImmutableSet.copyOf(s);
    }

    @SuppressWarnings("MagicNumber")
    @Override
    public double score(final Phrase p) {
        final int w = getWeight();
        if (w == 0) return 0;

        double score = 0.0;
        double weight = 0.75;    // weight increases 0.2 every word backward till the word "of" appears.
        for (int i = 0; i < p.phrase.size(); i++) {
            final String word = simplify(toEngLowerCase(clean(p.phrase.get(i).text)));
            if ("of".equalsIgnoreCase(word)) break;
            final double x = (WORDS.contains(word)) ? 1.0 : 0.0;
            score += weight * x;
            weight += 0.25;
        }
        return score * w;
    }
}
