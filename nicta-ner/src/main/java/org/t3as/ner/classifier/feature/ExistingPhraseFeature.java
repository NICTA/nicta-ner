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
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import static org.t3as.ner.util.Strings.toEngLowerCase;

@Immutable
public class ExistingPhraseFeature extends Feature {

    private final ImmutableCollection<String> PHRASES;

    public ExistingPhraseFeature(final List<String> resources, final int weight) throws IOException {
        super(resources, weight);
        final Set<String> s = new HashSet<>();
        for (final String resource : resources) {
            s.addAll(IO.lowercaseLines(getClass(), resource));
        }
        PHRASES = ImmutableSet.copyOf(s);
    }

    @Override
    public double score(final Phrase p) {
        final int w = getWeight();
        if (w == 0) return 0;

        final String phrase = Strings.simplify(p.phraseString());
        return PHRASES.contains(toEngLowerCase(phrase)) ? w : 0;
    }
}
