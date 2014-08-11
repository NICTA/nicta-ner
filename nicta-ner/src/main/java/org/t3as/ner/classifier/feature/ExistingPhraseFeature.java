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
public class ExistingPhraseFeature extends Feature {

    private final ImmutableCollection<String> PHRASES;

    public ExistingPhraseFeature(final String filename) throws IOException {
        super(filename);
        PHRASES = ImmutableSet.copyOf(IO.lines(getClass(), filename));
    }

    @Override
    public double score(final Phrase _p) {
        String phrase = "";
        for (int i = 0; i < _p.phrase.size(); i++) {
            phrase += _p.phrase.get(i).text + " ";
        }
        phrase = Strings.simplify(phrase);

        return PHRASES.contains(phrase) ? 1.0f : 0.0f;
    }
}
