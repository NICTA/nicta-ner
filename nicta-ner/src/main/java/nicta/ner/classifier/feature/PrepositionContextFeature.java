/*
 * #%L
 * NICTA Named Entity Recogniser library
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
package nicta.ner.classifier.feature;

import nicta.ner.data.Phrase;
import nicta.ner.util.IO;

import java.io.IOException;
import java.util.Collection;
import java.util.Collections;

public class PrepositionContextFeature extends Feature {

    private final Collection<String> WORDS;

    public PrepositionContextFeature(final String filename) throws IOException {
        super(filename);
        WORDS = Collections.unmodifiableCollection(IO.createSingleWordSet(getClass(), filename, false));
    }

    @Override
    public double score(final Phrase _p) {
        return WORDS.contains(_p.attachedWordMap.get("prep")) ? 1.0f : 0.0f;
    }
}
