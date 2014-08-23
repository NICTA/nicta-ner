/*
 * #%L
 * NICTA t3as NER CoNLL 2003 evaluation
 * %%
 * Copyright (C) 2014 NICTA
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
package org.t3as.ner.conll2003;

import com.google.common.collect.ImmutableCollection;
import com.google.common.collect.ImmutableList;
import com.google.common.primitives.Doubles;
import org.t3as.ner.EntityType;

public class NerClassification {

    public final String nerToken;
    public final EntityType type;
    public final int phraseStartIndex;
    public final ImmutableCollection<Double> scores;

    public NerClassification(final String nerToken, final EntityType type, final int phraseStartIndex,
                             final double[] scores) {
        this.nerToken = nerToken;
        this.type = type;
        this.phraseStartIndex = phraseStartIndex;
        this.scores = ImmutableList.copyOf(Doubles.asList(scores));
    }
}
