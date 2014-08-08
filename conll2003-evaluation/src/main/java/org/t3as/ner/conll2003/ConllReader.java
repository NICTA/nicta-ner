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

import java.io.BufferedReader;
import java.io.Closeable;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.regex.Pattern;

public class ConllReader implements Closeable {

    private static final Pattern SPACES = Pattern.compile(" ");

    private final BufferedReader r;
    private Collection<String> next;

    public ConllReader(final File testFile) throws IOException {
        r = new BufferedReader(new FileReader(testFile));
        // throw away first DOCSTART and empty line, so we are starting on a new section
        r.readLine();
        r.readLine();
    }

    public boolean hasNext() throws IOException {
        next = readDoc();
        return !next.isEmpty();
    }

    public Collection<String> next() {
        return next;
    }

    @Override
    public void close() throws IOException {
        r.close();
    }

    /*
        -DOCSTART- -X- O O

        CRICKET NNP I-NP O
        - : O O
        LEICESTERSHIRE NNP I-NP I-ORG
        TAKE NNP I-NP O
        OVER IN I-PP O
        AT NNP I-NP O
        TOP NNP I-NP O
        AFTER NNP I-NP O
        INNINGS NNP I-NP O
        VICTORY NN I-NP O
        . . O O

        LONDON NNP I-NP I-LOC
        1996-08-30 CD I-NP O

        West NNP I-NP I-MISC
        [...]
        . . O O

        -DOCSTART- -X- O O

        CRICKET NNP I-NP O
        [...]
     */
    // TODO: need to save the existing type at least (and may as well save the entire rest of the line), so have to change the datastructure
    private Collection<String> readDoc() throws IOException {
        final Collection<String> sentences = new ArrayList<>();
        StringBuilder sentence = new StringBuilder();
        for (String line; (line = r.readLine()) != null; ) {
            // empty line means end-of-sentence
            if (line.isEmpty()) {
                // if we have an empty line and something in the actual sentence then add it
                if (sentence.length() > 0) {
                    sentences.add(sentence.toString());
                    sentence = new StringBuilder();
                }
            }
            else {
                final String[] parts = SPACES.split(line, 2);
                switch (parts[0]) {
                    case "-DOCSTART-":
                        // we use DOCSTART as the end of the current doc, also throw away the following empty line
                        r.readLine();
                        // no need to think about a current sentence, the previous empty line will have saved it
                        return sentences;

                    default:
                        if (sentence.length() > 0) sentence.append(" ");
                        sentence.append(parts[0]);
                }
            }
        }

        // if we run out of data in the file
        if (sentence.length() > 0) sentences.add(sentence.toString());
        return sentences;
    }
}
