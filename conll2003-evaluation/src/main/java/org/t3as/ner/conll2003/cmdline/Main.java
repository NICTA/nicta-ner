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
package org.t3as.ner.conll2003.cmdline;

import com.beust.jcommander.JCommander;
import com.beust.jcommander.Parameter;
import org.t3as.ner.NamedEntityAnalyser;
import org.t3as.ner.NerResultSet;
import org.t3as.ner.conll2003.ConllReader;
import org.t3as.ner.conll2003.ConllToken;
import org.t3as.ner.conll2003.NerClassification;
import org.t3as.ner.conll2003.Sentence;
import org.t3as.ner.conll2003.Util;
import org.t3as.ner.resource.Configuration;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Map;

public final class Main {

    private Main() {}

    @SuppressWarnings("MethodNamesDifferingOnlyByCase")
    public static void main(final String[] args) throws IOException {
        final Options opts = getOptions(args);

        final NamedEntityAnalyser nea = new NamedEntityAnalyser(new Configuration());

        try (final ConllReader r = new ConllReader(opts.file.get(0))) {
            while (r.hasNext()) {
                System.out.println("-DOCSTART- -X- O O O");
                System.out.println();
                final Collection<Sentence> sentences = r.next();
                for (final Sentence conllSentence : sentences) {
                    final NerResultSet nerResultSet = nea.process(conllSentence.sentence);
                    final Map<Integer, NerClassification> phraseMap = Util.positionClassificationMap(nerResultSet);
                    for (int i = 0; i < conllSentence.tokens.size(); i++) {
                        final ConllToken conllToken = conllSentence.tokens.get(i);
                        final NerClassification nerClas = phraseMap.get(conllToken.startIndex);
                        // TODO: finding previous is broken
                        //final NerClassification previous = i == 0 ? null : phraseMap.get(i-1);
                        final String clas = Util.translateClassification(nerClas, null);
                        System.out.printf("%s %s %s\n", conllToken.token, conllToken.classifiers, clas);
                    }
                    System.out.println();
                }
            }
        }
    }

    @SuppressWarnings("CallToSystemExit")
    private static Options getOptions(final String[] args) {
        final Options opts = new Options();
        JCommander jc = null;
        try { jc = new JCommander(opts, args); }
        catch (final RuntimeException e) {
            System.err.println("Could not parse the options: " + e.getMessage());
            System.exit(1);
        }
        if (opts.showUsage) {
            jc.usage();
            System.exit(0);
        }
        if (opts.file.isEmpty() || !opts.file.get(0).canRead()) {
            System.out.println("Please pass a readable CoNLL 2003 test file.");
            jc.usage();
            System.exit(1);
        }
        return opts;
    }

    private static class Options {
        @Parameter(help = true, names = {"-h", "--help"}, description = "Show this help message.")
        boolean showUsage;

        // we are going to fail if there is anything else but 1 single file, so just call it file
        @Parameter(description = "<CoNLL 2003 test file>")
        List<File> file = new ArrayList<>();
    }
}
