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
package nicta.ner;

import nicta.ner.classifier.NameClassifier;
import nicta.ner.extractor.NameExtractor;
import nicta.ner.resource.Configuration;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.Scanner;

/**
 * This is the main class of the Named Entity Recognition system.
 * <p/>
 * This system contains two parts: named entity extraction and named entity classification.
 */
public class NamedEntityAnalyser {

    private final NameExtractor extractor;
    private final NameClassifier classifier;

    /**
     * Constructor to create a NamedEntityAnalyser. This Analyser will extract and classify
     * the named entities in the input text.
     */
    public NamedEntityAnalyser(final Configuration config) {
        extractor = new NameExtractor(config);
        classifier = new NameClassifier(config);
    }

    /**
     * Process the text, and return a NERResultSet.
     * <p/>
     * See NERResultSet class.
     */
    public NERResultSet process(final String text) {
        // program pipeline: (Text) ------> (Phrases without classified) ------> (Phrases classified)
        //                         extractor                            classifier
        final NERResultSet rs = extractor.process(text);
        return classifier.process(rs);
    }

    /**
     * The main() method takes a file as input and output the
     * process result on the screen.
     */
    public static void main(final String[] args) throws IOException {
        final NamedEntityAnalyser nea = new NamedEntityAnalyser(new Configuration());
        if (args.length >= 1) {
            final String content = new String(Files.readAllBytes(Paths.get(args[0])));
            System.out.println(nea.process(content));
        }
        else {
            try (final Scanner in = new Scanner(System.in)) {
                while (true) {
                    System.out.println("Type in texts, -q for quit.");
                    System.out.print("> ");
                    final String processString = in.nextLine();
                    if ("-q".equalsIgnoreCase(processString)) break;
                    System.out.println(nea.process(processString).getMappedResult());
                }
            }
        }
    }
}
