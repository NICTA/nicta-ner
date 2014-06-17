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

import com.google.common.base.Objects;
import com.google.common.io.LineProcessor;
import com.google.common.io.Resources;
import nicta.ner.data.Phrase;
import nicta.ner.util.Dictionary;

import java.io.IOException;
import java.nio.charset.Charset;
import java.util.HashSet;
import java.util.Set;
import java.util.regex.Pattern;

/**
 * This abstract class is a parent of features.
 */
public abstract class Feature {

    private final String resource;

    protected Feature(final String resource) { this.resource = resource; }

    @Override
    public String toString() {
        return Objects.toStringHelper(this)
                      .add("resource", resource)
                      .toString();
    }

    /**
     * Returns a score of the phrase according to the particular feature.
     */
    public abstract double score(Phrase _p);

    /**
     * Factory method create features by name.
     */
    public static Feature generateFeatureByName(final String feature, final String resource)
            throws IllegalArgumentException, IOException {
        final Feature f;
        switch (feature) {
            case "RuledWordFeature":
                f = new RuledWordFeature(resource);
                break;
            case "PrepositionContextFeature":
                f = new PrepositionContextFeature(resource);
                break;
            case "ExistingPhraseFeature":
                f = new ExistingPhraseFeature(resource);
                break;
            default:
                throw new IllegalArgumentException("Unknown feature: '" + feature + "'");
        }
        return f;
    }

    /**
     * Return a HashSet contains phrases (multi-words).
     */

    protected static Set<String> createPhraseSet(final String resource) throws IOException {
        try {
            return Resources.readLines(Resources.getResource(Feature.class, resource),
                                       Charset.forName("UTF-8"), new LineReader());
        }
        catch (final IOException ioe) {
            throw new IOException("Error reading Feature file: " + resource, ioe);
        }
    }

    /**
     * Returns a HashSet contains only single words.
     */
    protected static Set<String> createSingleWordSet(final String resource, final boolean eliminatePrepAndConj)
            throws IOException {
        try {
            return Resources.readLines(Resources.getResource(Feature.class, resource),
                                       Charset.forName("UTF-8"), new WordSetReader(eliminatePrepAndConj));
        }
        catch (final IOException ioe) {
            throw new IOException("Error reading Feature file: " + resource, ioe);
        }
    }

    @Override
    public boolean equals(final Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        final Feature feature = (Feature) o;
        return resource.equals(feature.resource);
    }

    @Override
    public int hashCode() {
        return resource.hashCode();
    }

    private static class WordSetReader implements LineProcessor<Set<String>> {

        private static final Pattern PATTERN = Pattern.compile(" ");
        final Dictionary dict = Dictionary.getSharedDictionary();
        final boolean eliminatePrepAndConj;
        Set<String> s = new HashSet<>();

        WordSetReader(final boolean eliminatePrepAndConj) { this.eliminatePrepAndConj = eliminatePrepAndConj; }

        @Override
        @SuppressWarnings("NullableProblems")
        public boolean processLine(final String line) throws IOException {
            final String l = line.trim();
            if (!l.startsWith("#") && !l.isEmpty()) {
                for (final String part : PATTERN.split(l)) {
                    if (eliminatePrepAndConj) {
                        final String wordType = dict.checkup(part);
                        if (wordType != null && (wordType.startsWith("IN") || wordType.startsWith("CC"))) {
                            return true;
                        }
                    }
                    s.add(part);
                }
            }
            return true;
        }

        @Override
        public Set<String> getResult() { return s; }
    }

    private static class LineReader implements LineProcessor<Set<String>> {
        Set<String> s = new HashSet<>();

        @Override
        @SuppressWarnings("NullableProblems")
        public boolean processLine(final String line) throws IOException {
            final String l = line.trim();
            if (!l.startsWith("#") && !l.isEmpty()) s.add(l);
            return true;
        }

        @Override
        public Set<String> getResult() { return s; }
    }
}
