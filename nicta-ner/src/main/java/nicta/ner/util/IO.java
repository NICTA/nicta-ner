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
package nicta.ner.util;

import com.google.common.io.LineProcessor;

import javax.annotation.Nonnull;
import java.io.IOException;
import java.nio.charset.Charset;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import java.util.regex.Pattern;

import static com.google.common.io.Resources.getResource;
import static com.google.common.io.Resources.readLines;

public final class IO {

    private static final Pattern SPACES = Pattern.compile(" ");
    private static final Pattern TABS = Pattern.compile("\t");
    private static final Charset UTF8 = Charset.forName("UTF-8");

    private IO() {}

    /** Return a Set containing trimmed lines read from a file, skipping comments. */
    public static Set<String> lines(final Class<?> origin, final String resource) throws IOException {
        return new HashSet<String>() {{
            readResource(origin, resource, new NullReturnLineProcessor() {
                @Override
                public boolean processLine(@Nonnull final String line) {
                    final String l = line.trim();
                    // add to the containing HashSet we are currently in the init block of
                    if (!l.startsWith("#") && !l.isEmpty()) add(l);
                    return true;
                }
            });
        }};
    }

    /** Returns a Set containing only single words. */
    public static Set<String> createSingleWordSet(final Class<?> origin, final String resource,
                                                  final boolean eliminatePrepAndConj) throws IOException {
        return new HashSet<String>() {{
            readResource(origin, resource, new NullReturnLineProcessor() {
                @Override
                public boolean processLine(@Nonnull final String line) {
                    final String l = line.trim();
                    if (!l.startsWith("#") && !l.isEmpty()) {
                        for (final String part : SPACES.split(l)) {
                            if (!part.isEmpty()) {
                                if (eliminatePrepAndConj) {
                                    final String wordType = Dictionary.checkup(part);
                                    if (wordType != null && (wordType.startsWith("IN") || wordType.startsWith("CC"))) {
                                        continue;
                                    }
                                }
                                // add to the containing HashSet we are currently in the init block of
                                add(part);
                            }
                        }
                    }
                    return true;
                }
            });
        }};
    }

    /** Return a set containing all non-comment non-empty lower cased words. */
    public static Set<String> lowerCasedWordSet(final Class<?> origin, final String resource) throws IOException {
        return new HashSet<String>() {{
            readResource(origin, resource, new NullReturnLineProcessor() {
                @Override
                public boolean processLine(@Nonnull final String line) {
                    final String l = line.trim();
                    // add to the containing HashSet we are currently in the init block of
                    if (!l.startsWith("#") && !l.isEmpty()) add(l.toLowerCase());
                    return true;
                }
            });
        }};
    }

    /** Return a map/dictionary of words. */
    public static Map<String, String> dictionary(final Class<?> origin, final String resource) throws IOException {
        return new HashMap<String, String>() {{
            readResource(origin, resource, new NullReturnLineProcessor() {
                @Override
                public boolean processLine(@Nonnull final String line) {
                    final String l = line.trim();
                    if (!l.startsWith("#")) {
                        final String[] parts = TABS.split(l);
                        put(parts[0], parts[1]);
                    }
                    return true;
                }
            });
        }};
    }

    private static <T> T readResource(final Class<?> origin, final String resource,
                                      final LineProcessor<T> processor) throws IOException {
        try { return readLines(getResource(origin, resource), UTF8, processor); }
        catch (final IOException ioe) {
            throw new IOException("Error reading resource: '" + resource + "'", ioe);
        }

    }

    private static abstract class NullReturnLineProcessor implements LineProcessor<Object> {
        @SuppressWarnings("ReturnOfNull")
        @Override
        public Object getResult() { return null; }
    }
}
