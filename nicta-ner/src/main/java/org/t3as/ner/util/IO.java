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
package org.t3as.ner.util;

import com.google.common.base.Splitter;
import com.google.common.collect.ImmutableMap;
import com.google.common.collect.ImmutableSet;
import com.google.common.io.LineProcessor;

import javax.annotation.Nonnull;
import java.io.IOException;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import static com.google.common.base.Charsets.UTF_8;
import static com.google.common.io.Resources.getResource;
import static com.google.common.io.Resources.readLines;
import static org.t3as.ner.util.Strings.clean;
import static org.t3as.ner.util.Strings.simplify;
import static org.t3as.ner.util.Strings.toEngLowerCase;

public final class IO {

    private static final Splitter SPACES = Splitter.on(' ').trimResults().omitEmptyStrings();
    private static final Splitter TABS = Splitter.on('\t').trimResults().omitEmptyStrings();

    private IO() {}

    /** Return a Set containing trimmed lowercaseLines read from a file, skipping comments. */
    public static Set<String> lowercaseLines(final Class<?> origin, final String resource) throws IOException {
        return ImmutableSet.copyOf(new HashSet<String>() {{
            readResource(origin, resource, new NullReturnLineProcessor() {
                @Override
                public boolean processLine(@Nonnull final String line) {
                    final String l = simplify(line);
                    // add to the containing HashSet we are currently in the init block of
                    if (!l.startsWith("#") && !l.isEmpty()) add(toEngLowerCase(l));
                    return true;
                }
            });
        }});
    }

    /** Return a Set containing lines with just the word characters of lines read from a file, skipping comments. */
    public static Set<String> cleanLowercaseLines(final Class<?> origin, final String resource) throws IOException {
        return ImmutableSet.copyOf(new HashSet<String>() {{
            readResource(origin, resource, new NullReturnLineProcessor() {
                @Override
                public boolean processLine(@Nonnull final String line) {
                    final String l = clean(simplify(line));
                    // add to the containing HashSet we are currently in the init block of
                    if (!l.startsWith("#") && !l.isEmpty()) add(toEngLowerCase(l));
                    return true;
                }
            });
        }});
    }

    /** Returns a Set containing only single words. */
    public static ImmutableSet<String> createLowercaseSingleWordSet(final Class<?> origin, final String resource,
                                                                    final boolean eliminatePrepAndConj) throws IOException {
        return ImmutableSet.copyOf(new HashSet<String>() {{
            readResource(origin, resource, new NullReturnLineProcessor() {
                @Override
                public boolean processLine(@Nonnull final String line) {
                    final String l = simplify(line);
                    if (!l.isEmpty() && !l.startsWith("#")) {
                        for (final String part : SPACES.split(l)) {
                            if (eliminatePrepAndConj) {
                                final String wordType = Dictionary.checkup(part);
                                if (wordType != null && (wordType.startsWith("IN") || wordType.startsWith("CC"))) {
                                    continue;
                                }
                            }
                            // add to the containing HashSet we are currently in the init block of
                            add(toEngLowerCase(part));
                        }
                    }
                    return true;
                }
            });
        }});
    }

    /** Return a set containing all non-comment non-empty words. */
    public static ImmutableSet<String> wordSet(final Class<?> origin, final String resource) throws IOException {
        return ImmutableSet.copyOf(new HashSet<String>() {{
            readResource(origin, resource, new NullReturnLineProcessor() {
                @Override
                public boolean processLine(@Nonnull final String line) {
                    final String l = simplify(line);
                    // add to the containing HashSet we are currently in the init block of
                    if (!l.isEmpty() && !l.startsWith("#")) add(l);
                    return true;
                }
            });
        }});
    }

    /** Return a map/dictionary of words from a file of key/values separated by a tab character. */
    public static ImmutableMap<String, String> dictionary(final Class<?> origin, final String resource)
            throws IOException {
        return ImmutableMap.copyOf(new HashMap<String, String>() {{
            readResource(origin, resource, new NullReturnLineProcessor() {
                @Override
                public boolean processLine(@Nonnull final String line) {
                    final String l = simplify(line);
                    if (!l.isEmpty() && !l.startsWith("#")) {
                        final List<String> parts = TABS.splitToList(l);
                        // add the parts to the map we are in the init block of
                        put(parts.get(0), parts.get(1));
                    }
                    return true;
                }
            });
        }});
    }

    private static <T> T readResource(final Class<?> origin, final String resource,
                                      final LineProcessor<T> processor) throws IOException {
        try { return readLines(getResource(origin, resource), UTF_8, processor); }
        catch (final IOException ioe) {
            throw new IOException("Error reading resource: '" + resource + "'", ioe);
        }
    }

    // the way we are using the LineProcessor does not depend on the result that it returns
    private static abstract class NullReturnLineProcessor implements LineProcessor<Object> {
        @SuppressWarnings("ReturnOfNull")
        @Override
        public Object getResult() { return null; }
    }
}
