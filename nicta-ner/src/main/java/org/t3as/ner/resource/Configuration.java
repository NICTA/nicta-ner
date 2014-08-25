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
package org.t3as.ner.resource;

import com.google.common.base.Objects;
import com.google.common.base.Splitter;
import org.t3as.ner.classifier.feature.Feature;
import org.t3as.ner.classifier.feature.FeatureMap;

import javax.annotation.concurrent.Immutable;
import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.List;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;
import java.util.regex.Pattern;

/** This class specifies the configurations. */
@Immutable
public class Configuration {

    public static final String DEFAULT_CONFIG_RESOURCE = "config";

    public final boolean tracing;
    private final FeatureMap featureMap;

    public Configuration() throws IOException, InterruptedException { this(false); }

    public Configuration(final boolean tracing) throws IOException, InterruptedException {
        this(Configuration.class.getResourceAsStream(DEFAULT_CONFIG_RESOURCE), tracing);
    }

    /** Constructor. Read in the config file. */
    public Configuration(final InputStream config, final boolean tracing) throws IOException, InterruptedException {
        this.tracing = tracing;
        final Pattern COLONS = Pattern.compile(":");
        final Splitter SPACES = Splitter.on(' ').trimResults().omitEmptyStrings();
        final ExecutorService exec = Executors.newFixedThreadPool(Runtime.getRuntime().availableProcessors());

        featureMap = new FeatureMap(tracing);

        try (final BufferedReader br = new BufferedReader(new InputStreamReader(config))) {

            for (String line; (line = br.readLine()) != null; ) {
                final String s = line.trim();
                if (s.startsWith("#")) continue;
                if (s.trim().isEmpty()) continue;

                final String[] parts = COLONS.split(s, 2);
                switch (parts[0]) {
                    case "Feature":
                        final List<String> l = SPACES.limit(4).splitToList(parts[1]);
                        final String featureType = l.get(0);
                        final String entityType = l.get(1);
                        final int weight = Integer.parseInt(l.get(2));
                        final List<String> resourceNames = SPACES.splitToList(l.get(3));
                        final Feature f = Feature.generateFeatureByName(featureType, weight, resourceNames);
                        featureMap.addFeature(entityType, f);
                        exec.execute(new FeatureInitialiserRunnable(f));
                        break;

                    default:
                        throw new IllegalArgumentException("Unexpected config keyword: '" + parts[0] + "'");
                }
            }
        }
        exec.shutdown();
        exec.awaitTermination(Long.MAX_VALUE, TimeUnit.DAYS);
    }

    public FeatureMap getFeatureMap() { return featureMap; }

    private static class FeatureInitialiserRunnable implements Runnable {

        final Feature f;

        FeatureInitialiserRunnable(final Feature f) { this.f = f; }

        @Override
        public void run() {
            final String s = Objects.toStringHelper(f)
                                    .add("weight", f.getWeight())
                                    .add("resources", f.getResources())
                                    .toString();
            try { f.loadResources(); }
            catch (final IOException e) { throw new RuntimeException("Could not load resources for feature: " + s, e); }
        }
    }
}
