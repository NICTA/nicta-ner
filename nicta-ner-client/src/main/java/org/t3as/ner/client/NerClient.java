/*
 * #%L
 * NICTA t3as Named-Entity Recognition client
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
package org.t3as.ner.client;

import org.glassfish.jersey.jackson.JacksonFeature;
import org.t3as.ner.NerResultSet;

import javax.ws.rs.client.ClientBuilder;
import javax.ws.rs.client.Entity;
import javax.ws.rs.client.WebTarget;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;
import java.io.UnsupportedEncodingException;
import java.net.URLEncoder;

import static javax.ws.rs.core.Response.Status.Family;

public class NerClient {

    public static final String DEFAULT_BASE_URL = "http://ner.t3as.org/nicta-ner-web/";
    private static final String NER_SERVICE_PATH = "rest/v1.0/ner";

    private final WebTarget target;

    public NerClient(final String url) {
        //final ClientConfig config = new DefaultClientConfig();
        //config.getClasses().add(JacksonJsonProvider.class);
        //config.getClasses().add(ObjectMapperProvider.class);
        target = ClientBuilder.newClient().register(new JacksonFeature()).target(url + NER_SERVICE_PATH);
    }

    public NerResultSet call(final String input) throws UnsupportedEncodingException {
        final String encodedInput = URLEncoder.encode(input, "UTF-8");
        final Response response = target.request(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON_TYPE)
                                        .post(Entity.entity(encodedInput, MediaType.APPLICATION_FORM_URLENCODED_TYPE));
        if (Family.SUCCESSFUL != response.getStatusInfo().getFamily()) {
            final Response.StatusType statusInfo = response.getStatusInfo();
            throw new RuntimeException("NER request failed with status " + statusInfo.getStatusCode()
                                       + ": " + statusInfo.getReasonPhrase());
        }
        return response.readEntity(NerResultSet.class);
    }
}
