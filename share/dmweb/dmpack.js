/* jshint esversion: 6 */

"use strict";

/**
 * Creates map with Leaflet.
 *
 * @param {string} id - The element id of the map container.
 * @param {string} url - The URL of the tile server.
 * @param {number} lon - The longitude coordinate.
 * @param {number} lat - The latitude coordinate.
 * @param {number} zoom - The Leaflet zoom level.
 * @param {array} geoJson - GeoJSON feature collection.
 */
function createMap(id, url, lon, lat, zoom, geoJson)
{
    const options = { attributionControl: false };
    const view    = { lat: lat, lng: lon };
    const maxZoom = { maxZoom: 24 };

    const map = L.map(id, options).setView(view, zoom);

    L.tileLayer(url, maxZoom).addTo(map);

    L.geoJson(geoJson, {
        pointToLayer,
        onEachFeature
    }).addTo(map);
}

/**
 * HTML-encodes Unicode characters in string.
 */
function encode(str)
{
    return str.replace(/[\u00A0-\u9999<>\&]/gim, function(i) {
        return '&#' + i.charCodeAt(0) + ';';
    });
}

/**
 * Returns true if type is valid.
 */
function isValidType(type)
{
    return (type == 'node' || type == 'sensor' || type == 'target');
}

/**
 * Leaflet callback function.
 */
function onEachFeature(feature, layer)
{
    const base = '/dmpack';

    let content = '';

    if (feature.properties && feature.properties.type && feature.properties.data)
    {
        if (feature.properties.data.id && feature.properties.data.name)
        {
            let name;

            if (isValidType(feature.properties.type))
                name = `<a href="${base}/${feature.properties.type}?id=${feature.properties.data.id}">${feature.properties.data.name}</a>`;
            else
                name = `${feature.properties.data.name}`;

            content += `<strong>${name}</strong> `;
        }

        content += `<em>${feature.properties.type}</em><br>`;
        if (feature.properties.data.meta) content += encode(feature.properties.data.meta);
    }

    layer.bindPopup(content);
}

/**
 * Leaflet callback function.
 */
function pointToLayer(feature, latlng)
{
    const colors = {
        node: "gold",
        sensor: "crimson",
        target: "chartreuse"
    };

    let options = {
        radius: 4,
        fillColor: "black",
        color: "black",
        weight: 1,
        opacity: 1,
        fillOpacity: 0.8
    };

    if (feature.properties && feature.properties.type && colors.hasOwnProperty(feature.properties.type))
        options.fillColor = colors[feature.properties.type];

    return L.circleMarker(latlng, options);
}
