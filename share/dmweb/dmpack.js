/* jshint esversion: 6 */

/**
 * Creates map with Leaflet.
 *
 * @param {string} id - The element id of the map container.
 * @param {string} url - The URL of the tile server.
 * @param {number} lon - The longitude coordinate.
 * @param {number} lat - The latitude coordinate.
 * @param {number} zoom - The Leaflet zoom level.
 * @param {array} features - Array of GeoJSON features.
 */
function createMap(id, url, lon, lat, zoom, features)
{
    const options = { attributionControl: false };
    const view    = { lat: lat, lng: lon };
    const maxZoom = 19;
    const map     = L.map(id, options).setView(view, zoom);

    L.tileLayer(url, { maxZoom: maxZoom }).addTo(map);

    const geoJson = { "type": "FeatureCollection", "features": features };

    L.geoJson(geoJson, {
        pointToLayer,
        onEachFeature
    }).addTo(map);
}

/**
 * Leaflet callback function.
 */
function onEachFeature(feature, layer)
{
    let content = '';

    if (feature.properties && feature.properties.data)
    {
        if (feature.properties.data.name)
        {
            content += `<strong>${feature.properties.data.name}</strong><br>`;
        }

        if (feature.properties.data.meta)
        {
            content += feature.properties.data.meta;
        }
    }

    layer.bindPopup(content);
}

/**
 * Leaflet callback function.
 */
function pointToLayer(feature, latlng)
{
    let options = {
        radius: 4,
        fillColor: "black",
        color: "black",
        weight: 1,
        opacity: 1,
        fillOpacity: 0.8
    };

    switch (feature.properties.type)
    {
        case 'node':
            options.fillColor = "gold";
            break;
        case 'sensor':
            options.fillColor = "crimson";
            break;
        case 'target':
            options.fillColor = "seagreen";
            break;
    }

    return L.circleMarker(latlng, options);
}
