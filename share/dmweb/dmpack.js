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
    const maxZoom = 19;
    const geoJson = { "type": "FeatureCollection", "features": features };

    const map = L.map(id, { attributionControl: false }).setView({lat: lat, lng: lon }, zoom);

    L.tileLayer(url, { maxZoom: maxZoom }).addTo(map);
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
    let popupContent = '';

    if (feature.properties && feature.properties.meta)
    {
        popupContent = feature.properties.meta;
    }

    layer.bindPopup(popupContent);
}

/**
 * Leaflet callback function.
 */
function pointToLayer(feature, latlng)
{
    return L.circleMarker(latlng, { radius: 2 });
}
