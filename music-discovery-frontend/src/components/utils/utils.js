import SpotifyIcon from "../../assets/SpotifyIcon.jpg";

const apiBaseUrl = "https://musicdiscovery-haskell.kylecui.com/";

/**
 Collection of utils which are used across the app (such as searching a tree, API calls used across
 multiple locations, etc).
 **/

// Traverses a tree looking for a node (which all have unique values) that contains spotifyId's value
export function findArtistNode(node, spotifyId) {
    if (node.id === spotifyId) {
        return node;
    } else if (node.children != null) {
        let result = null;
        for (let i = 0; result == null && i < node.children.length; i++) {
            result = findArtistNode(node.children[i], spotifyId);
        }
        return result;
    }
    return null;
}

// Finds the highest res images otherwise returns a default Spotify icon
export function findHighestResImage(images) {
    if (images.length !== 0) {
        return images[0].url;
    } else {
        return SpotifyIcon;
    }
}

// Traverses a tree and gets a list of artistIds
export function getArtistIds(node) {
    const result = [];
    result.push(node.id);
    const children = node.children || [];
    children.forEach(child => {
        const childResult = getArtistIds(child);
        result.push(...childResult)
    })
    return result;
}

// Makes a call to the backend and returns a list of artists with similar names to artistName
// Images are set to the highest res image or set to default if the artist does not have an image
export const getArtistsByName = async (artistName) => {
    return fetch(apiBaseUrl + "music/artists/" + artistName)
        .then((response) => response.json())
        .then((res) => {

            for (let i = 0; i < res.artists.items.length; i++) {
                if (res.artists.items[i].images.length !== 0) {
                    res.artists.items[i].images = res.artists.items[i].images[0].url;
                } else {
                    res.artists.items[i].images = SpotifyIcon;
                }
            }
            return res.artists.items;
        })
}


// Makes a call to the backend and returns an artist's info from Spotify
// Images are set to the highest res image or set to default if the artist does not have an image
export const getArtistInfoData = async (artistId) => {
    return fetch(apiBaseUrl + "music/artists/info/" + artistId)
        .then((response) => response.json())
        .then((res) => {
            res.images = findHighestResImage(res.images)
            let followerDigits = res.followers.total.toString().length;
            res.followers.total = res.followers.total.toString()[0] + "0".repeat(followerDigits - 1)
            res.followers.total = Number(res.followers.total).toLocaleString() + "+";
            res.id = artistId
            return res;
        })
}

// Makes a call to the backend and returns an artist's top tracks from Spotify
// Images are set to the highest res image or set to default if the artist does not have an image
export const getArtistTopTracksData = async (artistId) => {
    return fetch(apiBaseUrl + "music/artists/toptracks/" + artistId)
        .then((response) => response.json())
        .then((res) => {
            let tracks = []
            for (let i = 0; i < res.tracks.length; i++) {
                if (tracks.length === 3) {
                    break;
                }

                let dupeName = false;
                for (let j = 0; j < tracks.length; j++) {
                    if (res.tracks[i].name === tracks[j].name) {
                        dupeName = true;
                    }
                }

                if (!dupeName) {
                    res.tracks[i].album.images = findHighestResImage(res.tracks[i].album.images)
                    res.tracks[i].href = res.tracks[i].href.replace("https://api.spotify.com/v1/tracks/", "https://open.spotify.com/track/");
                    tracks.push(res.tracks[i]);
                }
            }
            return {"tracks": tracks};
        })
}

// Makes a call to the backend and returns an artist's related info from Spotify
// Images are set to the highest res image or set to default if the artist does not have an image
export const getRelatedArtists = async (artistId, artistTreeIds) => {
    return fetch(apiBaseUrl + "music/artists/related/" + artistId)
        .then((response) => response.json())
        .then((res) => {
            let children = [];
            for (let i = 0; i < res.artists.length; i++) {
                if (children.length === 10) {
                    break;
                }

                if (!artistTreeIds.includes(res.artists[i].id)) {
                    res.artists[i].images = findHighestResImage(res.artists[i].images);
                    children.push(res.artists[i]);
                }
            }
            return children;
        })
}
