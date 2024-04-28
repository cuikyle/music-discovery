import React from "react";
import {Box} from "@mui/material";
import {useState, useEffect} from "react";
import TextField from "@mui/material/TextField";
import Stack from "@mui/material/Stack";
import Autocomplete from "@mui/material/Autocomplete";
import "../../assets/application.css"
import {getArtistInfoData, getArtistsByName, getArtistTopTracksData, getRelatedArtists} from "../utils/utils";

// SearchBar component to display artist suggestions based on user input and then updating the tree + info tabs
function SearchBar({setArtistTreeData, setArtistInfoData, setArtistTopTracksData, setAboutArtist, setRootArtist}) {
    const [artists, setArtists] = useState([]);
    const [artistInput, setArtistInput] = useState("");
    const debouncedArtist = useDebounce(artistInput, 200) // Set a debounce on input to wait for users to finish typing

    const selectArtistHandler = (artist) => {
        setRootArtist(artist.id);
        setArtistInput(artist.name);
        setAboutArtist(true);
        getArtistInfoData(artist.id)
            .then((artistInfo) => setArtistInfoData(artistInfo));

        getArtistTopTracksData(artist.id)
            .then((artistTopTracksData) => setArtistTopTracksData(artistTopTracksData));

        getRelatedArtists(artist.id, [])
            .then((relatedArtists) => {
                artist.children = relatedArtists;
                setArtistTreeData(artist);
            });
    }

    useEffect(() => {
        if (!debouncedArtist) return;
        getArtistsByName(debouncedArtist)
            .then((artists) => setArtists(artists));
    }, [debouncedArtist]);


    return (
        <Stack sx={{width: 300, margin: "auto"}}>
            <Autocomplete
                id="searchbar"
                getOptionLabel={(jsonResults) => `${jsonResults.name}`}
                isOptionEqualToValue={(option, value) =>
                    option.name === value.name
                }
                noOptionsText={"Search for a new artist or ensure their name is correct"}
                options={artists}
                renderOption={(props, artist) => (
                    <Box component="li" {...props} key={artist.id} onClick={() => selectArtistHandler(artist)}>
                        <img src={artist.images} className={"searchbar-icon"}/>
                        {artist.name}
                    </Box>
                )}
                onInputChange={(event) => {
                    setArtistInput(event.target.value)
                }}

                renderInput={(params) => <TextField {...params} label="Search for an artist"/>}
            />
        </Stack>
    )
}

const useDebounce = (input, offset) => {
    const [debouncedInput, setDebouncedInput] = useState(input);

    useEffect(() => {
        const timeoutRef = setTimeout(() => {
            setDebouncedInput(input);
        }, offset);

        return () => {
            clearTimeout(timeoutRef)
        }
    }, [input])

    return debouncedInput;
}

export default SearchBar