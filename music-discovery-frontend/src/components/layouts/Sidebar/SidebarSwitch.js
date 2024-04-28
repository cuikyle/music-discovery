import Stack from "@mui/material/Stack";
import {Switch, Typography} from "@mui/material";
import React from "react";

// Animated switch which displays information panels either on the app or the artist
export function SidebarSwitch({showAboutArtist, setAboutArtist}) {
    return (
        <Stack direction="row" component="label" alignItems="center" justifyContent="center" className={"header-margin"}>
            <Typography>
                About App
            </Typography>
            <Switch onChange={() => {setAboutArtist(!showAboutArtist)}}  checked={showAboutArtist}/>
            <Typography>
                About Artist
            </Typography>
        </Stack>
    );
}